module STM30.Repository

open System
open System.IO
open System.Collections.ObjectModel

open Nessos.FsPickler

module Path = 
    let rootFolder = "ProductionData"

    let root = 
        let x = Path.Combine(exepath, rootFolder)
        createDirectory x
        x

    let batch, year, month, day = datePath rootFolder

[<AutoOpen>]
 module private Helpers = 
    let binarySerializer = FsPickler.CreateBinarySerializer()


    type File = 
        | FilePath of string 

        static member doWith<'a> (FilePath path) (x:FileMode) (f : FileStream -> 'a) = 
            try
                use file = new FileStream( path, x ) 
                f file |> Ok
            with e ->             
                Logging.debug "Ошибка обращения к файлу %s, %A, %A" path x e 
                Err e.Message

        static member path ( FilePath x ) = x


    module Path = 
        
        let baddata = lazy (                
            let x = Path.Combine( exepath, "__CORRUPTED__" ) 
            if Directory.Exists x |> not then 
                Directory.CreateDirectory x |> ignore
            x )

        let batch'' canCreate (id:string) (dt:DateTime)  = 
            let (~%%) = string
            let month = dt.ToString("MMM", System.Globalization.CultureInfo.InvariantCulture)
            let path = Path.Combine(Path.batch false dt, id )
            if canCreate then
                createDirectory path
            FilePath path

        let batchFile canCreate id dt  =             
            [|  File.path <| batch'' canCreate id dt
                sprintf "%s.batch"  id |]
            |> Path.Combine
            |> FilePath

        let product canCreate batchId dt productId =
            
            let path = 
                [|  File.path <| batch'' canCreate batchId dt
                    sprintf "%s.product" productId |]
                |> Path.Combine
            if canCreate then 
                createDirectory path
            path

        let batch id dateTime = 
            batch'' false id dateTime
            |> File.path

    let move'corrupted (src:string) = 
        let filename = Path.GetFileName src
        Logging.warn "Файл %s повреждён!" filename 
        try 
            let dest = ( Path.Combine(Path.baddata.Force(), filename) )
            if File.Exists dest then
                File.Delete dest |> ignore
            File.Move( src, dest ) 
        with e ->
            Logging.error  "Ошибка переноса файла %s, %A" filename e.Message 
            Logging.debug  "%A" e

    let getBatchesInfo() =
        Directory.GetFiles( Path.root, "*.batch", SearchOption.AllDirectories)
        |> Array.choose( fun filename -> 
            let r = File.doWith (FilePath filename) FileMode.Open <| fun stream ->
                binarySerializer.Deserialize<Info.Batch>(stream)
            match r  with
            | Err x ->  
                move'corrupted filename                    
                None
            | Ok x -> Some x )
        |> Array.toList


    let mutable batchesInfo = getBatchesInfo()
    
    let open' id dt =
        let (FilePath fileName) as file = Path.batchFile false id dt
        if File.Exists fileName |> not then
            batchesInfo <- batchesInfo |> List.filter( fun y -> y.Id <> id)
            Err ("не найден файл " + fileName)
        else
            File.doWith (FilePath fileName) FileMode.Open <| fun stream ->
                let _ = binarySerializer.Deserialize<Info.Batch>(stream, leaveOpen = true)
                binarySerializer.Deserialize<Batch>(stream)


            

    let save batch = 
        let batchInfo = Info.Batch.creteNew batch
        batch.ProductsSerials <- batchInfo.Serials

        File.doWith (Path.batchFile true batchInfo.Id batchInfo.Date) FileMode.Create <| fun stream ->        
            binarySerializer.Serialize( stream, batchInfo, leaveOpen = true)
            binarySerializer.Serialize( stream, batch)
            batchesInfo <- batchesInfo |> List.map( fun x -> if x.Id=batch.Id then batchInfo else x)
        |> Result.someErr

    let tryGetBatchInfo id = 
        match batchesInfo |> List.tryFind( fun x -> x.Id=id) with
        | None -> None
        | Some batchInfo -> Some batchInfo

    let updateBatchInfo batch = 
        batchesInfo <- 
            ( Info.Batch.creteNew batch) :: 
            ( batchesInfo |> List.filter( fun y -> y.Id <> batch.Id) )

let get'list() =  List.rev batchesInfo

module Batch = 
    
    let remove'ids removeIds =
        let xs1 = batchesInfo |> List.map( fun x -> x.Id, x)
        let existed = Map.ofList xs1
        let existedIds = xs1 |> List.map fst |> Set.ofList
        batchesInfo <-
            Set.difference existedIds removeIds
            |> Seq.map( fun id -> existed.[id])
            |> Seq.toList
            
    let save batch =              
        match save batch with
        | None -> updateBatchInfo batch                
        | Some e -> 
            failwithf "Не удалось сохраниться!\n\n%A\n\n%A" ( Info.Batch.creteNew batch) e


    let create'new() = 
        let b = Batch.createNew()
        save b
        b

    let delete id =
        match tryGetBatchInfo id with
        | None -> Some ("удаляемая партия  не найдена - " + id)
        | Some batchInfo ->
            let (FilePath filename) = Path.batchFile false id batchInfo.Date
            if File.Exists filename |>  not then Some ("удаляемая партия не найдена - " + filename) else
            try            
                let path = Path.batch id batchInfo.Date
                Directory.Delete( path, true )
                batchesInfo <- batchesInfo |> List.filter( fun y -> y.Id <> id)
                None
            with e ->
                Some e.Message

    let open'by'id id =
        match tryGetBatchInfo id with
        | None -> Err "партия не найдена"
        | Some batchInfo ->
            open' id batchInfo.Date 

    let private getLastSaved () =
        if batchesInfo |> List.isEmpty then Err "список партий пуст" else
        let batchInfo = batchesInfo |> List.maxBy ( fun x -> x.Date ) 
        open' batchInfo.Id batchInfo.Date 

    let private get'last'or'create'new() =
        match getLastSaved() with
        | Ok party ->  party
        | Err x ->     
            Logging.error "Не удалось открыть последнюю сохранённую партию - %s" x
            create'new()

    let open'saved() = 
        match get'list() with
        | [] -> create'new()
        | batches ->
            match batches |> List.tryFind(fun x -> x.Id = AppConfig.config.View.PartyId) with
            | None ->
                Logging.warn "партия %A отсутсвует в репозитории"  AppConfig.config.View.PartyId 
                get'last'or'create'new()
            | Some batchInfo ->
                match open'by'id batchInfo.Id with
                | Ok party -> party
                | Err x -> 
                    Logging.error "Не удалось открыть партию %A %s - %s" batchInfo.Date batchInfo.Name x
                    get'last'or'create'new()

let private pair x y = x,y

let getTreeByDate() = 
    get'list()    
    |> Seq.groupBy( fun x -> x.Date.Year )
    |> Seq.sortBy fst
    |> Seq.map( fun ( year, xs) -> 
        xs 
        |> Seq.groupBy( fun x -> x.Date.Month )
        |> Seq.sortBy fst
        |> Seq.map( fun ( month, xs) ->  
            xs 
            |> Seq.groupBy( fun x -> x.Date.Day )
            |> Seq.sortBy fst
            |> Seq.map( fun ( day, xs) -> pair day xs )
            |> pair month )
        |> pair year )