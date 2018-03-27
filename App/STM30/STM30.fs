namespace STM30

open System
//open System.ComponentModel

type Color = System.Drawing.Color

type Gas = 
    | Gas1
    | Gas2
    | Gas3
    member x.What = Gas.what x
    static member values = unionCasesList<Gas>
    static member what = function
        | Gas1 -> "ПГС1"
        | Gas2 -> "ПГС2"
        | Gas3 -> "ПГС3"
    static member n x =
        Gas.values |> List.findIndex ( (=) x) |> (+) 1

type Rele =
    |   Porog1
    |   Porog2
    |   Failure
    |   Mode
    |   Status
    member x.What1 = 
        let _,x,_,_ = Rele.info x
        x

    member x.Foreground = 
        let _,_,x,_ = Rele.info x
        x

    static member info  = function
        | Porog1 ->  "ПОРОГ1", "П1", Color.DarkGreen, (false, true, true)
        | Porog2 ->  "ПОРОГ2", "П2", Color.BlueViolet, (false, true, true)
        | Failure -> "АВАРИЯ", "Ав", Color.Green, (false, false, false)
        | Mode ->    "РЕЖИМ", "Ре", Color.Brown, (false,  false, false)
        | Status ->  "СТАТУС", "Ст", Color.DarkSlateBlue, (true, true, true)

    static member statesToStr states = 
        states |> listToStr " " ( fun r -> 
            let _,s,_,_ = Rele.info r
            s )
        

    static member values = unionCasesList<Rele>

    static member order rele = List.findIndex ( (=) rele ) Rele.values

    static member getState portValue rele = 
        not <| bit (Rele.order rele) portValue

    static member validState gas x = 
        let _,_,_,(v1,v2,v3) = Rele.info x
        match gas with
        | Gas1 -> v1
        | Gas2 -> v2
        | Gas3 -> v3

    static member validStates gas = 
        Rele.values |> List.filter(Rele.validState gas) |> Set.ofList

    static member getStates value =
        Rele.values 
        |> List.filter ( fun rele ->  Rele.getState value rele )
        |> Set.ofList
        

    static member readState addy =
        match Stend.getRele addy with
        | Err x -> Err x
        | Ok b ->
            Rele.values 
            |> List.map ( fun rele ->  rele, Rele.getState b rele )
            |> Map.ofList 
            |> Ok

    static member readReleState rele addy =         
        match Rele.readState addy with
        | Err x -> Err x
        | Ok m -> Ok m.[rele]

[<AutoOpen>]
module Helpers1 = 
    let concToCurr x = 
        x * 0.32m + 4m
    let (|ConcToCurr|) = concToCurr

    let currToConc x = 
        (x - 4m)/0.32m
    let (|CurrToConc|) = currToConc

    let concToTens x = 
        x * 0.02m
    let (|ConcToTens|) = concToTens

    let tensToConc x = 
        x / 0.02m
    let (|TensToConc|) = tensToConc

type VarType = 
    | Conc
    | Curr
    | Tens
    member x.What = VarType.what x

    static member what = function
        | Conc -> "Конц."
        | Curr -> "I.вых."
        | Tens -> "U.вых."

    static member values = unionCasesList<VarType>

    static member propertyPath = function
        | Conc -> "C"
        | Curr -> "I"
        | Tens -> "U"

    static member stringFormat = function
        | Conc -> "0.##"
        | Curr -> "0.###"
        | Tens -> "0.####"

    static member header = function
        | Conc -> "Конц."
        | Curr -> "I вых."
        | Tens -> "U вых."

    static member valueToConc = function
        | Conc -> id
        | Curr -> currToConc
        | Tens -> tensToConc

    static member concToValue = function
        | Conc -> id
        | Curr -> concToCurr
        | Tens -> concToTens

type PtGas = 
    | PtGas1
    | PtGas2
    | PtGas3
    | PtGas4
    | PtGas5
    | PtGas6

    member x.What = PtGas.what x
    member x.Gas = PtGas.gas x
    
    static member values = unionCasesList<PtGas>

    static member gas = function
        | PtGas1 -> Gas1
        | PtGas2 -> Gas2
        | PtGas3 -> Gas3
        | PtGas4 -> Gas2
        | PtGas5 -> Gas1
        | PtGas6 -> Gas3

    static member what x =
        let n = PtGas.values |> List.findIndex ((=) x) |> (+) 1 
        let ngas = Gas.values |> List.findIndex ((=) (PtGas.gas x) ) |> (+) 1
        sprintf "%d-ПГС%d" n ngas

    
    
type Id = string

type ScalePoint = 
    | ScaleNull | ScaleEnd
    member x.What = ScalePoint.what x
    static member what = function 
        | ScaleNull -> "Начало шкалы"
        | ScaleEnd -> "Конец шкалы"

type TuneVarPoint = 
    | TuneCurr|  TuneTens 

    member x.What = TuneVarPoint.what x

    static member what = function 
        | TuneCurr -> "Ток"
        | TuneTens -> "Напряжение"

    static member config = function 
        | TuneCurr -> AppConfig.config.TuneCurr 
        | TuneTens -> AppConfig.config.TuneTens

    static member read = function 
        | TuneCurr -> Stend.geti
        | TuneTens -> Stend.getu

type ProductionPoint =
    | ConnectionModbus
    | ConnectionStend
    | SetPorogs    
    | TestAdjust of ScalePoint    
    | TestMode
    | TestFailureOn
    | TestFailureOff
    | SoftVersionValue
    | Tune of TuneVarPoint * ScalePoint

    static member info norm = function
        | Some None -> norm, Color.Navy, Color.Azure
        | Some (Some x) -> x, Color.Red, Color.LightGray
        | _ -> "", Color.Black, Color.White

[<AutoOpen>]
module Helpers2 = 
    let whatTune = function
        | TuneCurr, ScaleNull -> "Iвых 4 мА"
        | TuneCurr, ScaleEnd -> "Iвых 20 мА"
        | TuneTens, ScaleNull -> "Uвых 0 В"
        | TuneTens, ScaleEnd -> "Uвых 1 В"

    let productions =     
        [   yield ConnectionModbus, "MODBUS"
            yield ConnectionStend, "СТЕНД"
            yield SetPorogs, "Уст. порог."
            yield SoftVersionValue, "Вер. ПО"
            yield TestMode, "Проверка РЕЖИМ"
            yield TestFailureOn, "Проверка вкл. ОТКАЗ"
            yield TestFailureOff, "Проверка выкл. ОТКАЗ"
            for tn in [TuneCurr; TuneTens] do 
                for sc in [ScaleNull; ScaleEnd] do
                    let x = tn,sc
                    yield Tune x, whatTune x
            yield TestAdjust ScaleNull, "Проверка калибр. нуля"
            yield TestAdjust ScaleEnd, "Проверка калибр. чувст."]
        |> Map.ofList

type ProductionPoint with
    static member values = productions |> Map.toList |> List.map fst
    static member what x = productions.[x]
    member x.What = ProductionPoint.what x

type LoggingRecord = DateTime * Logging.Level * string

type Product = 
    {   Id : Id
        mutable IsChecked : bool
        mutable Serial : int
        mutable Addy : byte
        mutable Adjust : decimal option
        mutable Main : Map<VarType*PtGas, decimal>
        mutable ReleMain : Map<PtGas, Set<Rele> > 
        mutable Production : Map<ProductionPoint, LoggingRecord > }

    member x.What = Product.what x

    static member createNewId() = getUniqueKey 12
    static member what x = sprintf "№%d #%d" x.Serial x.Addy 
    static member createNew() = 
        { Runtime.generateFsharpObject<Product>() with 
            Id = Product.createNewId() 
            IsChecked = true}
    
    static member getProduction prod x = Map.tryFind prod x.Production
    static member setProduction prod x (level,text) = 
        x.Production <- Map.add prod (DateTime.Now, level, text) x.Production

    static member getReles gas x = Map.tryFind gas x.ReleMain

    static member setReles gas x v = 
        x.ReleMain <- x.ReleMain |> (match v with
                                     | None -> Map.remove gas
                                     | Some v -> Map.add gas v )
        
        
    static member setReles1 gas x value = x.ReleMain <- Map.add gas (Rele.getStates value) x.ReleMain

    static member getRele gas rele x = 
        Product.getReles gas x
        |> Option.map( Set.contains rele )
    
//    static member setRele gas rele x value = 
//        x.ReleMain <- 
//            Map.add 
//                gas
//                ( (if value then Set.add else Set.remove) rele (Product.getReles gas x) )
//                x.ReleMain

    static member getMain var gas x = Map.tryFind (var,gas) x.Main
    static member setMain var gas x value = 
        x.Main <-
            (   match value with
                | None -> Map.remove (var,gas)
                | Some v -> Map.add (var,gas) v ) x.Main


    
type ProductType =
    {   mutable Name : string
        mutable NormativeDoc : string
        //mutable Porog1 : decimal
        //mutable Porog2 : decimal
        //mutable ErrorLimit : decimal 
        //mutable AdjustErrorLimit : decimal 
        }
    static member createNew() = 
        {   Name = "СТМ-30-"
            NormativeDoc = "413216.050-11"
            //Porog1 = 7m
            //Porog2 = 12m
            //ErrorLimit = 5m 
            //AdjustErrorLimit = 0.2m
            }

module ProductTypes =     
    open System.ComponentModel
    let values, save =  
        let filename = "productTypes.json"
        let values = Logging.JsonConfig.read filename <| fun () -> 
            [   {   Name = "СТМ-30М-10ДЦ"; NormativeDoc = "413216.050-11-10" }
                {   Name = "СТМ-30М-10ДБ"; NormativeDoc = "413216.050-11-11" } 
            ] 
        let values = ResizeArray<ProductType>(values)
        let values = new BindingList<ProductType>( values ) 
        let save() = Logging.JsonConfig.write filename [ for x in values -> x ]
        values,save

type RunOp = 
    {   Start : DateTime option 
        End : DateTime option
        Logging : LoggingRecord list }
    static member createNew() = 
        {   Start = None
            End = None
            Logging = [] }
 
    

type RunInfo = 
    {   mutable RunOps : Map<string, RunOp > }

    member x.get operationName =
        match Map.tryFind (md5hash operationName) x.RunOps  with 
        | None -> RunOp.createNew()
        | Some i -> i

    member x.set operationName i =
        x.RunOps <- Map.add (md5hash operationName) i x.RunOps

    member x.SetStart operationName = 
        let v = { Start = Some DateTime.Now        
                  End = None
                  Logging = []}
        x.set operationName v 

    member x.SetEnd operationName = 
        let v = x.get operationName
        x.set operationName { v with End = Some DateTime.Now }

    member x.AddRecord operationName level text = 
        let v = x.get operationName
        x.set operationName { v with Logging = (DateTime.Now,level,text)::v.Logging }

type Batch =
    {   Id : Id
        Date : DateTime
        mutable ProductsSerials : int list
        mutable Name : string
        mutable ProductType : string
        mutable PGS1 : decimal
        mutable PGS2 : decimal
        mutable PGS3 : decimal
        mutable Products : Product list  
        RunInfo : RunInfo}

    static member getNewValidAddy batch = 
        let rec loop n = 
            if batch.Products |> List.exists( fun x -> x.Addy=n ) then
                if n=127uy then 1uy else loop (n+1uy)
            else n
        loop 1uy

    static member createNew () = 
        {   Runtime.generateFsharpObject<Batch>() with
                Id = Product.createNewId()
                ProductsSerials = []
                Date=DateTime.Now
                ProductType="035"
                PGS1=0m
                PGS2=50m
                PGS3=100m
                Name = "Партия1" 
                Products = [ {Product.createNew() with Addy = 1uy} ] }

    static member createNew1 name productType pgs1 pgs2 pgs3 count = 
        {   RunInfo = { RunOps = Map.empty } 
            Id = Product.createNewId()
            ProductsSerials = []
            Date=DateTime.Now
            ProductType=productType
            PGS1=pgs1
            PGS2=pgs2
            PGS3=pgs3
            Name = name
            Products = [1uy..count] |> List.map( fun n -> 
                { Product.createNew() with Addy = n; Serial = int n}  ) }
        
    static member getProductsSerials (b : Batch ) = 
        b.Products
        |> List.map( function { Serial = serial } -> serial  )

    static member pgsConc gas b =
        match gas with
        | Gas1 -> b.PGS1
        | Gas2 -> b.PGS2
        | Gas3 -> b.PGS3
     
    static member ptGasConc ptGas b = Batch.pgsConc (PtGas.gas ptGas) b

    static member productType (x:Batch) = 
        let y = ProductTypes.values |> Seq.tryFind(fun prodType -> prodType.Name=x.ProductType)
        match y with
        | Some y -> y
        | None when ProductTypes.values.Count=0 -> ProductType.createNew()
        | _ -> ProductTypes.values.[0]

namespace STM30.Info 

open System

type Batch = 
    {   Id : STM30.Id
        Date : DateTime
        Serials : int list
        mutable Name : string
        mutable ProductType : string         
        }
    static member creteNew (x : STM30.Batch) : Batch = 
        {   ProductType = x.ProductType
            Id = x.Id
            Date = x.Date
            Name = x.Name 
            Serials = 
                x.Products  
                |> List.map( function { Serial = serial } -> serial  ) }


   