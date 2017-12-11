module STM30.Behaviors.Thread2

open System
open System.ComponentModel

open STM30.ViewModels.Operations
open MainWindow

let operations = BindingList<RunOperationInfo>()
let show'performing'report = Ref.Initializable<_>(sprintf "show'performing'report %s:%s" __LINE__ __SOURCE_FILE__ )
let show'performing'message = Ref.Initializable<_>(sprintf "show'performing'message %s:%s" __LINE__ __SOURCE_FILE__ )

[<AutoOpen>]
module private Helpers1 =
    let none _ = None
    let safe = MyWinForms.Utils.safe
    let party = STM30.ViewModels.Party.party        
    
    let keepRunning = Ref.Observable(false)

let isKeepRunning () = keepRunning.Get()
let notKeepRunning() = not (keepRunning.Get())
let forceStop() = keepRunning.Value <- false

let add'keep'running f = 
    keepRunning.AddChanged f



let scenary = 
    let x = Ref.Observable(Operation.CreateSingle("", none) none)
    x.AddChanged <| fun (o,n) ->
        if keepRunning.Value  then
            failwithf "can not change performing scenary from %A to %A" o.FullName n.FullName
        operations.Clear()
        let ops = Operation.tree n
        ops |> List.iter ( fun x -> 
            let i = Operation.GetRunInfo  x 
            i.Root <- Some ops.[0]
            operations.Add i )        
    x

[<AutoOpen>]
module private Helpers2 =
    let operation = 
        let x = Ref.Observable(None)
        Logging.addLogger <| fun l s ->        
            match  x.Value with
            | Some (op:Operation) -> 
                op.RunInfo.AddLogging l s                 
                show'performing'message.Value l s
            | _ -> ()
            
        |> ignore
        x

    open System.Windows.Forms
    
    let showCantExitMessage() = 
        let s = "Нельзя выйти из приложения пока идёт настройка приборов."
        MessageBox.Show(s,"СТМ-30М", MessageBoxButtons.OK, MessageBoxIcon.Exclamation)
        |> ignore

let add'operation'changed f = 
    operation.AddChanged (snd >> f)

let private is'running = 
    let x = Ref.Observable(false)
    MainWindow.form.Closing.Add <| fun e ->
        if x.Value then 
            e.Cancel <- true
            forceStop()
            showCantExitMessage()
    x

let add'is'running'handler f = 
    is'running.AddChanged <| fun e -> 
        safe form <| fun () -> 
            f e 

let doWhileScenaryPerforming startTime = 
    party.Save()

    

type PerformBuilder() =
    let (>>=) p rest = 
        match p with         
        | None when notKeepRunning() -> None
        | None -> rest()
        | x -> x 
    
    member b.Bind(p, rest) = p >>= rest

    member b.Bind(p, rest) = 
        match p with 
        | Ok _ when notKeepRunning() -> None
        | Ok x -> rest x
        | Err x -> Some x         

    member b.Delay(f : unit -> string option) = f

    member x.Run(f) = f()

    member x.While(cond, f) =
        if isKeepRunning() && cond() then f() >>= fun _ -> x.While(cond, f)
        else None

    member x.For(e:seq<_>,f) = 
        let cursor = e.GetEnumerator()
        let body () =  f cursor.Current        
        try
            x.While(cursor.MoveNext, x.Delay(body) )
        with _ ->
            cursor.Dispose()
            reraise()
        
    member b.ReturnFrom (x : string option) = x

    member b.ReturnFrom (x : Result<string,_>) = 
        match x with
        | Err x -> Some x
        | Ok _ -> None

    member b.Return (error:string) = Some error
    member b.Return (x:unit) = None    

    member b.Combine(m, f ) = 
        m >>= f
        
    member b.Zero() = None

    member x.TryWith(p, handler) = 
        try 
            p()
        with  exn -> 
            handler exn 

    member m.TryFinally(f, finalizer) = 
        try 
            f()
        finally 
            finalizer()

let maybeErr = PerformBuilder ()


let sleep50() = Threading.Thread.Sleep 50

let sleep t = 
    let t = TimeSpan.FromMilliseconds( float t )
    let start = DateTime.Now
    while DateTime.Now - start < t && isKeepRunning() do
        System.Threading.Thread.Sleep 50
    None    

let doWork work = 
    if notKeepRunning() then None
    elif party.HasNotOneCheckedProduct() then Some "выполнение прервано, так как невыбрано ни одного прибора"
    else work()

let private perfomOperationEvent = new Event<_>()
[<CLIEvent>]
let PerfomOperationEvent = perfomOperationEvent.Publish

let run =
    
    let doBeg op = 
        let prev'op = operation.Get()
        operation.Set (Some op)
        show'performing'message.Value Logging.Info ""
        perfomOperationEvent.Trigger(op,true)
        fun () -> 
            operation.Set prev'op
            perfomOperationEvent.Trigger(op,false)


    fun (needClosePneumo)  (x : Operation)  -> 
        
        if keepRunning.Value || is'running.Value then
            failwith "already performing"
        scenary.Set x
        operation.Set (Some x)
        keepRunning.Value <- true
        is'running.Set true   
        Logging.info "Начало выполнения сценария %A" x.FullName     
        
        let dostart, dostop = MyWinForms.Utils.timer 1000 doWhileScenaryPerforming
        dostart()

        async{
            let r = Operation.Perform doBeg isKeepRunning x
            let wasBreakedByUser = (keepRunning.Value = false)
            keepRunning.Value <- false

            if needClosePneumo then 
                Logging.warn "Выполняется операция закрытия пневмоблока после выполнения сценария" 
                match Stend.closePneumo() with
                | None ->
                    Logging.warn "Пневмоблок закрыт после выполнения сценария" 
                | Some err ->
                    Logging.error "При закрытии пневмоблока после выполнения сценария произошла ошибка: %s" err 
            
            let level,message = 
                if Operation.WasErrorWhenRunning x then
                    Logging.Warn, "при выполнении произошли ошибки"
                elif wasBreakedByUser then
                    Logging.Warn, "выполнение было прервано"
                else
                    match r with
                    | Some err -> Logging.Error, err 
                    | _ -> Logging.Info, "Выполнено"
                                    
            safe form dostop
            Comport.Ports.closeOpenedPorts (fun _ _ -> true) 
            Logging.write level "Окончание выполнения сценария %A - %s" x.FullName message
            let title = sprintf "%s %s" x.RunInfo.Status x.FullName
            show'performing'report.Value title level message 
            operation.Set None 
            party.Save() 
            is'running.Set false}
        |> Async.Start