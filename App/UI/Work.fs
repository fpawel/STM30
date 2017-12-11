module STM30.Behaviors.Work

open System
open STM30
open STM30.Behaviors.Thread2
open STM30.Behaviors.PartyBehavior
open System.ComponentModel
open STM30.ViewModels.Operations

module Delay = 
    let on'start = Ref.Initializable<_>(sprintf "Delay.start %s:%s" __LINE__ __SOURCE_FILE__ )
    let on'stop = Ref.Initializable<_>(sprintf "Delay.stop %s:%s" __LINE__ __SOURCE_FILE__ )
    let on'update = Ref.Initializable<_>(sprintf "Delay.stop %s:%s" __LINE__ __SOURCE_FILE__ )

    let mutable private keep'running = false

    let cancel() = keep'running <- false

    let private perform what gettime work = 
        on'start.Value what gettime
        keep'running <- true
        let start'time = DateTime.Now
        while keep'running && Thread2.isKeepRunning() && (DateTime.Now - start'time < gettime()) do
            on'update.Value start'time gettime
            work()
        keep'running <- false
        on'stop.Value()

    let doWithInterrogate what gettime = 
        perform what gettime ( interrogate >> ignore)

    let do' what gettime = 
        perform what gettime  (fun () -> Threading.Thread.Sleep 10)

[<AutoOpen>]
module private Helpers = 
    open STM30.ViewModels.Operations

    let party = STM30.ViewModels.Party.party
    let none _ = None
    let (<|>) what f = 
        Operation.CreateSingle (what, none) f 
    

    let (<-|->) (what,time) f = 
        Operation.CreateTimed (what, none) time f
    
    let (<||>) what xs =  Operation.CreateScenary ( what, none)  xs

    let doProductionWork prod work = 
        for p in checkedProducts() do
            if isKeepRunning() then
                p.SetProduction prod ( work p )

    let production prod ( prepare : unit -> string option) work = 
        ProductionPoint.what prod <|> fun () -> maybeErr {
            do! prepare()
            doProductionWork prod work}

    let configModbus = AppConfig.config.ComportProducts
    let configStend = AppConfig.config.ComportStend

    let testConcProduct ptGas (p:ViewModels.Party.Product) = maybeErr{
        let gas = PtGas.gas ptGas
        let whatPt = PtGas.what ptGas
        for var in VarType.values do
            if notKeepRunning() then () else
            let! value = p.Reader.ReadVar var
            p.SetMain var ptGas (Some value)
            match p.AlchemyMain var ptGas with
            | None -> Logging.error "не удалось выполнить расчёт основной погрешности прибора %s" p.What
            | Some ts ->
                Logging.write 
                    (if ts.IsError then Logging.Error else Logging.Info)
                    "%s, %s" p.What ts.What
        let! releValue = p.Reader.ReadRele()
        let releValue = Rele.getStates releValue
        p.SetRelesMain ptGas <| Some releValue
        let valid = Rele.validStates ( PtGas.gas ptGas )
        let s = Rele.statesToStr releValue
        if releValue = valid then 
            Logging.info "%s, реле %s" p.What s
        else 
            Logging.error "%s, реле %s, должно быть %s" p.What s (Rele.statesToStr valid) }

let resetReles() = 
    writeParty Mdbs16.RESET 0m

    
let closePneumo() = maybeErr{
    let config = AppConfig.config.Stend
    do! Stend.switchPneumo 0xffuy
    return! Stend.setConsumption 0m }

let switchGas gas = maybeErr{
    let config = AppConfig.config.Stend
    do! Gas.n gas |> byte |> Stend.switchPneumo
    do! Thread2.sleep 1000
    return! Stend.setConsumption ( (decimal (checkedProducts().Length) ) * config.ConsK * config.ConsD) }

let testConc ptGas = 
    let gas = PtGas.gas ptGas
    let whatPt = PtGas.what ptGas
        
    [   (sprintf "%s, продувка I" whatPt, TimeSpan.FromMinutes 1.)  <-|-> fun getTime -> maybeErr{
            do! switchGas (PtGas.gas ptGas)
            Delay.doWithInterrogate (sprintf "%s, продувка I" whatPt)  getTime 
            do! resetReles()  }

        (sprintf "%s, продувка II" whatPt, TimeSpan.FromMinutes 2.)  <-|-> fun getTime -> 
            Delay.doWithInterrogate (sprintf "%s, продувка II" whatPt) getTime 
            None

        sprintf "проверка %s" (PtGas.what ptGas) <|> fun () -> 
            doWithProducts (testConcProduct ptGas >> ignore )
            None  ]

let blowAir() = 
    ("Продувка воздухом", TimeSpan.FromMinutes 1.) <-|-> fun gettime -> maybeErr{
        do! switchGas Gas1
        Delay.do' "Продувка воздухом" gettime
        do! closePneumo() 
        do! resetReles()
        do! sleep 1000 }


let adjust scale'point = 
    let gas, cmd, s' = 
        match scale'point with
        | ScaleEnd -> Gas3, Mdbs16.ADJ_S, "чувствительности"
        | ScaleNull -> Gas1, Mdbs16.ADJ_0, "нуля"

    let whatBlow = sprintf "Продувка ПГС%d" (Gas.n gas)

    "Калибровка " + s' <||> [            
        yield (whatBlow, TimeSpan.FromMinutes 3.) <-|-> fun getTime -> maybeErr{
            do! switchGas gas
            Delay.doWithInterrogate whatBlow getTime  
            do! resetReles()
            Delay.doWithInterrogate whatBlow (fun () -> TimeSpan.FromSeconds 10.)
        }

        yield ("Запись в приборы", TimeSpan.FromMinutes 3.) <-|-> fun getTime -> maybeErr{
            do! writeParty cmd (party.GetPgs gas)  
            Delay.doWithInterrogate "Задержка на калибровку" getTime
            do! resetReles()
            do! sleep 1000
        }

        
        yield production (TestAdjust scale'point) Mdbs.testPort ( fun p -> maybeErr{
            let! _,mode,_ = p.Reader.ReadStatus()
            if mode<>0 then return "прибор не вышел из режима калибровки" else            
            if scale'point=ScaleEnd then
                let! conc = p.Reader.ReadVar Conc
                p.Adjust <- Some conc} )
        if scale'point=ScaleEnd then
            yield blowAir() 
    ]

            
module ModalMessage = 
    let on'show = Ref.Initializable<_>(sprintf "ModalMessage.on'show %s:%s" __LINE__ __SOURCE_FILE__ )
    let get'is'vivisble = Ref.Initializable<_>(sprintf "ModalMessage.on'show %s:%s" __LINE__ __SOURCE_FILE__ )
    let on'close = Ref.Initializable<_>(sprintf "ModalMessage.on'close %s:%s" __LINE__ __SOURCE_FILE__ )
        

    let show title level text = 
        on'show.Value title level text
        while Thread2.isKeepRunning() && get'is'vivisble.Value() do
            Threading.Thread.Sleep 50
        on'close.Value()    

let main =     
    
    let (<!!>) prod (f1,f) = production prod f1 f
    let (<!>) prod work = 
        ProductionPoint.what prod <|> fun () -> 
            doProductionWork prod work
            None 

    let (~%%) (title,text) = ModalMessage.show title Logging.Warn text
    let softVersion = [0x32uy; 0x6Buy; 0x02uy; 0x11uy ]

    "Производство СТМ-30" <||> 
        [   yield SoftVersionValue <!> fun p ->  maybeErr{
                let! d = Mdbs.read3bytes p.Addy 53 2
                return!
                    match d with
                    | EqualsTo softVersion true -> None
                    | d -> Some <| sprintf "%s не соотв. %s" (bytesToStr d) (bytesToStr softVersion)  }            
            
            yield SetPorogs <!> fun p ->  maybeErr{
                let t = party.GetProductType()
                do! writeProduct Mdbs16.POR_1 t.Porog1 p
                do! writeProduct Mdbs16.POR_2 t.Porog2 p 
                do! resetReles()}
            
            yield TestMode <!> fun p -> maybeErr{
                do! writeProduct Mdbs16.SET_I_4 0m p     
                let! mode = p.Reader.ReadReleState Mode 
                if not mode then  return "разомкнуто в режиме подстройки" else    
                do! writeProduct Mdbs16.SET_I_4 1m p
                let! mode = p.Reader.ReadReleState Mode
                if mode then return "замкнуто после выхода из режима подстройки" }
            
            yield "Проверка реле ОТКАЗ" <|> fun () -> maybeErr{
                
                %% ("Проверка включения реле ОТКАЗ","Отключите датчики")
                do! sleep 5000
                do! Stend.testPort()
                doProductionWork TestFailureOn <| fun p -> maybeErr{
                    let! r = p.Reader.ReadReleState Failure 
                    if not r then return "не включилось" }

                %% ("Проверка выключения реле ОТКАЗ","Подключите датчики" )
                do! sleep 60000
                do! Stend.testPort()
                doProductionWork TestFailureOff <| fun p -> maybeErr{
                    let! r = p.Reader.ReadReleState Failure 
                    if r then return "не выключилось" } } 

            yield "Подстройка" <||> listOf{
                let! var = [ TuneCurr; TuneTens ]
                let! scale = [ ScaleNull; ScaleEnd ]   
                let _,_,_,_,what = Tune.varPtNfo (var, scale)
                let prod = Tune(var,scale)

                return ProductionPoint.what prod <|> fun () -> maybeErr {
                    do! testPorts()
                    do! resetReles()
                    doWithProducts ( Tune.tune var scale >> ignore ) } }

            yield adjust ScaleNull
            yield adjust ScaleEnd
            yield "Проверка осн. погр." <||> [   
                for pt in PtGas.values do
                    yield! testConc pt
                yield blowAir() ] ]   
    |> fun x ->
        let fileName = IO.Path.Combine(exepath, "scenary.json")
        let subj = 
            if IO.File.Exists fileName |> not then                
                Logging.debug "не найден файл сценария %A" fileName                
                Config.CreateNew()
            else                
                try
                    match Json.Serialization.parse<Config> (IO.File.ReadAllText(fileName)) with
                    | Ok x -> x
                    | Err error ->
                        Logging.error "ошибла файла сценария %s\n%s" fileName error
                        Config.CreateNew()
                with e ->             
                    Logging.error "ошибла файла сценария %s\n%A" fileName e 
                    Config.CreateNew()
        Operation.SetConfig (x,subj)

        MainWindow.form.Closing.Add <| fun _ ->
            let subj = Operation.GetConfig x
            try
                IO.File.WriteAllText(fileName, Json.Serialization.stringify subj ) 
            with e ->             
                Logging.error "ошибла сохранения файла сценария %s\n%A" fileName e 
            let json'content = Json.Serialization.stringify subj            
            IO.File.WriteAllText( IO.Path.Combine(exepath, "scenaryConfig.json"), json'content  )
        Thread2.scenary.Set x    
        x

let runInterrogate =
    let rec loop () = 
        doWork <| fun () ->
            match interrogate () with
            | None -> loop()
            | x -> x     
    let operation = "Опрос" <|> loop
    fun () ->        
        run false operation 


