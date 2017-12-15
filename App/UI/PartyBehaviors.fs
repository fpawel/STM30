module STM30.Behaviors.PartyBehavior

open System

open STM30

open STM30.Behaviors.Thread2
//open ProductIO

type private P = ViewModels.Party.Product

let products() = ViewModels.Party.party.Products |> Seq.toList
let checkedProducts() = 
    products()
    |> List.filter( fun p -> p.IsChecked )
let hasNotCheckedProduct() = checkedProducts() |> List.isEmpty 
let hasCheckedProduct() = not <| hasNotCheckedProduct()


let doWithProducts f = 
    checkedProducts() |> List.iter ( fun p ->       
        if isKeepRunning() && p.IsChecked then 
            f p ) 

let interrogate() = maybeErr {
    let v = AppConfig.config.Var

    let is'modbus = v.Conc || v.Mode
    let is'stend = v.Curr || v.Rele || v.Tens

    if is'modbus || not is'stend  then 
        do! Mdbs.testPort()
    if is'stend then
        do! Stend.testPort()
    doWithProducts <| fun p ->
        p.Reader.Interrogate() |> ignore }

let writeProduct cmd arg (p:ViewModels.Party.Product)  = 
    Mdbs16.write p.Addy cmd arg 
    
let writeParty cmd arg = maybeErr{
    let v = AppConfig.config.Var
    do! Mdbs.testPort() 
    doWithProducts (writeProduct cmd arg >> ignore) }

let testPorts() = maybeErr{
    do! Mdbs.testPort()
    do! Stend.testPort() }    

module Tune = 
    open Mdbs16
    let private safe = MyWinForms.Utils.safe

    let private cfgI = AppConfig.config.TuneCurr 
    let private cfgU = AppConfig.config.TuneTens 

    let varPtNfo = function
        | TuneCurr,ScaleNull -> SET_I_4, INC_I_4, cfgI.ScaleNullMin, cfgI.ScaleNullMax, "4 мА"
        | TuneCurr,ScaleEnd -> SET_I_20, INC_I_20, cfgI.ScaleEndMin, cfgI.ScaleEndMax, "20 мА"
        | TuneTens,ScaleNull -> SET_U_0, INC_U_0, cfgU.ScaleNullMin, cfgU.ScaleNullMax, "0 В"
        | TuneTens,ScaleEnd -> SET_U_1, INC_U_1, cfgU.ScaleEndMin, cfgU.ScaleEndMax, "1 В"

    [<AutoOpen>]
    module private Helpers =
        let ofBool = function true -> 1m | _ -> 0m
        let whatConfig var pt = 
            let _, _, vmin, vmax,_ = varPtNfo (var,pt)
            let config = TuneVarPoint.config var
            let step = config.Step
            sprintf "мин %M, макс %M, шаг %M" vmin vmax step 

        type I = 
            {   TuneVar : TuneVarPoint
                TuneScale : ScalePoint
                TuneProduct : P
                TuneStart : DateTime }
            static member info i = varPtNfo (i.TuneVar, i.TuneScale)

        type A = 
            {   Count : int
                Inc : bool }

        let rec sendTuneCommand i a = maybeErr{
            let cmmode, cminc, _, _, what = I.info i
            let cmincCode = cminc.Code + 256 * (a.Count - 1)
            let arg = 
                let arg = a.Inc
                if i.TuneVar=TuneCurr then arg else not arg
                |> ofBool
            let config = TuneVarPoint.config i.TuneVar

            Logging.info "%X (%M)" cmincCode arg 
            do! Mdbs.write i.TuneProduct.Addy cmincCode "подстройка" arg 
            do! sleep config.Delay                
            if isKeepRunning() then 
                return! tune1 i
            else 
                return "Подстройка прервана" }

        and tune1 i =  maybeErr{
            let config = TuneVarPoint.config i.TuneVar
            let cmmode, cminc, vmin, vmax,what = I.info i
            let step = config.Step
            let what = sprintf "#%d, подстройка %s, %M..%M" i.TuneProduct.Addy what vmin vmax

            if config.Step=0m then 
                return "установлен нулевой шаг подстройки"

            if DateTime.Now - i.TuneStart >= config.Timeout then
                return sprintf "Длительность подстройки превысила установленный таймаут %A" config.Timeout else
            let! value = 
                match i.TuneVar with 
                | TuneCurr -> Curr 
                | TuneTens -> Tens
                |>  i.TuneProduct.Reader.ReadVar

            if value < config.Unsense then
               Logging.info "сигнал %M в зоне нечувствительности %M, шаг %d" value config.Unsense config.StepUnsense
               return! sendTuneCommand i {Count = config.StepUnsense; Inc = true } 
            else
                let wt' =
                    match value <= vmin, value >= vmax with
                    | false, false -> None 
                    | true,_ -> Some (vmin, true, "увеличение")
                    | _,true -> Some (vmax, false, "уменьшение")

                match wt' with
                | None -> 
                    Logging.info "%s, сигнал %M, соответсвует" what value 
                    i.TuneProduct.TuneValue <- value
                    return ()
                | Some (vdest, inc, whatDo) ->
                    let count = 
                        let count =Decimal.ToInt32 <| abs (vdest - value) / step                            
                        if count > config.MaxStepsCount then config.MaxStepsCount
                        elif count=0 then 1 
                        else  count
                    Logging.info  "%s, сигнал %M, %s %d шагов, шаг %M" what value whatDo count step
                    return! sendTuneCommand i {Count = count; Inc = inc } }

        let tune2 var pt (p:P)  = maybeErr {        
            let cmmode, _, vmin, vmax, what = varPtNfo (var,pt)
            let what = sprintf "%d, подстройка %s, %M..%M" p.Addy what vmin vmax
            Logging.info "%s, начало подстройки" what
            do! writeProduct cmmode 0m p       
            let! mode = p.Reader.ReadReleState Mode 
            if not mode then 
                return "Реле \"РЕЖИМ\" разомкнуто в режиме подстройки" 
            let r = 
                tune1 { TuneStart = DateTime.Now 
                        TuneVar = var 
                        TuneScale =  pt 
                        TuneProduct = p }
            
            do! writeProduct cmmode 1m p
            let! mode = p.Reader.ReadReleState Mode 
            if mode then 
                return "Реле \"РЕЖИМ\" замкнуто после выхода из режима подстройки"        
            return! r }

    let tune var pt (p:P)  = maybeErr {
        let r = tune2 var pt p
        p.SetProduction1 
            (Tune(var,pt))
            (   match r with
                | None -> Ok(sprintf "%M" p.TuneValue)
                | Some e -> Err e )
        return! r}