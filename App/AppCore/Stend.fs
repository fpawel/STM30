module Stend

open System
open System.Collections.Generic
open System.ComponentModel
open System.ComponentModel.DataAnnotations

open Comport.Protocol
    
[<AutoOpen>]
module private Helpers = 

    let comportConfig = AppConfig.config.ComportStend
    let stendConfig = AppConfig.config.Stend
    let pneumoAddy() = AppConfig.config.PneumoAddy
    
    let transmitEvent = new Event<_>()
    //let stend = config.Stend

    let makeTxD request = 
        let x = 
            [   yield byte (4 + Seq.length(request.data) )
                yield byte request.addy
                yield byte request.cmd 
                yield! request.data ] 
        [|  yield! x
            yield x |> List.fold ( fun acc y -> acc + y ) 0uy |]

    let isCrc (rx : byte []) = 
        let len = Array.length rx
        let rx1 = rx.[0..len-2]
        let sum =  rx1 |> Array.fold (+) 0uy
        sum = rx.[len-1]

    let checkRxD request rxd = 
        let len = Array.length rxd
        if len=0 then 
            (   if request.addy = pneumoAddy() then
                    sprintf "не отвечает пневмоблок стенда с адресом %d" request.addy
                else "не отвечает" ) |> Err
        elif len<4 then Err ( sprintf "менее 4 байт %d" len )
        elif byte len <> rxd.[0] then Err ( sprintf "Несоответствие дины ответа len==%d != rxd[0]==%d" len rxd.[0])
        elif not <| isCrc rxd then Err "несоответсвие контрольной суммы в ответе" else
        let rx = rxd.[1..len-2] |> Array.toList
        match rx with     
        | xaddy::xcmd::data  -> 
            if xaddy <> byte request.addy then Err ( sprintf "Несовпадение адреса запроса %d и ответа %d" request.addy xaddy )
            elif xcmd <> byte request.cmd then Err ( sprintf "Несовпадение кода команды запроса %d и ответа %d" request.cmd xcmd)
            else Ok data
        | _ -> Err "Неизвестный формат ответа"

    let proto = "СТЕНД", comportConfig, transmitEvent.Trigger
    
    
    let debug format = 
        Printf.kprintf (fun s -> if comportConfig.CanLog then Logging.debug "%s" s ) format

    let whatAddy x = sprintf "##%d" x
    
    let (~%%) x = sprintf "[%s]" (bytesToStr x)

    let transact request parse =
        getResponse proto (makeTxD, checkRxD) request parse

[<CLIEvent>]
let TransmitEvent = transmitEvent.Publish

let testPort() = Comport.testPort comportConfig

let OUTPUT_OPTORELE_COUNT = 9

let setReleN addy n state = 
    let request =
        {   what = sprintf "реле.%s.%d <-- %d" (whatAddy addy) n state
            addy = addy
            cmd = 23uy
            data = [n; state] 
            canLog = true}
    transact request (fun _ -> "") Ok
    
let setAllReles addy state = 
    let rec loop n = 
        if n<OUTPUT_OPTORELE_COUNT then            
            match setReleN addy (byte n) state with
            | Ok _ -> loop (n+1)
            | Err x -> Some x
        else None
    loop 0

type LimitU = 
    | Limit25 
    | Limit250
let lim = function Limit25 -> 0uy,"25" | _ -> 1uy,"250"
type SignalT = AC  | DC
let sigt = function AC -> 0uy | _ -> 1uy

let  byte2 x = [ x >>> 8 |> byte ; byte x ] 

let getv addy what n lm sgt count mxtime = 
    let lm,limWhat = lim lm
    let what = sprintf "%s, %s, канал %d, предел %s, измерений %d, время %d" what (unif sgt) n limWhat count mxtime
    let sgt = sigt sgt
    let data = 
        [   yield n; yield lm; yield sgt; 
            yield! byte2 count 
            yield! byte2 mxtime  ]

    let request =
        {   what = what
            addy = addy
            cmd = 31uy
            data = data
            canLog = false}

    transact request (sprintf "%M") <| function
    | _::bytes ->
        try
            let x = BitConverter.ToSingle(  List.toArray bytes, 0) |> decimal 
            Ok (abs x )
        with e ->
            Err (sprintf "%s не является числом с плавающей точкой, %A" (bytesToStr bytes) e.Message)
    | _ -> Err "неверный формат ответа стенда на запрос 31"
    

let getu addy =     
        
    match getv addy "измерение напряжения" 1uy Limit25 DC stendConfig.MeasureTensionCount stendConfig.MeasureTensionMXTime with
    | Err x -> Err x
    | Ok x ->
        let a, b = stendConfig.TensA, stendConfig.TensB 
        let r =  x * a + b
        debug "напряжение 1 - (x:%g) * (a:%g) + (b:%g) = %g" x a b r 
        Ok r

let geti addy = 
    let a,b = 
        match stendConfig.ConsI |> List.tryFind ( fun x -> x.On && x.Addy = byte addy) with 
        | Some {ValueA=a; ValueB=b; On=true} -> a,b
        | _ -> stendConfig.CurrA, stendConfig.CurrB

    let conv_i x = x * a  + b
    getv addy "измерение тока канал 2" 2uy Limit25 DC stendConfig.MeasureCurrentCount stendConfig.MeasureCurrentMXTime
    |> Result.bind( fun value1 -> 
        if value1 >= stendConfig.MinChan2 && value1 <= stendConfig.MaxChan2 then
            getv addy "измерение тока канал 3" 3uy Limit25 DC stendConfig.MeasureCurrentCount stendConfig.MeasureCurrentMXTime
            |> Result.map( fun value2 -> 
                let chan_a, chan_b = stendConfig.ChanA, stendConfig.ChanB 
                let r =  conv_i (value2 * chan_a + chan_b)
                debug "ток 1 - ( (x:%g) * (chan_a:%g) + (chan_b:%g) ) * (a:%g) + (b:%g) = %g" 
                        value2 chan_a chan_b a b r 
                r )            
        else
            let r = conv_i value1
            if comportConfig.CanLog then
                Logging.debug "ток 2 -  (x:%g) * (a:%g) + (b:%g) = %g" value1 a b r 
            Ok r )
            
    

let getRele addy = 
    let request =
        {   what = "реле"
            addy = addy
            cmd = 6uy
            data = []
            canLog = false}

    transact request (int >> intToBin) <| function
    | [_;b] -> Ok b
    | x -> Err <| sprintf "не правильный формат ответа на запрос состояния реле - %s " (%% x)

let switchPower addy on  = 
    
    let request =
        {   what = sprintf "%s питание" (if on then "включить" else "выключить")
            addy = addy
            cmd = 30uy
            data = [ (if on then 1uy else 0uy) ]
            canLog = true}
    
    let rec switch n = 
        if n=0 then None  else
            transact request (fun _ -> "") Ok |> function
                | Err x -> Some x
                | _ -> switch (n-1)
    switch 3

let setConsumption x =  
    Logging.info "расход: %M л/мин" x
    let request =
        {   what = sprintf "расход %g" x    
            addy = pneumoAddy()
            cmd = 3uy
            data = BitConverter.GetBytes( Decimal.ToSingle x) |> Array.rev 
            canLog = true}
    transact request (fun _ -> "") Ok
    |> Result.someErr

[<AutoOpen>]
module private Helpers2 = 
    let mutable is'pneumo'open = false
    
let switchPneumo x = 
    let request =
        {   what = sprintf "пневмоблок %d" x
            addy = pneumoAddy()
            cmd = 2uy
            data = [x]
            canLog = true}

    let what = sprintf "пневмоблок <- %d" x
    transact request (fun _ -> "") Ok
    |> Result.map( fun _ -> is'pneumo'open <- x <> 0xffuy )
    |> Result.someErr
    

let closePneumo() =    
    match setConsumption 0m  with
    | None -> switchPneumo 0uy
    | x -> x

let isPneumoOpen() = is'pneumo'open


