module Logging

open System
open System.IO
open System.Drawing
open System.Diagnostics

type Level = 
    | Error
    | Warn
    | Info
    | Debug
    static member toString (x:Level) = 
        (sprintf "%A" x).ToUpper()

let addLogger, getLoggers = 

    let mutable loggers = []
    let addLogger logger = 
        let k = getUniqueKey 10
        loggers <- (k,logger) :: loggers
        fun () -> 
            loggers <- List.filter( fst >> ((<>) k)  ) loggers
   
    let flevel = Level.toString
    let path,_,_,_ = datePath "Log" 
    let stream = 
        let x = new StreamWriter(Path.Combine(path true DateTime.Now, "log.log"), true)
        x.AutoFlush <- true
        x
    addLogger (fun l -> fprintfn stream "%s|%s|%s" (DateTime.toString DateTime.Now) (flevel l)) |> ignore
    addLogger (fun l m -> printfn "%s|%s|%s" (DateTime.toString DateTime.Now) (flevel l) m) |> ignore


    addLogger, (fun () -> loggers)
 

        
        
let write (level :Level) format =
    let cont (message : string) =
        getLoggers() |> List.map snd |> List.iter( fun f -> f level message)

    Printf.kprintf cont format 

let info f = write Info f  
let warn f = write Warn f 
let error f = write Error f 
let debug f = write Debug f 


let foreColor = function
    | Error -> Color.Red 
    | Info -> Color.Navy
    | Debug -> Color.Gray
    | Warn -> Color.Maroon

let backColor = function
    | Error -> Color.LightGray
    | Info -> Color.Azure
    | Debug -> Color.White
    | Warn -> Color.White

module JsonConfig =

    let read filename dummy = 
        let x, e = Json.Config.read filename dummy
        match e with
        | Some e -> error "%s" e
        | _ -> ()
        x

    let write filename config = 
        match Json.Config.write filename config with
        | Ok () -> ()
        | Err e -> error "%s" e

    let create filename dummy =         
        let config = read filename dummy
        let save() = write filename config
        config, save