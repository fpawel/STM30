module Json.Config

open System
open System.IO

open Json.Serialization
    


let private retDummy<'a> (filename, dummy  : unit -> 'a) errorText =    
    dummy(), Some (sprintf "ошибка файла конфигурации json \"%s\": %s" filename errorText ) 

let read filename dummy =    
    let path =  IO.Path.Combine( exepath, filename)
    let retDummy = retDummy (filename, dummy)
    if IO.File.Exists path then 
        try
            match parse (IO.File.ReadAllText(path)) with
            | Ok x -> x, None
            | Err x -> retDummy x
        with e -> retDummy <| sprintf "%A" e
    else
        retDummy "файл не существует"

let write filename x' = 
    let path =  IO.Path.Combine( exepath, filename)
    let x = stringify x'
    try
        File.WriteAllText(filename, x)
        Ok ()
    with e ->
        Err <| sprintf "oшибка записи файла конфигурации json %A: %A" filename e 
