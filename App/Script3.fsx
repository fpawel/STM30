open System


type A = A with
    
    static member ($) (A, x : string) =
        x
    
    static member ($) (A, x : int) =
        sprintf "%d" x

    static member ($) (A, x : decimal) =
        sprintf "%M" x

let inline show x = (A $ x)

let a = show ""
let b = show 12
let c = show 12m

module X = 
    type A = A with    
        static member ($) (A, x : float) =
            sprintf "%f" x

open X

let ee = show 12.

