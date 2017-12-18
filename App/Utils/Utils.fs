[<AutoOpen>]
module Utils

open System
open System.IO
open System.ComponentModel
open System.Text
open System.Collections.Generic
open System.Collections.ObjectModel
open System.Text.RegularExpressions
open System.Globalization
open Microsoft.FSharp.Reflection 

let rec exnRoot (exn:System.Exception) = 
    if exn.InnerException=null then exn else exnRoot exn.InnerException


let appDir = 
    let appDataDir = 
        Environment.GetFolderPath(Environment.SpecialFolder.ApplicationData)
    let exeName =
        Path.GetFileNameWithoutExtension(AppDomain.CurrentDomain.FriendlyName) 
    let dir = Path.Combine(appDataDir, exeName)
    if not <| Directory.Exists dir then
        let x = Directory.CreateDirectory dir
        assert x.Exists
    dir

let exepath = 
    
    try
        IO.Path.GetDirectoryName(Reflection.Assembly.GetExecutingAssembly().Location)
    with _ -> 
        Environment.CurrentDirectory

let tryGetCaseAttribute<'T,'a> (x:'a) = 
    let case,_ = FSharpValue.GetUnionFields(x, x.GetType() )
    case.GetCustomAttributes() |> 
    Seq.tryFind( fun e -> e.GetType()=typeof< 'T > ) |> 
    Option.map( fun atr -> atr :?> 'T )

let unif<'a> (x:'a) = 
    try
        match FSharpValue.GetUnionFields(x, typeof<'a>) with
        | case, _ -> 
            case.Name
    with e ->
        failwithf "Utils unif %A" e

let caseDescr<'T> (x:'T) = 
    match tryGetCaseAttribute<DescriptionAttribute,'T> x with 
    | None -> unif x
    | Some d -> d.Description

let unionCasesList<'T> =
    FSharpType.GetUnionCases typeof<'T> 
    |> Array.toList 
    |> List.map( fun case -> FSharpValue.MakeUnion(case,[||]) :?> 'T )


let bit n x = (1uy <<< n) &&& x <> 0uy 

type Dynamic = Dictionary<string,obj>

type ListBuilder() =
    member this.Bind(m, f) = 
        List.collect f m

    member this.Zero(_) = 
        []

    member this.Return(x) = 
        [x]

// make an instance of the workflow                
let listOf = new ListBuilder()

type Result<'T, 'E> = 
    | Ok of 'T
    | Err of 'E
    

module Result =
    let isErr = function
      | Err _ -> true
      | _      -> false

    let isOk = function
      | Ok _ -> true
      | _      -> false

    let map f = function
      | Ok x -> Ok( f x )
      | Err e -> Err e  

    let mapErr f = function
      | Ok x -> Ok x
      | Err e -> Err ( f e  )

    let bind f = function
      | Ok x ->  f x
      | Err e -> Err e

    let bindErr f = function
      | Ok x ->  Ok x
      | Err e -> f e

    let someErr = function
      | Ok _ ->  None
      | Err e -> Some e

    module Unwrap = 
        
        let ok = function
          | Ok x -> x
          | Err e -> failwithf "unwraping Err %A as Ok" e
        
        let err = function
          | Ok x -> failwithf "unwraping Ok %A as Err" x
          | Err e -> e

module Option = 
    let getWith x = function
        | None -> x
        | Some x' -> x'


let createDirectory x = 
    if not <| Directory.Exists x then
        let x = Directory.CreateDirectory x
        assert x.Exists

let getUniqueKey len = 
    let rec loop x = 
        let x = x + Guid.NewGuid().ToString().GetHashCode().ToString("x")
        if x.Length < len then loop x else x
    loop ""

let md5hash (input : string) =
    let md5 = Security.Cryptography.MD5.Create()    
    input
    |> System.Text.Encoding.ASCII.GetBytes
    |> md5.ComputeHash
    |> Seq.map (fun c -> c.ToString("X2"))
    |> Seq.reduce (+)

let monthByNumber n = (Globalization.CultureInfo.CurrentCulture.DateTimeFormat.GetMonthName n).ToLower()

let isValidDate (year,month,day) = 
    year >= DateTime.MinValue.Year && year <= DateTime.MaxValue.Year &&
    month > 0 && month < 13 &&
    day > 0 && day <= DateTime.DaysInMonth(year, month)


let isValidTime (h,m,s) = 
    h>(-1) && h < 24 &&
    m>(-1) && m < 60 &&
    s>(-1) && s < 60

let isValidDateTime (year,month,day,h,m,s) = 
    isValidDate (year,month,day) && isValidTime (h,m,s)


    
let tryParseDecimal s = 
    let sep = CultureInfo.CurrentCulture.NumberFormat.NumberDecimalSeparator
    match Decimal.TryParse( Regex.Replace( s, "[,\\.]", sep) ) with
    | true, v -> Some v
    | _ -> None


let listToStr<'T> delimString conv (collection : 'T seq )  = 
    collection |> Seq.fold( fun acc x ->
        acc + (if acc |> String.IsNullOrEmpty then acc else delimString) + (conv x) ) ""

let intToHex len x = 
    let x = sprintf "%X" x
    let n = String.length x
    (if n < len then String('0', len-n ) else "") + x

let bytesToStrings bytes =      
    Seq.fold ( fun (acc,i) b ->
        let s = sprintf "%s%X" (if b<0x10uy then "0" else "") b
        if i%16=0 then (s::acc, i+1) else                     
        match acc with 
        | [] -> ( [s], i+1) 
        | [s'] -> ([ s'+" "+s], i+1)
        | s'::acc  -> ((s'+" "+s)::acc, i+1)) ( [], 0 ) bytes |> fst |> List.rev

let rec intToBin i =
    match i with
    | 0 | 1 -> string i
    | _ ->
        let bit = string (i % 2)
        (intToBin (i / 2)) + bit

let bytesToStr bytes = Seq.fold ( fun acc s -> if acc="" then s else acc + " "+s) "" (bytesToStrings bytes)
let (|BytesToStr|) = bytesToStr

type TimeSpan with
    static member toString (x:TimeSpan) = 
        x.ToString( @"hh\:mm\:ss" )

type DateTime with
    static member format (spec:string) (x:DateTime) = 
        x.ToString(spec)

    static member toString x = 
        DateTime.format "HH:mm:ss" x

    static member toString1 (x:DateTime) = 
        DateTime.format "HH:mm" x

    static member toString2 (x:DateTime) = 
        DateTime.format "dd.MM.yy HH:mm" x


let (|EqualsTo|) lhs rhs = lhs=rhs
let (|RefEqualsTo|) lhs rhs = Object.ReferenceEquals(lhs,rhs)

let datePath root =
    let (~%%) = sprintf "%d"

    let year year = 
        Path.Combine(root, %% year )

    let month y month = 
        
        let smonth = (DateTime.MinValue.AddMonths (month-1)).ToString("MMM", System.Globalization.CultureInfo.InvariantCulture)
        Path.Combine(year y, sprintf "%d-%s" month smonth)

    let day y m day = 
        Path.Combine(month y m, %% day )

    let path canCreate (dt:DateTime)  =         
        let month = dt.ToString("MMM", System.Globalization.CultureInfo.InvariantCulture)
        let path = day dt.Year dt.Month dt.Day 
        if canCreate && not ( Directory.Exists path) then 
            Directory.CreateDirectory path |> ignore
        path

    path, year, month, day

let iterate n f = 
    for k=1 to n do f()

let zip2 xs ys = 
    let rec loop r xs ys =
        match xs,ys with 
        | [],_         -> r
        | _,[]         -> r
        | xh::xt,yh::yt -> loop ((xh,yh)::r) xt yt
    loop [] xs ys |> List.rev

module Seq =
    let toStr<'T> delimString conv (collection : 'T seq )  = 
        collection |> Seq.fold( fun acc x ->
            acc + (if acc |> String.IsNullOrEmpty then acc else delimString) + (conv x) ) ""
    
