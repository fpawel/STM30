module STM30.Core

open System
open System.ComponentModel

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


type VarType with    
    static member valueToConc = function
        | Conc -> id
        | Curr -> currToConc
        | Tens -> tensToConc

    static member concToValue = function
        | Conc -> id
        | Curr -> concToCurr
        | Tens -> concToTens
    
let what'tune = function
    | TuneCurr, ScaleNull -> "Iвых 4 мА"
    | TuneCurr, ScaleEnd -> "Iвых 20 мА"
    | TuneTens, ScaleNull -> "Uвых 0 В"
    | TuneTens, ScaleEnd -> "Uвых 1 В"

let productions =     
    [   yield ConnectionModbus, "MODBUS"
        yield ConnectionStend, "СТЕНД"
        yield SetPorogs, "Уст. порог."
        yield SoftVersionValue, "Вер. ПО"
        yield TestMode, "РЕЖИМ"
        yield TestFailureOn, "Вкл. ОТКАЗ"
        yield TestFailureOff, "Выкл. ОТКАЗ"
        for tn in [TuneCurr; TuneTens] do 
            for sc in [ScaleNull; ScaleEnd] do
                let x = tn,sc
                yield Tune x, what'tune x
        yield TestAdjust ScaleNull, "Калибр. нуля"
        yield TestAdjust ScaleEnd, "Калибр. чувст."]
    |> Map.ofList

type ProductionPoint with
    
    static member what'tune = function
        | TuneCurr, ScaleNull -> "Iвых 4 мА"
        | TuneCurr, ScaleEnd -> "Iвых 20 мА"
        | TuneTens, ScaleNull -> "Uвых 0 В"
        | TuneTens, ScaleEnd -> "Uвых 1 В"


    static member values = productions |> Map.toList |> List.map fst
    static member what x = 
        productions.[x]

let productTypes, saveProductTypes =  
    let values,save = Config.fromJson "productTypes" <| fun () ->  [ProductType.createNew()] 
    let values = new BindingList<ProductType>( List.toArray values ) 
    let save() = save [ for x in values -> x ]
    values,save

type Batch with
    static member productType (x:Batch) = 
        let y = productTypes |> Seq.tryFind(fun prodType -> prodType.Name=x.ProductType)
        match y with
        | Some y -> y
        | None when productTypes.Count=0 -> ProductType.createNew()
        | _ -> productTypes.[0]

   