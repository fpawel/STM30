module STM30.Alchemy  


type Color = System.Drawing.Color
open MyWinForms.Color

let decToStr (specifier:string) (value:decimal) = value.ToString(specifier)

type ValueError = 
    {   Value : decimal 
        VarType : VarType
        Nominal : decimal
        Limit  : decimal }

    

        

    member x.ValueNominal =
        VarType.concToValue x.VarType x.Nominal
    
    member x.Conc = VarType.valueToConc x.VarType x.Value

    member x.Error = x.Conc - x.Nominal
    member x.ValueError =
        x.Error |> VarType.concToValue x.VarType
        

    member x.ValueLimit = 
        VarType.concToValue x.VarType x.Limit

    member x.IsError = abs x.Error >= abs x.Limit 
        

    member x.Foreground = if x.IsError then Color.Red else Color.Navy

    member x.ReportBackground = 
        if x.IsError then Color.LightGray else Color.White

    member x.What = ValueError.what x

    static member what x = 
        if x.VarType = Conc then
            sprintf "%M, номинал %M, погрешность %M, предел погрешности %M" x.Value x.Nominal x.Error x.Limit
        else
            sprintf "%M (%M), номинал %M (%M), погрешность %M (%M), предел погрешности %M (%M)" 
                    x.Value x.Conc 
                    x.ValueNominal x.Nominal 
                    x.ValueError x.Error 
                    x.ValueLimit x.Limit

    static member createNew varType value nominal limit  =         
        {   Value = value
            VarType = varType
            Nominal = nominal
            Limit = limit  }

    static member isError (x :ValueError) = x.IsError

type ReleError =
    {   Value : bool
        ValidValue : bool }    
    
    member x.IsError = ReleError.isError x
    member x.IsValid = ReleError.isValid x
    member x.Foreground = if x.IsError then Color.Yellow else MidleGreen
    
    static member isValid x = ( x.ValidValue = x.Value )
    static member isError x = ( x.ValidValue <> x.Value )
    
module private Helpers2 =
    let main b var gas value = 
        let pgs = Batch.ptGasConc gas b
        let limit = 5m 
        ValueError.createNew var value pgs limit

    let adjust b value = 
        let t = Batch.productType b   
        let pgs = Batch.pgsConc Gas3 b
        let limit = 5m * 0.2m
        ValueError.createNew Conc value pgs limit 

    let rele gas state = 
        Rele.values 
        |> List.map( fun rele -> 
            rele, {  ValidValue = Rele.validState ( PtGas.gas gas) rele; Value = Set.contains rele state}  )
        |> Map.ofList

let main b p = 
    listOf{
        let! var = VarType.values
        let! gas = PtGas.values
        match Product.getMain var gas p with
        | None -> ()
        | Some value -> 
            return (var,gas), Helpers2.main b var gas value } 
    |> Map.ofList
    |> fun m -> (fun var gas -> Map.tryFind (var,gas) m)

let adjust b p = 
    let t = Batch.productType b   
    match p.Adjust with
    | None -> None
    | Some value -> Some <| Helpers2.adjust b value 

let variation p =
    match p.Main.TryFind (Conc,PtGas2), p.Main.TryFind (Conc,PtGas4) with
    | Some c2, Some c4 -> 
        let limit = 5m * 0.5m
        Some ( ValueError.createNew Conc (c2 - c4) 0m limit )
    | _ -> None 

let rele p = 
    p.ReleMain 
    |> Map.map Helpers2.rele
    |> fun m ->
        fun gas ->
            match m.TryFind gas with
            | None -> None
            | Some m -> Some (fun rele -> m.[rele] )