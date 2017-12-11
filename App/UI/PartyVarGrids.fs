module STM30.View.PartyVarGrids

open System
open System.Windows.Forms
open System.Drawing
open MyWinForms.Utils

 

[<AutoOpen>]
module private Helpers =
    
    type P = STM30.ViewModels.Party.Product 
    type VE = STM30.Alchemy.ValueError
    let (<==) cols col = grid'view'add'col cols col 
    
    let get'product (g:DataGridView) (e:DataGridViewCellFormattingEventArgs) =
        g.Rows.[e.RowIndex].DataBoundItem :?> P

    type TextCol = DataGridViewTextBoxColumn
    let decToStr (specifier:string) (value:decimal) = value.ToString(specifier)
    let decOptToStr specifier value = 
        match value with
        | None -> ""
        | Some value -> decToStr specifier value

    let format'cell var (g:DataGridView) (e:DataGridViewCellFormattingEventArgs) ve =  
        let row = g.Rows.[e.RowIndex]
        let col = g.Columns.[e.ColumnIndex]        
        let cell = row.Cells.[e.ColumnIndex]
        let p = get'product g e
        let what = sprintf "%s, %s" p.What (STM30.VarType.what var)        
        let whatVar = STM30.VarType.what var
        let specifier = (STM30.VarType.stringFormat var)
        let decOptToStr = decOptToStr specifier
        let decToStr = decToStr specifier
        ve |> Option.map( fun (ve : VE) ->  
            let foreColor, backColor = if ve.IsError then Color.Red, Color.LightGray else Color.Navy, Color.Azure 
            let toolTip = 
                [|  yield "Концентрация", decToStr ve.Conc 
                    yield "Погрешность", decToStr ve.Error 
                    yield "Номинал", decToStr ve.Nominal
                    yield "Предел погрешности", decToStr ve.Limit  
                    if var<>STM30.Conc then
                        yield sprintf "Погрешность, %s" whatVar,  decToStr ve.ValueError 
                        yield sprintf "Номинал, %s" whatVar,  decToStr ve.ValueNominal 
                        yield sprintf "Предел погрешности, %s" whatVar,  decToStr ve.ValueLimit |]
                |> Array.map( fun (p,v) -> sprintf "%s = %s" p v)
                |> fun v -> String.Join("\n", v)
            ve.Value, foreColor, backColor, toolTip  )
        |> function
        | None -> cell.ToolTipText <- sprintf "%s - нет данных" what
        | Some (value, foreColor, backColor, text) ->
            cell.Style.ForeColor <- foreColor
            cell.Style.BackColor <- backColor
            cell.ToolTipText <- sprintf "%s\n%s" what text


let initialize = 
    STM30.VarType.values |> List.iter ( fun var -> 

    let specifier = (STM30.VarType.stringFormat var)
    let decToStr = decToStr specifier
    let decOptToStr = decOptToStr specifier
    
    let g = STM30.View.Controls1.ProductsGrids.var.[var]
    let cols = g.Columns
    let isConc = var = STM30.Conc
    
    
    let isFormatingColumn (e:DataGridViewCellFormattingEventArgs) (col:DataGridViewColumn) =
        Object.ReferenceEquals( g.Columns.[e.ColumnIndex], col)

    let format'cell = format'cell var g

    if isConc then
        let col = new TextCol(   DataPropertyName = "Adjust", HeaderText = "Калибр." )
        cols <== col
        g.CellFormatting.Add <| fun e ->
            if isFormatingColumn e col then
                let p = get'product g e
                e.Value <- decOptToStr p.Adjust
                format'cell e p.AlchemyAdjust
        g.CellParsing.Add <| fun e ->             
            if g.Columns.[e.ColumnIndex].GetHashCode() = col.GetHashCode() then                
                let p = g.Rows.[e.RowIndex].DataBoundItem :?> P  
                let v =  e.Value |> string  |> tryParseDecimal
                p.Adjust <- v 
                e.Value <- v
                e.ParsingApplied <- true                    

    let mcols = 
        [   for ptgas in STM30.PtGas.values do
                let col = 
                    new TextCol ( DataPropertyName = "Main", HeaderText = STM30.PtGas.what ptgas )
                g.Columns.Add col  |> ignore
                yield col.GetHashCode(), ptgas ]
        |> Map.ofList

    let getCol (e:DataGridViewCellFormattingEventArgs) = mcols.TryFind (g.Columns.[e.ColumnIndex].GetHashCode())
    g.CellFormatting.Add <| fun e ->
        mcols.TryFind (cols.[e.ColumnIndex].GetHashCode())
        |> Option.map( fun gas ->  
            let p = get'product g e
            e.Value <- decOptToStr (p.Main.TryFind (var,gas) )
            format'cell e (p.AlchemyMain var gas)
            e.FormattingApplied <- true )
        |> ignore
           

    g.CellParsing.Add <| fun e ->             
        match mcols.TryFind (g.Columns.[e.ColumnIndex].GetHashCode()) with
        | None -> ()
        | Some gas ->
            let p = g.Rows.[e.RowIndex].DataBoundItem :?> P  
            let v =  e.Value |> string  |> tryParseDecimal
            p.SetMain var gas v    
            e.Value <- p.Main                
            e.ParsingApplied <- true

    if isConc then
        let col =
            new TextCol
                (   DataPropertyName = "AlchemyVariation",
                    HeaderText = "Вариац." )
        g.Columns.Add col  |> ignore
        g.CellFormatting.Add <| fun e ->
            if Object.ReferenceEquals( g.Columns.[e.ColumnIndex], col) then
                let p = get'product g e
                e.Value <- match p.AlchemyVariation with Some v -> decToStr v.Value | _ -> ""
                format'cell e p.AlchemyVariation )
    fun () -> ()
    
