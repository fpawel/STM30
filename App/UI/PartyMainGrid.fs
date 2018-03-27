module STM30.View.PartyMainGrid

open System
open System.Windows.Forms
open System.Drawing


open MyWinForms.Utils

open STM30
 
open STM30.View.Controls1
open STM30.Behaviors

[<AutoOpen>]
module private Helpers =

    type P = STM30.ViewModels.Party.Product 
    type Column = DataGridViewColumn
    type TextColumn = DataGridViewTextBoxColumn
    type RelesColumn = MyWinForms.DataGridViewRelesColumn

    let party = STM30.ViewModels.Party.party
    
    let (<==) cols col = grid'view'add'col cols col

    let prodCellFormatting (p:P) (cell:DataGridViewCell) prod =
        let g = cell.DataGridView
        let col = g.Columns.[cell.ColumnIndex]
        let what = sprintf "%s, %A" (STM30.ProductionPoint.what prod) p.What
        let text,tooltip,fore,back = 
            match p.Production.TryFind prod with
            | None -> "", "не известно", Color.Black, Color.White
            | Some (dt, level, text) -> 
                (   match level with
                    | Logging.Info -> "Ок"
                    | Logging.Error -> "Ошибка"
                    | _ -> text ),  
                (sprintf "%s %s" (DateTime.toString(dt))  text),
                (Logging.foreColor level),
                (Logging.backColor level)
            
        cell.ToolTipText <- sprintf "#%d №%d, %s - %s" p.Serial p.Addy col.HeaderText tooltip      
        cell.Style <- new DataGridViewCellStyle ( ForeColor = fore, BackColor = back ) 
        text

    let (|ProductionInfoColumn|_|)  =
        let connection'info'cols = 
            STM30.ProductionPoint.values 
            |> List.map( fun prod -> 
                new  TextColumn(DataPropertyName = "Production", HeaderText = STM30.ProductionPoint.what prod, 
                                ReadOnly=true), prod )       
        
        let is'con p = p = ConnectionStend || p = ConnectionModbus        
        let xs1,xs2 = connection'info'cols |> List.partition ( snd >> is'con )
        
        xs1 |> List.map fst |> List.iter ( grid'view'add'col ProductsGrids.main.Columns ) 
        xs2 |> List.map fst |> List.iter ( grid'view'add'col ProductsGrids.test.Columns ) 

        fun (col : Column) ->
            connection'info'cols |> List.tryFind( fun (c,_) -> Object.ReferenceEquals(c,col) ) 

type I = 
    | Value of string
    | Error of string
    | No

let initialize =
    let g = ProductsGrids.test    
    g.CellFormatting.Add <| fun e ->
        let row = g.Rows.[e.RowIndex]
        let col = g.Columns.[e.ColumnIndex]        
        let cell = row.Cells.[e.ColumnIndex]
        let p = row.DataBoundItem :?> STM30.ViewModels.Party.Product
        match col with 
        | ProductionInfoColumn (_,prod) -> 
            e.Value <- prodCellFormatting p cell prod
            e.FormattingApplied <- true
        | _ -> e.FormattingApplied <- false

    let g = ProductsGrids.main 

    let (~%%) x = x :> Column
    //let confVar = STM30.Config.config.Var
    let colConc = %% new  TextColumn(DataPropertyName = "Reader", HeaderText = "Конц."), "InterrogateConc"
    let colCurr = %% new  TextColumn(DataPropertyName = "Reader", HeaderText = "I.вых."), "InterrogateCurr"
    let colTens = %% new  TextColumn(DataPropertyName = "Reader", HeaderText = "U.вых."), "InterrogateTens"
    let colRele = %% new  RelesColumn(DataPropertyName = "Reader", HeaderText = "Реле"), "InterrogateRele"
    let colStat = %% new  TextColumn(DataPropertyName = "Reader", HeaderText = "Режим"), "InterrogateMode"
    let cols1 = [   colConc; colCurr; colTens; colRele; colStat ]

    let getcolvis prop = 
        party.GetType().GetProperty(prop).GetValue(party, null) :?> bool

    cols1 |> List.iter ( fun (col,prop) -> 
        g.Columns <== col
        col.Visible <- getcolvis prop
        Runtime.PropertyChanged.add party <| fun e ->
            if e.PropertyName = prop then 
                col.Visible <- getcolvis prop )

    let (==) (x : Column) (y : Column) = Object.ReferenceEquals(x,y)

    let (|VarColumn|_|) (col : Column) =
        [ Conc, colConc; Curr, colCurr; Tens, colTens ] 
        |> List.tryFind( snd >> fst >> (==) col )
        |> Option.map( fst )

    g.CellFormatting.Add <| fun e ->
        let row = g.Rows.[e.RowIndex]
        let col = g.Columns.[e.ColumnIndex]        
        let cell = row.Cells.[e.ColumnIndex]
        let p = row.DataBoundItem :?> STM30.ViewModels.Party.Product
        let setTooltip tooltip = 
            cell.ToolTipText <- sprintf "%s #%d №%d, %s - %s" (DateTime.format "HH:mm:ss" DateTime.Now) p.Serial p.Addy col.HeaderText tooltip

        let setError error = 
            e.Value <- "Ошибка"
            setTooltip error
            cell.Style <- new DataGridViewCellStyle ( ForeColor = Color.Red, BackColor = Color.LightGray ) 
            e.FormattingApplied <- true

        let setGood (value:string) = 
            e.Value <- value
            cell.Style <- new DataGridViewCellStyle ( ForeColor = Color.Navy, BackColor = Color.Azure ) 
            setTooltip value 
            e.FormattingApplied <- true

        let setNoValue () = 
            e.Value <- ""
            setTooltip "значение не было получено"
            cell.Style <- new DataGridViewCellStyle ( ForeColor = Color.Black, BackColor = Color.White ) 
            e.FormattingApplied <- true

        match col with  
        | ProductionInfoColumn (_,prod) -> 
            e.Value <- prodCellFormatting p cell prod
            e.FormattingApplied <- true
        | VarColumn var -> 
            match p.Reader.Var.TryFind var  with 
            | None  -> setNoValue ()
            | Some (Err e) -> setError e
            | Some (Ok value) -> setGood ( value.ToString(VarType.stringFormat var) )
        | _ when col == fst colRele  ->
            match p.Reader.Rele with
            | Some( Err e ) -> setError e
            | None -> setNoValue ()
            | _ -> ()
        | _ when col == fst colStat ->
            match p.Reader.Stat with
            | Some( Err e ) -> setError e
            | Some( Ok (n1,n2,s) ) -> 
                setGood (sprintf "%s, %d, %d" s n1 n2)
            | None -> setNoValue ()

        | _ -> e.FormattingApplied <- false


    PartyBehavior.onInterrogateProduct.Value <- fun n -> 
        g.SelectedRows.Clear()
        for i = 0 to g.Rows.Count - 1 do    
            if i = n then 
                g.Rows.[i].Selected <- true
            
    fun () -> ()