module MainWindow

open System
open System.Windows.Forms
open System.Drawing


let aboutForm = 
    let x = new Widgets.AboutForm() 
    x.LabelVersion.Text <-  
        try
            Reflection.Assembly.GetExecutingAssembly().GetName().Version.ToString()
        with _ -> 
            ""
    x.Deactivate.Add(fun _ -> 
        x.Hide()
        )
    x

let form = 
    
    let x = new Form(Font = new Font("Consolas", 12.f), WindowState = FormWindowState.Maximized )
    let path = IO.Path.Combine( exepath, "icon.ico")
    try        
        let customIcon = new Icon( path )
        x.Icon <- customIcon
    with e ->
        Logging.error "fail to set icon.ico from %A : %A" path e

    x
let safe = MyWinForms.Utils.safe

let errorMessageBox title message = 
    Logging.error "%A, %s" title message
    safe form <| fun () ->
        MessageBox.Show( message, title, MessageBoxButtons.OK, MessageBoxIcon.Error ) 
        |> ignore

let onExeption (e:Exception) = 
    Logging.error "Исключение %A" e 
    safe form <| fun () ->
        MessageBox.Show( sprintf "%A" e ,"Исключение", MessageBoxButtons.OK, MessageBoxIcon.Error ) 
        |> ignore
    System.Environment.Exit(1)
    failwith ""

open AppConfig
open AppConfig.View

do
    let get'grids() = 
        MyWinForms.Utils.enum form
            (fun x -> 
                if x.GetType()=  typeof<DataGridView>  then                     
                    Some (x :?> DataGridView) 
                else None)
            id
    form.FormClosing.Add <| fun _ -> 
        config.View.Grids <-
            get'grids()
            |> Seq.map( fun g -> 
                g.Name,
                    {   ColWidths = [for c in g.Columns -> c.Width]
                        ColumnHeaderHeight = g.ColumnHeadersHeight } )     
            |> Map.ofSeq

    let mutable h : EventHandler = null
        
    h <- EventHandler( fun _ _ -> 
        form.Activated.RemoveHandler h
        
        for g in get'grids() do 
            let dt = config.View.Grids.TryFind g.Name
            
            g.ColumnHeadersHeight <-
                match dt with
                | Some { ColumnHeaderHeight = h} -> h
                | _ -> g.ColumnHeadersHeight
                |> max (let sz = TextRenderer.MeasureText( "X", g.ColumnHeadersDefaultCellStyle.Font )
                        sz.Height + 7 )
            [for c in g.Columns -> c ] 
            |> List.iteri( fun n c -> 
                c.Width <-
                    (   match dt with            
                        | Some { ColWidths = dt } when n < dt.Length ->  dt.[n]
                        | _ -> 
                            let sz = TextRenderer.MeasureText( c.HeaderText, c.HeaderCell.Style.Font )
                            sz.Width + 10 )
                    |> max 50 )
        aboutForm.Hide()

        aboutForm.FormBorderStyle <- FormBorderStyle.FixedDialog
        aboutForm.ControlBox <- false
        aboutForm.ShowInTaskbar <- false
        aboutForm.ShowIcon <- true )

    form.Activated.AddHandler h

    aboutForm.Show()
    aboutForm.Refresh()