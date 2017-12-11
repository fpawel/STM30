module MyWinForms.Utils 

open System 
open System.Windows.Forms
open System.Drawing
open System.Reflection

let safe<'a> (ctrl:Control) (f:unit -> 'a) =
    if ctrl.InvokeRequired then 
        let mutable x : 'a option = None
        ctrl.Invoke <| new  MethodInvoker( fun () -> x <- Some (f()) ) |> ignore
        x.Value
    else f()

let timer interval onTick = 
    let mutable startTime  = DateTime.MinValue
    let timer = new Timer(Enabled = false, Interval = interval)
    //let upd _ = 
        //let elepsed = DateTime.Now - startTime
        //let v = int elepsed.TotalMilliseconds
        //textBlock.Text <- sprintf "%s - %s" (startTime.ToString("HH:mm:ss")) (TimeSpan.toString elepsed) 
      //  onTick()
    timer.Tick.Add <| fun _ ->  
        onTick startTime
    let start () = 
        startTime <- DateTime.Now
        timer.Start()        
    let stop () = 
        timer.Stop()    
    start, stop

let buttonsMenu (font:Font) maxWidth xs =
    
    let width =         
        let s =  xs |> List.map fst |> List.maxBy( String.length ) 
        let sz = TextRenderer.MeasureText( s, font, Size( Int32.MaxValue, Int32.MaxValue))
        match maxWidth with 
        | Some maxWidth -> min (sz.Width + 10) maxWidth 
        | _ -> Int32.MaxValue

    
    
        
    let p = new Panel(Font = font, Width = width + 10, Height = 3 ) 
    
    xs 
    |> List.map( fun (text,f) -> 
        let h = 
            let sz = TextRenderer.MeasureText( text, font, Size( width, Int32.MaxValue), TextFormatFlags.WordBreak)
            sz.Height
        
        let b = new Button(Parent = p, Width = width, Height = h + 18, Left = 5,
                           Text = text, TextAlign = ContentAlignment.MiddleLeft)        
        b.Click.Add <| fun _ ->  f b 
        b )
    |> List.iter( fun b -> 
        b.Top <- p.Height
        p.Height <- p.Height + b.Height + 3 )
    let popup = new MyWinForms.Popup(p)
    popup

let setInfoStyle (x:Control) = 
    x.BackColor <- Color.Navy
    x.ForeColor <- Color.White
    x.MouseEnter.Add <| fun _ ->  x.BackColor <- Color.DodgerBlue
    x.MouseLeave.Add <| fun _ ->  x.BackColor <- Color.Navy

let path  = 
    let rec loop (x:Control) =seq{
        yield x
        if x.Parent<>null then             
            yield! loop x.Parent }
    loop

let index<'a when 'a :> Control > (c:'a) = 
    let t = c.GetType()
    if c.Parent=null then t.Name else
    sprintf "%s%d" t.Name (c.Parent.Controls.IndexOf c) 


let enum =
    let rec loop (x:Control) chooser mapper = seq{
        match chooser x with
        | None -> ()
        | Some y -> 
            yield mapper y
        for x in x.Controls do
            yield! loop x chooser mapper }
    loop 

let mapIndexes c chooser pathMapper mapper  = enum c chooser (fun x -> 
    mapper x ( Seq.map index (path x) |> pathMapper ) )

let grid'view'add'col<'a when 'a :> DataGridViewColumn > (cols:DataGridViewColumnCollection) (col:'a)  =
    cols.Add col |> ignore

let updateGridDataSourceBinding (g:DataGridView) = 
    let d = g.DataSource
    g.DataSource <- null
    g.DataSource <- d


let isPropGridEditing (g:PropertyGrid) = 
    let gridView = 
        g.GetType()
            .GetField("gridView", BindingFlags.Instance ||| BindingFlags.NonPublic)
            .GetValue(g)
        :?> Control
    let edit = 
        gridView.GetType()
            .GetField("edit", BindingFlags.Instance ||| BindingFlags.NonPublic)
            .GetValue(gridView)
        :?> Control;
    let dropDownHolder = 
        gridView.GetType()
            .GetField("dropDownHolder", BindingFlags.Instance ||| BindingFlags.NonPublic)
            .GetValue(gridView)
        :?> Control;
    ((edit <> null) && (edit.Visible && edit.Focused)) || 
    ((dropDownHolder <> null) && (dropDownHolder.Visible))


let popupConfig title selectedObject propertySort = 
    let p = new Panel(Width = 400, Height = 600 )
    let g = new PropertyGrid(Parent = p, SelectedObject = selectedObject, Dock = DockStyle.Fill,
                             ToolbarVisible = false,
                             PropertySort = propertySort)
    let l = new Label(Parent = p,  Dock = DockStyle.Top, Text = title, TextAlign = ContentAlignment.MiddleLeft)
    setInfoStyle l
    let popup = new MyWinForms.Popup(p, Resizable = true)
    popup.Closing.Add <| fun e ->
        if isPropGridEditing g then
            e.Cancel <- true
    popup


let tabsheet parent items = 
    let mutable activeControl : Control = null

    let mainplaceholder = new Panel(Parent = parent, Dock = DockStyle.Fill)
    
    let rightplaceholder = new Panel(Parent = mainplaceholder, Dock = DockStyle.Fill)

    let _ = new Panel(Parent = mainplaceholder, Dock = DockStyle.Left, Width = 3)
    let leftplaceholder = new Panel(Parent = mainplaceholder, Dock = DockStyle.Left, Width = 100)
    let _ = new Panel(Parent = mainplaceholder, Dock = DockStyle.Left, Width = 3)
    items 
    |> List.rev 
    |> List.map ( fun (text, (c:Control) ) -> 
        let b = new RadioButton( Parent = leftplaceholder, Dock = DockStyle.Top,
                                    TextAlign = ContentAlignment.MiddleLeft,
                                    Text = text, AutoSize=true, Appearance = Appearance.Button)
        let _ = new Panel(Parent = leftplaceholder, Dock = DockStyle.Top, Height = 3)

        let contph = 
            let x = new Panel(Dock = DockStyle.Fill)
            c.Parent <- x
            c.Dock <- DockStyle.Fill
            let _ = new Panel(Parent = x, Dock = DockStyle.Top, Height = 3)
            let l = new Label(Parent = x, Text = text, Dock = DockStyle.Top, 
                              Height = 20, TextAlign = ContentAlignment.MiddleLeft)
            let _ = new Panel(Parent = x, Dock = DockStyle.Top, Height = 3)
            setInfoStyle l

            x

        b.CheckedChanged.Add <| fun _ ->                
            if b.Checked then 
                if rightplaceholder.Controls.Count > 0 then
                    rightplaceholder.Controls.RemoveAt(0) |> ignore
                activeControl <- c
                contph.Parent <- rightplaceholder
                contph.Dock <- DockStyle.Fill
        b.FlatStyle <- FlatStyle.Flat
        b )
    |> List.rev
    |> List.head 
    |> fun x -> x.Checked <- true

    fun () -> activeControl


let colorFromString x = ColorTranslator.FromHtml(x)
let MidleAqua = colorFromString "#569CD6"
let MidleGreen = colorFromString "#84BB96"