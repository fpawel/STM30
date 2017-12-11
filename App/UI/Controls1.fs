module STM30.View.Controls1

open System
open System.Windows.Forms
open System.Drawing
open System.Runtime.InteropServices

open MyWinForms.Utils

open MainWindow
open MyWinForms.TopDialog

[<AutoOpen>]
module private Helpers1 =
    let add'ctrl<'a when 'a :> Control> (x:'a) y = 
        x.Controls.Add(y)
    let (<==) x y = add'ctrl x y

    let config = AppConfig.config.View
    let party = STM30.ViewModels.Party.party

    let tooltip = new ToolTip(AutoPopDelay = 5000, InitialDelay = 1000,  ReshowDelay = 500, ShowAlways = true)

    type Column = DataGridViewColumn
    type TextColumn = DataGridViewTextBoxColumn

let setTooltip<'a when 'a :> Control > (x:'a) text = 
    tooltip.SetToolTip(x, text)

    
let mainLayer = new Panel(Parent = form, Dock = DockStyle.Fill)   

let scenaryLayer = new Panel(Parent = mainLayer, Dock = DockStyle.Fill)

let productsLayer = 
    let _ = new Splitter(Parent = mainLayer, Dock = DockStyle.Top, Height = 3, BackColor = Color.LightGray )
    let x = new Panel(Parent = mainLayer, Dock = DockStyle.Top, 
                      MinimumSize = Size(0,200),
                      MaximumSize = Size(0,1000),

                      Height = config.ProductsLayerSplitterDistance ) 
    form.Closing.Add <| fun _ ->
        config.ProductsLayerSplitterDistance <- x.Height
    x    

 
module TopBar = 
    let placeHolder = new Panel(Parent = mainLayer, Dock = DockStyle.Top, Height = 40)
    let thread2 =      
        new Panel(Parent = placeHolder, Dock = DockStyle.Fill)
    let right = new Panel(Parent = placeHolder, Dock = DockStyle.Right, AutoSize = true)

    let thread1ButtonsBar = new Panel(Parent = placeHolder, Dock = DockStyle.Left, AutoSize = true)

    let buttonTools = 
            new Button( Parent = right, Height = 40, Width = 40, Visible = true,
                        ImageList = Widgets.Icons.instance.imageList1,
                        Dock = DockStyle.Right, ImageKey = "tools")

    let buttonReport = 
            new Button( Parent = right, Height = 40, Width = 40, Visible = true,
                        ImageList = Widgets.Icons.instance.imageList1,
                        Dock = DockStyle.Right, ImageKey = "doc")
    

    let initialize = 
        setTooltip buttonReport "Индивидуальные паспорта"

        let buttonSave = new Button(Parent = thread1ButtonsBar, AutoSize = true, Dock = DockStyle.Left,
                                    Text = "Сохранить", Visible = false)
        setInfoStyle buttonSave
        setTooltip buttonSave "Сохранить изменения"
        STM30.Behaviors.Thread2.add'is'running'handler <| fun (_,is'running) ->
            thread1ButtonsBar.Visible <- not is'running

        STM30.ViewModels.Party.party.AddIsChangedHandler <| fun(_,v) -> 
            safe buttonSave <| fun () ->                
                buttonSave.Visible <- v

        buttonSave.Click.Add <| fun _ -> 
            STM30.ViewModels.Party.party.Save()
    
        let buttonStendSettings = 
            new Button( Parent = right, Height = 40, Width = 40, Visible = true,
                        ImageList = Widgets.Icons.instance.imageList1,
                        Dock = DockStyle.Right, ImageKey = "tools1")
        setTooltip buttonStendSettings "Параметры \"железа\""
        buttonStendSettings.Click.Add <| fun _ ->            
            let popup = MyWinForms.Utils.popupConfig "Параметры \"железа\"" AppConfig.config PropertySort.Alphabetical
            popup.Font <- form.Font
            popup.Show(buttonStendSettings)
        
        let buttonSettings = 
            new Button( Parent = right, Height = 40, Width = 40, Visible = true,
                        ImageList = Widgets.Icons.instance.imageList1,
                        Dock = DockStyle.Right, ImageKey = "settings")
        setTooltip buttonSettings "Параметры приложения" 
        buttonSettings.Click.Add <| fun _ ->            
            let popup = MyWinForms.Utils.popupConfig "Параметры" party PropertySort.CategorizedAlphabetical
            popup.Font <- form.Font
            popup.Show(buttonSettings)
        fun () -> ()

let TextBlockOperationLogging =    
    let x =  
        new WebBrowser(Parent = scenaryLayer, BackColor = productsLayer.BackColor, 
                       Dock = DockStyle.Fill, AllowNavigation = false, Url = null,
                       IsWebBrowserContextMenuEnabled = false, AllowWebBrowserDrop = false )
    x.DocumentCompleted.Add <| fun _ ->
        x.AllowNavigation <- false
        if  x.Document <> null && x.Document.Body <> null then 
            x.Document.Body.ScrollIntoView(false)
    x

let GridScenary = 
    let splt = new Splitter(Parent = scenaryLayer, Dock = DockStyle.Left, Width = 3, BackColor = Color.LightGray)
    let x = 
        new DataGridView( Parent = scenaryLayer, AutoGenerateColumns = false, 
                            Name = "ScenaryGridView", 
                            Dock = DockStyle.Left, 
                            Width = AppConfig.config.View.ScnDetailTextSplitterDistance,
                            MinimumSize = Size(200,0), MaximumSize = Size(1000,0),
                            ColumnHeadersHeight = 40, DataSource = STM30.Behaviors.Thread2.operations, 
                            BorderStyle = BorderStyle.None, BackgroundColor = productsLayer.BackColor  )
    form.FormClosing.Add <| fun _ ->
        AppConfig.config.View.ScnDetailTextSplitterDistance <- x.Width

    x

module private SelectedOperation = 

    let get() = 
        let xs = GridScenary.SelectedCells
        if xs.Count=0 then None else
        let x = xs.[0]
        x.OwningRow.DataBoundItem :?> STM30.ViewModels.Operations.RunOperationInfo |> Some

    let showLoggigng() = 
        match get() with
        | None -> TextBlockOperationLogging.DocumentText <- ""
        | Some x -> 
            STM30.View.LoggingHtml.set TextBlockOperationLogging x.Operation.FullName x.Logging



    
    
let private initialize1 = 
    GridScenary.SelectionChanged.Add <| fun _ ->
        SelectedOperation.showLoggigng()

    let rec h = EventHandler( fun _ _ -> 
        if GridScenary.RowCount > 0 then
            GridScenary.CurrentCell <- null
            GridScenary.CurrentCell <- GridScenary.Rows.[0].Cells.[0]
        form.Activated.RemoveHandler h )
    form.Activated.AddHandler h

    GridScenary.DataBindingComplete.Add <| fun _ ->
        if GridScenary.RowCount > 0 then
            GridScenary.CurrentCell <- GridScenary.Rows.[0].Cells.[0]

    party.AddLoggingEvent.Add <| fun (operation, level,text) ->
        match SelectedOperation.get() with            
        | Some op  -> 
            let xs = STM30.ViewModels.Operations.Operation.tree op.Operation
            if xs |> List.exists( fun o -> o.FullName = operation) then
                safe TextBlockOperationLogging <| fun () -> 
                    STM30.View.LoggingHtml.addRecord TextBlockOperationLogging level text
        | _ -> ()

    STM30.Behaviors.Thread2.PerfomOperationEvent.Add <| function
        | operation,true ->
            match SelectedOperation.get() with            
            | Some op when op.Operation.FullName = operation.FullName -> 
                safe TextBlockOperationLogging <| fun () ->
                    STM30.View.LoggingHtml.set TextBlockOperationLogging operation.FullName []
            | _ -> ()
        | _ -> ()

    let (~%%) x = x :> Column
    let col'stat = %% new TextColumn(DataPropertyName = "Status", HeaderText = "Статус")
    GridScenary.Columns.AddRange            
        [|  %% new TextColumn(DataPropertyName = "Name", HeaderText = "Операция")
            %% new TextColumn(DataPropertyName = "Delaytime", HeaderText = "Задержка") 
            col'stat  |]
    GridScenary.CellFormatting.Add <| fun e ->
        let row = GridScenary.Rows.[e.RowIndex]
        let col = GridScenary.Columns.[e.ColumnIndex]        
        let cell = row.Cells.[e.ColumnIndex]
        let i = row.DataBoundItem :?> STM30.ViewModels.Operations.RunOperationInfo

        let x = e.CellStyle
            
        if i.WasPerformed then
            x.SelectionForeColor <- SystemColors.HighlightText
            x.SelectionBackColor <- SystemColors.Highlight
            x.ForeColor <- Color.Black
            x.BackColor <- Color.Bisque

        if i.HasErrors then                
            x.SelectionForeColor <- Color.Yellow
            x.SelectionBackColor <- Color.MidnightBlue 
            x.ForeColor <- Color.Red  
            x.BackColor <- Color.LightGray 

        if i.IsPerforming then 
            x.SelectionForeColor <- SystemColors.HighlightText
            x.SelectionBackColor <- SystemColors.Highlight
            x.ForeColor <- Color.Black
            x.BackColor <- Color.Aquamarine
        e.FormattingApplied <- true

    fun () -> ()

module ProductsGrids = 
    type CheckBoxColumn = MyWinForms.GridViewCheckBoxColumn
    type TextColumn = DataGridViewTextBoxColumn
    let (<==) cols col = grid'view'add'col cols col
        
    let private createGridView name  = 
        let g = 
            new DataGridView( AutoGenerateColumns = false, 
                                BackgroundColor = productsLayer.BackColor,
                                Name = name,
                                ColumnHeadersHeight = 40, DataSource = party.Products, 
                                BorderStyle = BorderStyle.None  )
        g.Columns <== new CheckBoxColumn(DataPropertyName = "IsChecked", Width = 50) 
        g.Columns <== new TextColumn(DataPropertyName = "Addy", HeaderText = "#", Width = 50) 
        g.Columns <== new TextColumn(DataPropertyName = "Serial", HeaderText = "№", Width = 80) 
        g

    let main = createGridView "ProductsInterrogateGrid"
    let test = createGridView "ProductsTestGrid"

    let var = 
        STM30.VarType.values 
        |> List.map ( fun var -> var, createGridView (sprintf "%A_ProductsVarGrid" var)  )
        |> Map.ofList

    let grids = 
        [   yield "Показания", main 
            yield "Проверки", test 
            for KeyValue(var,g) in var do
                yield STM30.VarType.what var, g  ] 

    let getSelectedGrid = 
            
        let f = 
            grids
            |> List.map( fun (x,y) -> x, y :> Control)
            |> MyWinForms.Utils.tabsheet productsLayer
        fun () -> f() :?> DataGridView
            

let productsToolsLayer = 
    let x = new Panel(Parent = productsLayer, Dock = DockStyle.Right, Width = 40 ) 
    
    STM30.Behaviors.Thread2.add'is'running'handler <| fun (_,isRunning) ->
        x.Visible <- not isRunning

    x    


        

        
let initialize = 
    TopBar.initialize()
    initialize1()
    fun () -> ()

    


        

    