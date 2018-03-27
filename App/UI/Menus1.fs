module STM30.View.Menus1

open System
open System.Windows.Forms
open System.Drawing
open System.Collections.Generic
open System.ComponentModel
open System.ComponentModel.DataAnnotations

open STM30.ViewModels.Operations
open STM30.Behaviors.Thread2
open STM30.Behaviors.PartyBehavior
open STM30.Behaviors.Work
open MyWinForms.TopDialog



[<AutoOpen>]
module private Helpers =
    type Operation = STM30.ViewModels.Operations.Operation
    type P = STM30.ViewModels.Party.Product     
    let safe = MyWinForms.Utils.safe
    let party = STM30.ViewModels.Party.party
    let popupDialog = MyWinForms.PopupDialog.create
    type Dlg = MyWinForms.PopupDialog.Options
    let none _ = None
    let form = MainWindow.form


    let getSelectedProducts() = 
        let gv = Controls1.ProductsGrids.getSelectedGrid()

        seq{ for x in gv.SelectedCells -> x.RowIndex, x.OwningRow }
        |> Map.ofSeq
        |> Map.toList
        |> List.map( fun (_,x) -> x.DataBoundItem :?> STM30.ViewModels.Party.Product )

    let simpleMenu = MyWinForms.Utils.buttonsMenu (new Font("Consolas", 12.f)) ( Some 300 )

    

[<TypeConverter(typeof<ExpandableObjectConverter>)>]
type PartyInfo = 
    {   [<DisplayName("Наименование")>]    
        [<Description("Наименование партии")>]
        mutable Name : string

        [<DisplayName("Исполнение")>]    
        [<Description("Исполнение приборов партии")>]
        [<TypeConverter (typeof<STM30.ViewModels.Party.ProductTypesConverter>) >]
        mutable ProductType : string

        [<DisplayName("ПГС1")>]    
        [<Description("Концентрация ПГС1, начало шкалы")>]
        mutable PGS1  : decimal
        [<DisplayName("ПГС2")>]    
        [<Description("Концентрация ПГС2, середина шкалы")>]
        mutable PGS2  : decimal
        [<DisplayName("ПГС3")>]    
        [<Description("Концентрация ПГС3, конец шкалы")>]
        mutable PGS3  : decimal 

        [<DisplayName("Количество приборов")>]    
        [<Description("Количество приборов в партии")>]
        mutable Count  : byte  }

let addProducts (b:Button) = 
    let tb = new TextBox(Width = 200, Dock = DockStyle.Left, Text = "1")
    let getValue() = 
        let b,v = Int32.TryParse tb.Text
        if b  && v > 0 && v<21 then
            Some v
        else
            None
    tb.MouseWheel.Add <| fun e ->
        let b,v = Int32.TryParse tb.Text
        if b then
            let d = if e.Delta>0 then 1 else (-1)
            if v<>0 || d>0 then
                tb.Text <- sprintf "%d" <| v + d  
    let errorProvider = 
        new ErrorProvider( BlinkStyle = ErrorBlinkStyle.NeverBlink )
    errorProvider.SetIconAlignment (tb, ErrorIconAlignment.MiddleRight)
    errorProvider.SetIconPadding (tb, 2)
    tb.Validating.Add <| fun e ->
        match getValue() with
        | Some _ -> errorProvider.Clear()                
        | None ->
            errorProvider.SetError(tb, sprintf "Введено не пралильное значение количества добавляемых в партию приборов %A. Пожалуйста, введите число от 1 до 20" tb.Text )
            e.Cancel <- true

    let dialog,validate  =             
        popupDialog
            { Dlg.def() with 
                Dlg.Text = Some "Введите количество добавляемых в партию приборов от 1 до 20" 
                Dlg.ButtonAcceptText = "Добавить" 
                Dlg.Title = "Добавить приборы"
                Width = 300
                Dlg.Content = 
                    let p = new Panel(Height = tb.Height + 10)
                    p.Controls.Add(tb)
                    p }
            getValue 
            ( fun value -> 
                iterate value party.AddNewProduct )
    tb.TextChanged.Add <| fun _ -> validate()
    dialog.Show(b)

let deleteProducts (b:Button) =
    let delete() =  
        getSelectedProducts()
        |> List.iter party.DeleteProduct      
    let dialog, _  =            
        popupDialog
            { Dlg.def() with 
                Dlg.Text = 
                    getSelectedProducts() 
                    |> listToStr ", " (fun x -> sprintf "#%d №%d" x.Addy x.Serial)
                    |> sprintf "Подтвердите необходимость удаления приборов %s" 
                    |> Some
                Dlg.ButtonAcceptText = "Удалить" 
                Dlg.Title = "Удалить приборы" }
            ( fun () -> Some () )
            delete
    dialog.Show(b)



let createNewParty (b:Button) = 
    let d = 
        {   Name = "Партия СТМ-30"
            ProductType = "СТМ-30-"
            PGS1 = 0m
            PGS2 = 49m
            PGS3 = 98m 
            Count = 1uy}
    let g = new PropertyGrid(SelectedObject = d, 
                                ToolbarVisible = false, Height = 250,
                                PropertySort = PropertySort.Alphabetical)
    let popup1,_ = 
        popupDialog
            { Dlg.def() with 
                Text = None 
                Content = g
                ButtonAcceptText = "Создать новую партию"
                Title = "Создать новую партию"
                Width = 400 }
            ( fun () -> Some () )
            ( fun () ->
                let b = STM30.Batch.createNew1 d.Name d.ProductType d.PGS1 d.PGS2 d.PGS3 d.Count
                STM30.Repository.Batch.save b
                party.SetBatch b
                MyWinForms.Utils.updateGridDataSourceBinding Controls1.GridScenary )
    popup1.Closing.Add <| fun e ->
        if MyWinForms.Utils.isPropGridEditing g then
            e.Cancel <- true
    popup1.Show(b)

let toolsMenu() =
    let rec clearLoggigng(b:Button) = 
        let d,_ = 
            popupDialog
                { Dlg.def() with 
                    Dlg.Text = Some  "Подтвердите необходимость очистки журнала выполнения сценария." 
                    Dlg.ButtonAcceptText = "Очистить" 
                    Dlg.Title = "Очистка журнала"
                    Width = 300 }
                ( fun () -> Some () )
                ( fun () ->
                    popup.Close()
                    party.ClearLoggigng() 
                    let g = Controls1.GridScenary
                    let d = g.DataSource
                    g.DataSource <- null
                    g.DataSource <- d )
        d.Show(b)

    and popup : MyWinForms.Popup = 
        [   yield "Очистить журнал", fun b ->
                clearLoggigng(b)
            yield "Редактировать таблицу исполнений", fun _ ->
                popup.Close()
                STM30.View.ProductTypesEdit.show()
            yield "Перейти в каталог приложения", fun _ ->
                Diagnostics.Process.Start(appDir) 
                |> ignore
            
            ]
        |> simpleMenu
    popup

[<AutoOpen>]
module private Helpers3 =
    let (<|>) what f = 
        Operation.CreateSingle (what, none) f 
    let ( -->> ) s f =
        s <|> f
        |> run (false)


let hardwareTools = 
    
    let rec popup : MyWinForms.Popup = 
        [   yield "Установка адреса", fun (btn : Button) ->
                let tb = new TextBox(Width = 290, Text = (party.GetNewValidAddy() |> string) )
                    
                let dialog,validate  = 
                    popupDialog 
                        { Dlg.def() with 
                            Text = Some "Введите адрес MODBUS от 1 до 127" 
                            ButtonAcceptText = "Установить адрес" 
                            Title = "Установка адреса MODBUS"
                            Width = 300
                            Content = tb }
                        ( fun () ->
                            let b,v = Byte.TryParse tb.Text
                            if b  && v > 0uy && v<128uy then Some v else None)
                        ( fun value ->  
                            popup.Hide()
                            sprintf "Установка адреса %d" value -->> fun () -> maybeErr{
                                do! Mdbs.testPort()
                                do! Mdbs16.write 0uy Mdbs16.SetAddy ( decimal value )
                                return!  Mdbs.read3decimal (byte value) 0 |> Result.someErr } ) 
                tb.TextChanged.Add <| fun _ -> validate()                        
                dialog.Show btn

            yield "Стенд: включить питание", fun _ -> swithcPower true
            yield "Стенд: выключить питание", fun _ -> swithcPower false

            yield!
                Mdbs16.Cmd.values 
                |> List.filter( (<>) Mdbs16.SetAddy ) 
                |> List.map( fun cmd -> 
                    (sprintf "MDBUS: %s" cmd.What), fun (btn : Button) ->                         
                        let tb = new TextBox( Width = 290, Text = "0" )
                        let dialog, validate =      
                            popupDialog        
                                { Dlg.def() with 
                                    Dlg.Text = 
                                        sprintf "Пожалуйста, введите значение аргумента команды %A" cmd.What
                                        |> Some
                                    Dlg.ButtonAcceptText = "Отправить" 
                                    Dlg.Title = cmd.What
                                    Width = 300
                                    Dlg.Content = tb }
                                (fun () -> 
                                    let b,v = Decimal.TryParse tb.Text
                                    if b  then Some v else None)
                                (fun value -> 
                                    popup.Hide()
                                    sprintf "Отправка команды МОДБАС %A, %M" cmd.What value -->> fun () -> 
                                        writeParty cmd value )                                 
                        tb.TextChanged.Add <| fun _ -> validate()
                        dialog.Show(btn) ) ]
        |> MyWinForms.Utils.buttonsMenu (new Font("Consolas", 12.f)) ( Some 300 )

    and swithcPower x = 
        sprintf "В%sключить питание" (if x then "" else "ы") -->> fun () -> maybeErr{    
            do! Stend.testPort()
            doWithProducts <| fun p ->
                Stend.switchPower p.Addy x 
                |> ignore } 
    popup    

let pneumo = 
    let rec ( -->> ) s f =
        s <|> f        
        |> fun x ->
            popup.Hide() 
            run (false) x

    and swithcGas (x:STM30.Gas) = 
        x.What -->> fun () -> 
            maybeErr{    
                do! Stend.testPort()
                return! switchGas x } 

    and popup : MyWinForms.Popup = 
        let createDialog title prompt f = 
            let tb = new TextBox(Width = 290, Text = "" )
            let dialog,validate  = 
                popupDialog 
                    { Dlg.def() with 
                        Text = Some prompt
                        ButtonAcceptText = "Применить" 
                        Title = title
                        Width = 300
                        Content = tb }
                    ( fun () ->
                        let b,v = Decimal.TryParse tb.Text
                        if b  then Some v else None)
                    ( fun value ->  
                        popup.Hide()
                        sprintf "%s, %M" title value -->> fun () -> 
                            f value ) 
            tb.TextChanged.Add <| fun _ -> validate()
            dialog
        [   yield! STM30.Gas.values |> List.map ( fun gas -> 
                STM30.Gas.what gas, fun _ -> swithcGas gas )
            yield "Выкл.", fun _ -> "Откл. пневм." -->> fun () -> maybeErr{    
                do! Stend.testPort()
                do! Stend.setConsumption 0m 
                return! Stend.switchPneumo 0uy }
            yield "Расход...", fun (b:Button) -> 
                let d = createDialog "Задать расход газа" "Задайте значение расхода газа" <| fun value -> maybeErr{    
                        do! Stend.testPort()
                        return! Stend.setConsumption value  } 
                d.Show b ]
        |> MyWinForms.Utils.buttonsMenu (new Font("Consolas", 12.f)) ( Some 300 )
    popup

open STM30.View.Controls1.TopBar
open STM30.Behaviors

let selectScenaryDialog(popupTarget:Button) = 
    let tv = 
        new MyWinForms.TriStateCheckTreeView.TriStateCheckTreeView
                (Font = new Font( "Segue UI", 12.f ),
                    Height = 600)
    let mp = Dictionary<TreeNode,Operation>()
    let ndmp = Dictionary<string,TreeNode>()
    let rec populate (parnd:TreeNode) (x:Operation) = 
        let nd = (if parnd=null then tv.Nodes else parnd.Nodes).Add(x.Name)
        nd.Checked <- true
        mp.[nd] <- x
        ndmp.[x.FullName] <- nd
        match x with 
        | Operation.Scenary (_,xs) -> xs |> List.iter (populate nd)                
        | _ -> ()
                
    populate null STM30.Behaviors.Work.main
            
    let getOperation() = 
        STM30.Behaviors.Work.main |> Operation.Choose1 ( fun y -> 
            let n = ndmp.[y.FullName].StateImageIndex
            n > 0 )

    let dialog,validate  = 
        popupDialog
            { Dlg.def() with 
                Dlg.ButtonAcceptText = "Выбрать" 
                Width = 450
                Dlg.Content = tv 
                Dlg.Title = "Выбрать сценарий"}
            getOperation
            ( fun operation ->
                STM30.Behaviors.Thread2.scenary.Set operation )

    tv.StateImageIndexChanged.Add <| fun _ ->
        validate()
    
    tv.Nodes.[0].Checked <- false   
    tv.Nodes.[0].Checked <- true   
    tv.ExpandAll()
    dialog.Show(popupTarget)
    tv.Select()
    tv.Focus() |> ignore
    validate()

let initialize =
    
    let buttonRun = new Button( Parent = Controls1.TopBar.thread1ButtonsBar, Dock = DockStyle.Left, AutoSize = true,
                                ImageKey = "run",
                                Text = "Выполнить",
                                ImageAlign = ContentAlignment.MiddleLeft,
                                TextImageRelation = TextImageRelation.ImageBeforeText,
                                TextAlign = ContentAlignment.MiddleCenter,
                                ImageList = Widgets.Icons.instance.imageList1)
    Controls1.setTooltip buttonRun ("Выполнить " + buttonRun.Text)
    buttonRun.Click.Add <| fun _ ->  
        STM30.Behaviors.Thread2.run true STM30.Behaviors.Thread2.scenary.Value
    STM30.Behaviors.Thread2.scenary.AddChanged <| fun (_,x) ->
        Controls1.setTooltip buttonRun ("Выполнить " + buttonRun.Text)
        buttonRun.AutoSize <- false
        buttonRun.AutoSize <- true

    let (<==) (text,tooltip) f = 
        let b = new Button( Parent = Controls1.TopBar.thread1ButtonsBar, Dock = DockStyle.Left, Text = text, AutoSize = true )
        b.Click.Add <| fun _ ->  f b    
        Controls1.setTooltip b tooltip
        
    do 
        let s1 = "Опрос","Опрос выбранных параметров приборов партии - концентрация, ток, напряжение, состояние контактов реле, режим"    
        s1 <== fun _ ->
            Work.runInterrogate()
    ("Адрес", "Отправить широковещательную комманду MODBUS установки адреса") <== fun x ->    
        
        let newAddr = 
            let xs = STM30.View.Controls1.ProductsGrids.main.SelectedCells
            if xs.Count = 0 then party.GetNewValidAddy() else party.Products.[xs.[0].RowIndex].Addy

        let tb = new TextBox(Width = 290, Text = string newAddr )                    
        let dialog,validate  = 
            popupDialog 
                { Dlg.def() with 
                    Text = Some "Ведите адрес MODBUS от 1 до 127" 
                    ButtonAcceptText = "Установить адрес" 
                    Title = "Установка адреса MODBUS"
                    Width = 300
                    Content = tb }
                ( fun () ->
                    let b,v = Byte.TryParse tb.Text
                    if b  && v > 0uy && v<128uy then Some v else None)
                ( fun value ->  
                    sprintf "Установка адреса %A" value -->> fun () -> maybeErr{
                        do! Mdbs.testPort()
                        do! Mdbs16.write 0uy Mdbs16.SetAddy ( decimal value )
                        return!  Mdbs.read3decimal (byte value) 0 |> Result.someErr } ) 
        tb.TextChanged.Add <| fun _ -> validate()                        
        dialog.Show x
    
    ("Упр.", "Ручное управление приборами и оборудованием") <== fun x ->
        hardwareTools.Show x

    ("Пневм.", "Ручное управление пневмоблоком стенда") <== fun x ->
        pneumo.Show x   

    do
        let x = 
            new Button( Parent = Controls1.TopBar.thread1ButtonsBar, Dock = DockStyle.Left, AutoSize = true,
                        ImageKey = "three_lines", Width = 40, Height = 40,
                        ImageList = Widgets.Icons.instance.imageList1)
        Controls1.setTooltip x "Выбрать сценарий настройки"
        x.Click.Add <| fun _ ->  
            selectScenaryDialog x

    

    let imgbtn1 key tooltip f1 f = 
        let x = 
            new Button( Parent = Controls1.productsToolsLayer, Dock = DockStyle.Top, AutoSize = true,
                        ImageKey = key, Width = 40, Height = 40,
                        ImageList = Widgets.Icons.instance.imageList1)
        f1 x
        Controls1.setTooltip x tooltip
        x.Click.Add <| fun _ ->  
            f x

    let imgbtn key tooltip f = imgbtn1 key tooltip (fun _ -> ()) f 

    imgbtn1 "removeitem" "Удалить выбранные приборы из партии" 
        ( fun b ->
            b.Visible <- false
            Controls1.ProductsGrids.grids
            |> List.iter( fun (_,g) -> 
                g.SelectionChanged.Add <| fun _ ->
                    if Controls1.ProductsGrids.getSelectedGrid() = g then
                        b.Visible <- g.SelectedCells.Count > 0 ) )
        deleteProducts
            
    
    imgbtn "additem" "Добавить в партию новые приборы" addProducts

    imgbtn "open" "Открыть ранее сохранённую партию" (fun b -> OpenPartyDialog.showDialog b )

    imgbtn "add" "Создать новую партию" createNewParty

    Controls1.TopBar.buttonTools.Click.Add <| fun _ ->
        toolsMenu().Show(Controls1.TopBar.buttonTools)
        
    fun () -> ()