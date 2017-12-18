module STM30.View.ProductTypesEdit

open System
open System.Windows.Forms
open System.Drawing

open STM30
open STM30.View

[<AutoOpen>]
module private Helpers =     

    let popupDialog = MyWinForms.PopupDialog.create
    type Dlg = MyWinForms.PopupDialog.Options

    type Dck = DockStyle
    

let private create() = 
    let source = STM30.ProductTypes.values
    let party = STM30.ViewModels.Party.party

    Controls1.mainLayer.Visible <- false
    let placeHolder = new Panel(Parent = MainWindow.form, Dock = Dck.Fill)

    let g = 
        new DataGridView
            (   AutoGenerateColumns = false, 
                Parent = placeHolder, BackColor = MainWindow.form.BackColor,
                Dock = Dck.Fill, 
                ColumnHeadersHeight = 30,
                BorderStyle = BorderStyle.None,
                DataSource = source)
    

    let toolbar = new Panel(Parent = placeHolder, Dock = Dck.Top, Height = 40)

    let (~%%) k = 
        let x = new Button(Parent = toolbar, Height = 40, Width = 40, 
                           ImageList = Widgets.Icons.instance.imageList1,
                           Dock = Dck.Right, ImageKey = k)
        let _ = new Panel(Parent = toolbar, Dock = Dck.Right, Width = 5)
        x

    let getSelectedProductTypes() =     
        seq{ for x in g.SelectedCells -> x.RowIndex, x.OwningRow }
        |> Map.ofSeq
        |> Map.toList
        |> List.map( fun (_,x) -> x.DataBoundItem :?> STM30.ProductType )

    [   "Name", "Исполнение"
        "NormativeDoc", "ИБЯЛ"
         ]
    |> List.iter( fun (pth,hdr) -> 
        (   new DataGridViewTextBoxColumn ( DataPropertyName = pth, HeaderText = hdr, Width = 300 ) )
        |> g.Columns.Add 
        |> ignore ) 

    let btnAdd = %% "additem"
    
    Controls1.setTooltip btnAdd "Создать новое исполнение" 
    btnAdd.Click.Add <| fun _ ->
        let popup,_ = 
            popupDialog
                { Dlg.def() with 
                    Dlg.Text = Some  "Пожалуйста, подтвердите необходимость создания нового исполнения СТМ30" 
                    Dlg.ButtonAcceptText = "Создать" 
                    Dlg.Title = "Новое исполнение"
                    Width = 300 }
                ( fun () -> Some () )
                ( fun () ->
                    source.Add(STM30.ProductType.createNew()))
        popup.Show(btnAdd)
    
    let btnDel = %% "removeitem"
    g.SelectionChanged.Add <| fun _ ->
        btnDel.Visible <- getSelectedProductTypes() |> List.isEmpty |> not
    Controls1.setTooltip btnDel "Удалить выделенные исполнения" 
    btnDel.Click.Add <| fun _ ->
        let popup,_ = 
            popupDialog
                { Dlg.def() with 
                    Dlg.Text = 
                        getSelectedProductTypes() 
                        |> listToStr ", " (fun (x:STM30.ProductType) ->sprintf "%s %s" x.Name x.NormativeDoc )
                        |> sprintf "Пожалуйста, подтвердите необходимость удаления исполнений %s" 
                        |> Some
                    Dlg.ButtonAcceptText = "Удалить" 
                    Dlg.Title = "Удалить исполнения" }
                ( fun () -> Some () )
                (fun () -> 
                    getSelectedProductTypes() 
                    |> List.iter ( STM30.ProductTypes.values.Remove >> ignore ) )
        popup.Show(btnDel)

    let btnClose = %% "close"
    btnClose.Click.Add <| fun _ ->
        placeHolder.Parent <- null
        Controls1.mainLayer.Visible  <- true
        party.UpdateProductsTypeAlchemy()
        party.Save()
        STM30.ProductTypes.save()

    placeHolder



let show() =    
    create().BringToFront()


    


    