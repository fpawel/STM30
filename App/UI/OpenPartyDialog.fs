module STM30.View.OpenPartyDialog

open System
open System.Windows.Forms
open System.Drawing
open System.Collections.Generic
open System.ComponentModel
open System.ComponentModel.DataAnnotations

open MyWinForms.TopDialog
open MainWindow
open STM30
open STM30.View

[<AutoOpen>]
module private Helpers =
    
    let party = STM30.ViewModels.Party.party
        
        
    type Node = 
        | Year of int 
        | Month of int * int
        | Day of int * int * int
        | Party of STM30.Info.Batch

    let setPartyNodeStyle(node:TreeNode) = 
        node.ForeColor <- Color.Blue
        node.BackColor <- Color.LightGray
        //node.NodeFont <- new Font( "Segue UI", 12.f, FontStyle.Bold )

    let updateTreeView 
            (treeview:TreeView) 
            (nodes:ResizeArray<Node * TreeNode>) 
            prodType serial  = 
        let mutable selectedNode : TreeNode = null     
        nodes.Clear() 
        treeview.Nodes.Clear()
        STM30.Repository.getParties prodType serial 
        |> Seq.iter( fun (year,xs) -> 
            let xyear = TreeNode(sprintf "%d" year  )
            treeview.Nodes.Add xyear |> ignore
            nodes.Add(Year year, xyear)
            xs |> Seq.iter( fun (month,xs) -> 
                let xmonth = TreeNode(monthByNumber month  )
                nodes.Add(Month (year,month), xmonth)
                xyear.Nodes.Add xmonth |> ignore
                xs |> Seq.iter( fun (day,xs) ->                         
                    let xday = TreeNode(sprintf "%d" day  )
                    nodes.Add(Day (year,month,day), xday)
                    xmonth.Nodes.Add xday |> ignore
                    xs |> Seq.iter( fun b -> 
                        let node = 
                            TreeNode(Text = sprintf "%s, %A, %d" b.ProductType b.Name b.Serials.Length )                        
                        if b.Id = party.Id then
                            selectedNode <- node
                            setPartyNodeStyle node
                            setPartyNodeStyle xday
                            setPartyNodeStyle xmonth
                            setPartyNodeStyle xyear
                        nodes.Add(Party b, node)
                        xday.Nodes.Add node |> ignore ) ) ) )        
                                
        
        //treeview.Nodes.AddRange ( nodes |> Seq.map snd |> Seq.toArray )
        treeview.SelectedNode <- selectedNode  
        treeview.ExpandAll()
        if selectedNode <> null then
            selectedNode.EnsureVisible()

    let createRigthPanelControls p =
        let cbType = 
            let x =
                new MyWinForms.FlatComboBox(Parent = p, Dock = DockStyle.Top, DropDownStyle = ComboBoxStyle.DropDownList,
                                            DisplayMember = "What", FlatStyle = FlatStyle.Flat)        
            x.Items.Add "" |> ignore
            for a in ProductTypes.values do
                x.Items.Add a.Name |> ignore
            x            
        let _ = new Panel( Parent = p, Height = 3, Dock = DockStyle.Top )         

        let _ = new Label(Parent = p, Dock = DockStyle.Top, Text = "Исполнение", AutoSize = true)
        let _ = new Panel( Parent = p, Height = 3, Dock = DockStyle.Top )     
        
        
        let tbSerialNumber = new TextBox(Parent = p, Dock = DockStyle.Top)
        let _ = new Panel( Parent = p, Height = 3, Dock = DockStyle.Top )         

        let _ = new Label(Parent = p, Dock = DockStyle.Top, Text = "Серийный №", AutoSize = true)
        let _ = new Panel( Parent = p, Height = 15, Dock = DockStyle.Top )

        let buttonAccept = new Button( Parent = p, Dock = DockStyle.Top, Height = 40,
                                        Text="Открыть", FlatStyle = FlatStyle.Flat )
        let _ = new Panel( Parent = p, Height = 3, Dock = DockStyle.Top )     
        cbType, tbSerialNumber, buttonAccept
        

    let cretaeDialogControls() =
        let p = new Panel( Width = 500, Height = 600, Font = new Font("Consolas", 12.f), 
                            BorderStyle = BorderStyle.FixedSingle )  
        let treeview = new TreeView(Parent = p, CheckBoxes = false, Dock = DockStyle.Fill, 
                                    ShowNodeToolTips = true,
                                    Font = new Font( "Segue UI", 10.f ))     
        let _ = new Panel( Parent = p, Width = 5, Dock = DockStyle.Left ) 
        let _ = new Panel( Parent = p, Height = 5, Dock = DockStyle.Top ) 
        let _ = new Panel( Parent = p, Width = 5, Dock = DockStyle.Right ) 
        let p1 = new Panel( Parent = p, Width = 120, Dock = DockStyle.Right )  
        let _ = new Panel( Parent = p, Width = 5, Dock = DockStyle.Right ) 
        let _ = new Panel( Parent = p, Height = 5, Dock = DockStyle.Bottom ) 

        let _ = new Label(Parent = p, TextAlign = ContentAlignment.MiddleLeft,
                                        Text = "Открыть партию", Width = p.Width,
                                        Dock = DockStyle.Top,
                                        ForeColor = Color.White, BackColor = Color.Navy)
        let popup = new MyWinForms.Popup(p)
        
        let p2 = new Panel( Parent = p1, Dock = DockStyle.Fill )  
        let cbType, tbSerialNumber, buttonAccept = createRigthPanelControls p2
        treeview, buttonAccept, cbType, tbSerialNumber,  popup
        
        
    let createDialog () = 
        
        let treeview, buttonAccept, cbType, tbSerialNumber, popup = 
            cretaeDialogControls()

        let nodes = ResizeArray<Node * TreeNode>()
        
        let getBatch() = 
            let x = treeview.SelectedNode
            if x=null then None else
            let nodes1 = nodes |> Seq.map( fun (x,y) -> y.GetHashCode(),x) |> Map.ofSeq
            match nodes1.[x.GetHashCode()] with
            | Party x -> Some x
            | _ -> None

        buttonAccept.Enabled <- getBatch().IsSome

        let acceptFilter _ =
            let prodType = 
                let n = cbType.SelectedIndex - 1
                if n>(-1) && n<ProductTypes.values.Count then Some ProductTypes.values.[n].Name else None
            let serial = 
                let b,v = Int32.TryParse tbSerialNumber.Text
                if b && v > -1  then Some v else None
            
            updateTreeView treeview nodes prodType serial 
            buttonAccept.Enabled <- getBatch().IsSome

        cbType.SelectedIndexChanged.Add acceptFilter
        tbSerialNumber.TextChanged.Add acceptFilter

        acceptFilter ()

        treeview.AfterSelect.Add <| fun _ -> 
            buttonAccept.Enabled <- getBatch().IsSome

        buttonAccept.Click.Add <| fun _ ->
            popup.Hide()
            match getBatch() with
            | None -> ()
            | Some b -> 
                match STM30.Repository.Batch.open'by'id b.Id with
                | Ok b -> 
                    party.SetBatch b
                    MyWinForms.Utils.updateGridDataSourceBinding Controls1.GridScenary
                | Err error -> 
                    MessageBox.Show(sprintf "Не удалось открыть данные партии %A, %A. %s" 
                                        b.Date b.ProductType error, 
                                        "Ошибка", MessageBoxButtons.OK, MessageBoxIcon.Error )
                    |> ignore
        popup



let showDialog (button : Button) = 
    let popup = createDialog()    
    popup.Show(button)