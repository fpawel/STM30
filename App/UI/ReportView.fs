module STM30.View.Report 

open System
open System.Windows.Forms
open System.Drawing

open STM30
open STM30.View

type private Dck = DockStyle

let placeHolder = new Panel(Dock = Dck.Fill)

let ie = 
    let x = 
        new WebBrowser(Parent = placeHolder, Dock = Dck.Fill, Url = null,
                       IsWebBrowserContextMenuEnabled = false, AllowWebBrowserDrop = false)
    x
let toolbar = new Panel(Parent = placeHolder, Dock = Dck.Top, Height = 40)



let private (~%%) k = 
    let x = new Button(Parent = toolbar, Height = 40, Width = 40, 
                       ImageList = Widgets.Icons.instance.imageList1,
                       Dock = Dck.Right, ImageKey = k)
    let _ = new Panel(Parent = toolbar, Dock = Dck.Right, Width = 5)
    x

let btnSave = %% "save"
let btnPrint = %% "print"
let btnClose = %% "close"



let initialize =
    btnClose.Click.Add <| fun _ ->
        placeHolder.Parent <- null
        Controls1.mainLayer.Visible <- true


    Controls1.TopBar.buttonReport.Click.Add <| fun _ ->  
        ie.DocumentText <- ViewModels.Party.party.GetReport()
        Controls1.mainLayer.Visible <- false
        placeHolder.Parent <- MainWindow.form
        placeHolder.BringToFront()

    btnSave.Click.Add <| fun _ ->
        let dlg = 
            new SaveFileDialog
                (Filter = "веб-страница (*.html)|*.html|Все файлы (*.*)|*.*",
                 FilterIndex = 1, RestoreDirectory = true
                    )
        if dlg.ShowDialog() <> DialogResult.OK then () else
        IO.File.WriteAllText
            (dlg.FileName, ie.Document.Body.Parent.OuterHtml, 
                Text.Encoding.GetEncoding(ie.Document.Encoding))

    fun () -> ()
