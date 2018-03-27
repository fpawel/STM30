module STM30.View.Thread2 

open System
open System.Windows.Forms
open System.Drawing
open System.Threading

open MyWinForms.Utils
open MyWinForms.TopDialog

open MainWindow
open STM30.View
open STM30.Behaviors.Thread2

let private updLabelTime (tb:Label) startTime = 
    tb.Text <- sprintf "%s - %s" ( DateTime.toString startTime) (TimeSpan.toString <| DateTime.Now - startTime  ) 

let private par'bar = Controls1.TopBar.thread2

let text'scenary'name = 
    let x = new Label(  Parent = par'bar, Height = 35, 
                        TextAlign = ContentAlignment.MiddleLeft, 
                        Dock = DockStyle.Left, Visible = false,
                        Font = new Font("Consolas", 12.f) ) 
    x.TextChanged.Add <| fun _ ->
        let sz = TextRenderer.MeasureText( x.Text, x.Font, Size(Int32.MaxValue, x.Height) )
        x.Width <- sz.Width + 10
    x

let closing'bar = 
    TopmostBar( form, Visible = false, Width = 300, Font = new Font("Consolas", 12.f),
                TextForeColor = Some Color.Brown, Placement = RightBottom,
                ButtonAccept = None, ButtonCancel = None,
                Text = "Выполнение прервано! Выполняется подготовка данных...") 

let buttonStop = 
    new Button(Parent = par'bar, Height = 40, Width = 40, Visible = false,
               ImageList = Widgets.Icons.instance.imageList1,
               Dock = DockStyle.Left, ImageKey = "close")

let performing'bar = TopmostBar(form, Visible = false, MinTextHeight = 0, Width = 300, 
                                Font = new Font("Consolas", 10.f))

let modal'message'bar = 
            TopmostBar(form, Visible = false, Width = 400, Font = new Font("Consolas", 12.f),
                       Placement = Center, ButtonAccept = Some "Продолжить", ButtonCancel = None )

module Delay = 
    open STM30.Behaviors.Work
    [<AutoOpen>]
    module private P = 
        let private textHeight = (TextRenderer.MeasureText("X", performing'bar.Font)).Height
        let p = new Panel(Height = 50 + textHeight + 5, Left = 10)
                
        let progressBar = 
            new ProgressBar(Parent = p, Height = 15, Width = 315, Value = 0,  Minimum=0, Maximum = Int32.MaxValue)
        let button'skip = new Button(Parent = p, Top = progressBar.Top, Left = 330, Height = 45, Width = 45,
                                        ImageList = Widgets.Icons.instance.imageList1,
                                        ImageKey = "skip")
        let mutable text = ""
                
                
        let upd start'time get'time = safe p <| fun () ->
            let elepsed = DateTime.Now - start'time

            performing'bar.Text <- sprintf "%s %s из %s" text (TimeSpan.toString elepsed) (TimeSpan.toString (get'time()))                
            let value, maximum = int elepsed.TotalMilliseconds, int (get'time()).TotalMilliseconds
            if value < maximum then
                if value > progressBar.Maximum then
                    progressBar.Maximum <- Int32.MaxValue
                progressBar.Value <- int elepsed.TotalMilliseconds
                progressBar.Maximum <- int (get'time()).TotalMilliseconds
            

    let initialize = 
        button'skip.Click.Add <| fun _ ->
            STM30.Behaviors.Work.Delay.cancel()

        Delay.onStart.Value <- fun what get'time -> safe p <| fun () ->
            text <- what
            progressBar.Value <- 0                
            performing'bar.DoUpdate <| fun () ->
                p.Height <- button'skip.Bottom + 10
                performing'bar.BottomControl <- Some ( p :> Control )

            upd DateTime.Now get'time

        Delay.onStop.Value <- fun () -> safe p <| fun () ->
            performing'bar.DoUpdate <| fun () ->                    
                performing'bar.BottomControl <- None

        Delay.onUpdate.Value <- upd
        fun () -> ()
    
    

let initialize = 

    Delay.initialize()

    add'keep'running <| function
        | _, false ->                 
            safe form <| fun () ->
                modal'message'bar.Visible <- false
        | _ -> ()

    STM30.Behaviors.Work.ModalMessage.onShow.Value <- fun title level text ->            
        let t = modal'message'bar
        safe form <| fun () -> 
            t.Visible <- false
            t.Title <- title
            t.TextForeColor <- Some <| Logging.foreColor level
            t.Text <- text 
            t.Visible <- true

    STM30.Behaviors.Work.ModalMessage.onClose.Value <- fun () ->            
        safe form <| fun () -> 
            modal'message'bar.Visible <- false

    STM30.Behaviors.Work.ModalMessage.getIsVivisble.Value <- fun () ->
        modal'message'bar.Visible

    show'performing'message.Value <- fun level text ->            
        safe form <| fun () -> performing'bar.DoUpdate <| fun () ->
            let text = if text.Length < 50 then text else text.Substring(0,47) + "..."
            performing'bar.Text <- text
            performing'bar.TextForeColor <- Some <| Logging.foreColor level

    show'performing'report.Value <- fun title level text ->
        let t = performing'bar
        safe form <| fun () -> t.DoUpdate <| fun () ->
            t.Placement <- RightBottom
            t.TopControl <- None
            t.BottomControl <- None
            t.Title <- title
            t.ButtonAccept <- Some "Закрыть"
            t.ButtonCancel <- None
            t.Text <- text
            t.TextForeColor <- Some <| Logging.foreColor level
    

    add'is'running'handler <| fun (_,v) ->
        safe par'bar <| fun () ->
            text'scenary'name.Visible <- v
            if v then
                text'scenary'name.Text <- sprintf "Выполняется сценарий %A"  scenary.Value.FullName

    add'is'running'handler <| function
        | true,false -> safe form <| fun () ->
            closing'bar.Visible <- false        
        | _ -> ()
        
    buttonStop.Click.Add <| fun _ ->
        STM30.Behaviors.Thread2.forceStop()
        STM30.Behaviors.Work.Delay.cancel()
        buttonStop.Visible <- false
        Logging.warn "выполнение сценария %A было прервано пользователем" scenary.Value.FullName
        closing'bar.DoUpdate <| fun () ->
            closing'bar.Title <- scenary.Value.Name
        
    add'keep'running <| fun (_,keep'running) ->
        safe form <| fun () ->
            buttonStop.Visible <- keep'running 

    buttonStop.Click.Add <| fun _ ->        
        performing'bar.Visible <- false
    add'operation'changed <| fun x ->
        safe form <| fun () ->   
            let x = 
                match x with
                | Some x -> x
                | None -> STM30.Behaviors.Thread2.scenary.Value
            performing'bar.Title <- sprintf "%s %s" x.RunInfo.Status x.FullName   
    add'is'running'handler <| function
        | false,true -> 
            let t = performing'bar
            t.DoUpdate <| fun () ->                 
                t.Placement <- RightBottom
                t.BottomControl <- None
                t.ButtonAccept <- None
                t.ButtonCancel <- None
                t.Text <- ""
        | _ -> ()

    
    fun () -> ()