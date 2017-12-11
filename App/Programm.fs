module Programm

open System
open System.IO
open System.Windows.Forms
open System.Threading

open STM30.View
open STM30.ViewModels

let main () = 
    try 
        Application.EnableVisualStyles() 
        Application.SetCompatibleTextRenderingDefault true 
        
        let config = AppConfig.config
        let party = STM30.ViewModels.Party.party
        let form = MainWindow.form
        
        Controls1.initialize()
        Thread2.initialize()
        PartyMainGrid.initialize()
        PartyVarGrids.initialize()
        Report.initialize()
        Menus1.initialize()
        Ref.``check referencies was initialized``()
        
        Application.Run MainWindow.form 
        Party.party.Save()
        
        AppConfig.save ()
        STM30.ProductTypes.save()

        
    with e ->
        Logging.error "%A" e 
        MessageBox.Show( sprintf "%A" e ,"Исключение", MessageBoxButtons.OK, MessageBoxIcon.Error ) |> ignore   


let onAnotherInstanceExist() = 
    MessageBox.Show( "Нельзя создать более одного экземпляра приложения" ,"Производство СТМ-30М", MessageBoxButtons.OK, MessageBoxIcon.Information ) |> ignore   
    

let mutexid = "Global\\{B1E7934A-F688-417f-8FCB-65C3985E9E27}"

open System.Security.Principal

open System.Security.AccessControl

[<EntryPoint>]
[<STAThread>]
do
    
    use mutex = new System.Threading.Mutex(false, mutexid)
    let si = new SecurityIdentifier(WellKnownSidType.WorldSid, null)

    let allowEveryoneRule =             
        new MutexAccessRule(si, MutexRights.FullControl, AccessControlType.Allow)   
    let securitySettings = new MutexSecurity();
    securitySettings.AddAccessRule(allowEveryoneRule);
    mutex.SetAccessControl(securitySettings)
        
    try
        let hasHandle = mutex.WaitOne(0, false);
        if not hasHandle then onAnotherInstanceExist() else
        main()
    with
    | :? AbandonedMutexException ->
        onAnotherInstanceExist()