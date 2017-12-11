module STM30.ViewModels.Party

open System
open System.ComponentModel

open STM30

open AppConfig
open MyWinForms.Converters

type private P = STM30.Product

type Product(batch, product, notifyChanged) as this =
    inherit ViewModelBase()
    
    let alchemy'main() = Alchemy.main batch product
    let alchemy'adjust() = Alchemy.adjust batch product
    let alchemy'variation() = Alchemy.variation batch product
    let alchemy'rele() = Alchemy.rele product

    let reader = Reader (fun () -> product.Addy)

    let rec (~%%) propertyName = 
        match propertyName with
        | "Main" ->
            %% "AlchemyMain"
            %% "AlchemyVariation"                
        | "Adjust" ->
            %% "AlchemyAdjust"
        | "ReleMain" ->
            %% "AlchemyRele"
        | _ -> ()
        ViewModelBase.raisePropertyChanged this propertyName
    
    do
        Runtime.PropertyChanged.add reader <| fun e -> 
            %% "Reader" 

    let p = product
    let p' = p.Main


    member val TuneValue = 0m with get, set

    member x.UpdateProductTypeAlchemy() = 
        %% "AlchemyAdjust"
        %% "AlchemyMain"
        %% "AlchemyVariation"

    static member updateProductTypeAlchemy (x:Product) =
        x.UpdateProductTypeAlchemy()
         

    //member __.Product = product
    member __.Reader = reader
    member __.Id = p.Id

    member __.AlchemyAdjust = alchemy'adjust()
    member __.AlchemyMain = alchemy'main()
    member __.AlchemyVariation = alchemy'variation()
    member __.AlchemyRele = alchemy'rele()
    

    member x.IsChecked  
        with get() = p.IsChecked  
        and set v = 
            p.IsChecked <- v
            notifyChanged()
    member x.Addy 
        with get() = p.Addy 
        and set v = 
            p.Addy <- v
            notifyChanged()
    member x.Serial 
        with get() = p.Serial 
        and set v = 
            p.Serial <- v
            notifyChanged()
    
    member x.Production = product.Production

    member x.Adjust 
        with get() = product.Adjust 
        and set v = 
            product.Adjust <- v
            notifyChanged()
            
    
    member x.Main 
        with get() = product.Main 
        and set v = 
            product.Main <- v
            notifyChanged()
    
    member x.ReleMain 
        with get() = product.ReleMain 
        and set v = 
            product.ReleMain <- v
            notifyChanged()

    member x.What = p.What

    member x.SetMain var gas value = 
        STM30.Product.setMain var gas p value
        notifyChanged()
        %% "Main"

    member x.SetRelesMain gas value = 
        STM30.Product.setReles gas p value
        notifyChanged()
        %% "ReleMain"
    

    member x.SetProduction prod value = 
        let (level, text) as lt =
            match value with
            | None -> Logging.Info, "Выполнено"
            | Some e -> Logging.Error, e
        STM30.Product.setProduction prod p lt
        Logging.write level "%s, %s - %s" product.What (ProductionPoint.what prod) text
        notifyChanged()
        %% "Production"

    member x.SetProduction1 prod value = 
        let (level, text) as lt =
            match value with
            | Ok x -> Logging.Info, x
            | Err e -> Logging.Error, e
        STM30.Product.setProduction prod p lt
        Logging.write level "%s, %s - %s" product.What (ProductionPoint.what prod) text
        notifyChanged()
        %% "Production"

let private partyProps = 
    [   "ProductType"
        "PGS1"
        "PGS2"
        "PGS3" ]
    |> Set.ofList

type ProductTypesConverter() = 
    inherit MyWinForms.Converters.ListConverter()
    override __.GetList () = 
        let xs = 
            STM30.ProductTypes.values
            |> Seq.toArray
            |> Array.map( fun x -> x.Name)
        xs

type Configger() as this = 
    inherit ViewModelBase()

    let rec (~%%) propertyName =         
        ViewModelBase.raisePropertyChanged this propertyName

    [<Category("СОМ порты")>]    
    [<DisplayName("СОМ порт СТМ30")>]
    [<Description("Имя СОМ порта, к которому подключены настраиваемые приборы чеерз преобразователь интерфейсов RS232, RS485")>]
    [<TypeConverter (typeof<MyWinForms.Converters.ComPortNamesConverter>) >]
    member x.ComportProducts
        with get() = config.ComportProducts.PortName
        and set v = 
            if v <> config.ComportProducts.PortName then
                config.ComportProducts.PortName <- v
                %% "ComportProducts"

    [<Category("СОМ порты")>]    
    [<DisplayName("СОМ порт стенда")>]
    [<Description("Имя СОМ порта, к которому подключен стенд")>]
    [<TypeConverter (typeof<MyWinForms.Converters.ComPortNamesConverter>) >]
    member x.ComportStend
        with get() = config.ComportStend.PortName
        and set v = 
            if v <> config.ComportStend.PortName then
                config.ComportStend.PortName <- v
                %% "ComportStend"

    [<Category("Опрос")>]    
    [<DisplayName("Концентрация")>]
    [<Description("Флаг, определяющий опрашивать ли концентрацию СТМ30 по интерфейсу modbus RS-485")>]
    [<TypeConverter (typeof<MyWinForms.Converters.InterrogateVarConverter>) >]
    member x.InterrogateConc
        with get() = config.Var.Conc
        and set v = 
            if v <> config.Var.Conc then
                config.Var.Conc <- v
                %% "InterrogateConc"

    [<Category("Опрос")>]    
    [<DisplayName("Ток выхода")>]
    [<Description("Флаг, определяющий опрашивать ли ток выхода СТМ30")>]
    [<TypeConverter (typeof<MyWinForms.Converters.InterrogateVarConverter>) >]
    member x.InterrogateCurr
        with get() = config.Var.Curr
        and set v = 
            if v <> config.Var.Curr then
                config.Var.Curr <- v
                %% "InterrogateCurr"

    [<Category("Опрос")>]    
    [<DisplayName("Напряжение")>]
    [<Description("Флаг, определяющий опрашивать ли напряжение СТМ30")>]
    [<TypeConverter (typeof<MyWinForms.Converters.InterrogateVarConverter>) >]
    member x.InterrogateTens
        with get() = config.Var.Tens
        and set v = 
            if v <> config.Var.Tens then
                config.Var.Tens <- v
                %% "InterrogateTens"

    [<Category("Опрос")>]    
    [<DisplayName("Реле")>]
    [<Description("Флаг, определяющий опрашивать ли реле СТМ30")>]
    [<TypeConverter (typeof<MyWinForms.Converters.InterrogateVarConverter>) >]
    member x.InterrogateRele
        with get() = config.Var.Rele
        and set v = 
            if v <> config.Var.Rele then
                config.Var.Rele <- v
                %% "InterrogateRele"

    [<Category("Опрос")>]    
    [<DisplayName("Режим")>]
    [<Description("Флаг, определяющий опрашивать ли признак режима СТМ30 по интерфейсу modbus RS-485")>]
    [<TypeConverter (typeof<MyWinForms.Converters.InterrogateVarConverter>) >]
    member x.InterrogateMode
        with get() = config.Var.Mode
        and set v = 
            if v <> config.Var.Mode then
                config.Var.Mode <- v
                %% "InterrogateMode"

type Party() as this =
    inherit Configger()
    let products = BindingList<Product>()
    let mutable batch = STM30.Repository.Batch.open'saved()
    let updateProductsTypeAlchemy() = 
        Seq.iter Product.updateProductTypeAlchemy products

    let rec (~%%) propertyName =         
        match propertyName with
        | "PGS1" | "PGS2" | "PGS3" | "ProductType" ->
            updateProductsTypeAlchemy()            
        | _ -> ()
        ViewModelBase.raisePropertyChanged this propertyName

    let isСhanged = Ref.Observable(false)
    let setIsChanged() = isСhanged.Value <- true

    let invalidateBatch() = 
        batch.Products |> List.iter ( fun p -> 
            products.Add( Product(batch, p, setIsChanged) ) ) 
        
    let setProd prod (request : Comport.Protocol.Request, level, rxd, strResult) = 
        batch.Products 
        |> List.filter( fun p -> p.Addy = request.addy)
        |> List.iter( fun p -> 
            STM30.Product.setProduction prod p (level, strResult) )    
    do
        invalidateBatch()
        Mdbs.TransmitEvent.Add (setProd ConnectionModbus)
        Stend.TransmitEvent.Add (setProd ConnectionStend)

    let getChecked() = 
        if batch.Products |> List.forall( fun x -> x.IsChecked ) then Nullable<bool>(true) else
        if batch.Products |> List.forall( fun x -> not x.IsChecked ) then Nullable<bool>(false) else
        Nullable<bool>()
    let mutable productsChecked = Nullable<bool>()

    let addLoggingEvent = new Event<_>()

    let setMainWindowTitle() = 
        MainWindow.form.Text <- 
            sprintf "Партия %s %s %A ПГС1=%M ПГС2=%M ПГС3=%M" 
                (DateTime.format "dd/MM/yy" batch.Date)
                batch.ProductType  
                batch.Name
                batch.PGS1 batch.PGS2 batch.PGS3
                
    do
        setMainWindowTitle()

    let save() = 
        Repository.Batch.save batch
        isСhanged.Value <- false
        config.View.PartyId <- batch.Id

    [<Browsable(false)>]
    member __.Products = products
    
    [<Browsable(false)>]
    member x.Id = batch.Id
    
    member x.GetNewValidAddy() = STM30.Batch.getNewValidAddy batch

    member __.Save() = 
        if isСhanged.Value then
            Repository.Batch.save batch
            isСhanged.Value <- false
            config.View.PartyId <- batch.Id

    member x.GetPgs gas = Batch.pgsConc gas batch

    member x.GetProductType() = Batch.productType batch

    member x.SetBatch v = 
        x.Save()
        products.Clear()
        batch <- v
        invalidateBatch()
        %% "PGS1"
        %% "PGS2"
        %% "PGS3"
        %% "ProductType"
        %% "Name"
        setMainWindowTitle()
        config.View.PartyId <- batch.Id

    member __.AddNewProduct() = 
        let addy = Batch.getNewValidAddy batch
        let newProduct = { STM30.Product.createNew() with Addy = addy }
        batch.Products <- batch.Products @ [newProduct]
        products.Add( Product(batch, newProduct, setIsChanged ) )
        setIsChanged()

    member __.DeleteProduct(product) = 
        if products.Remove( product ) then            
            batch.Products <- 
                batch.Products 
                |> List.filter( fun x -> x.Id <> product.Id )        
            setIsChanged()
        else failwithf "удаление из списка отсутствующего элемента, %s %s" __SOURCE_FILE__ __LINE__

    member x.UpdateProductsTypeAlchemy _ = updateProductsTypeAlchemy()
    member x.HasOneCheckedProduct() =
        products
        |> Seq.exists( fun p -> p.IsChecked )
    member x.HasNotOneCheckedProduct() =
        products
        |> Seq.exists( fun p -> p.IsChecked )
        |> not

    member x.GetRunInfo operation = batch.RunInfo.get operation
    
    member x.SetRunStart operation = 
        batch.RunInfo.SetStart operation
        setIsChanged()
    
    member x.SetRunEnd operation = 
        batch.RunInfo.SetEnd operation
        setIsChanged()

    member x.AddRunRecord operation l s = 
        batch.RunInfo.AddRecord operation l s
        setIsChanged()
        addLoggingEvent.Trigger( operation, l, s  )

    member x.ClearLoggigng() = 
        batch.RunInfo.RunOps <- Map.empty
        Repository.Batch.save batch

    member x.AddIsChangedHandler f = 
        isСhanged.AddChanged f

    
    [<CLIEvent>]
    member x.AddLoggingEvent = addLoggingEvent.Publish

    member x.GetReport() = 
        Report.batch batch

    [<Category("Концентрация ПГС")>] 
    [<DisplayName("ПГС1")>]    
    [<Description("Концентрация ПГС1, начало шкалы")>]
    member x.PGS1 
        with get() = batch.PGS1 
        and set v = 
            if v <> batch.PGS1 then
                batch.PGS1 <- v
                %% "PGS1"
                setMainWindowTitle()
                save()

    [<Category("Концентрация ПГС")>] 
    [<DisplayName("ПГС2")>]    
    [<Description("Концентрация ПГС2, середина шкалы")>]
    member x.PGS2 
        with get() = batch.PGS2 
        and set v = 
            if v <> batch.PGS2 then
                batch.PGS2 <- v
                %% "PGS2"
                setMainWindowTitle()
                save()

    [<Category("Концентрация ПГС")>] 
    [<DisplayName("ПГС3")>]    
    [<Description("Концентрация ПГС3, конец шкалы")>]
    member x.PGS3 
        with get() = batch.PGS3 
        and set v = 
            if v <> batch.PGS3 then
                batch.PGS3 <- v
                %% "PGS3"
                setMainWindowTitle()
                save()

    [<Category("Партия")>]
    [<DisplayName("Исполнение")>]    
    [<Description("Исполнение приборов партии")>]
    [<TypeConverter (typeof<ProductTypesConverter>) >]
    member x.ProductType 
        with get() = batch.ProductType 
        and set v = 
            if v <> batch.ProductType then
                batch.ProductType <- v
                %% "ProductType"
                setMainWindowTitle()
                save()
            
            
    [<Category("Партия")>]
    [<DisplayName("Наименование")>]    
    [<Description("Наименование партии")>]
    member x.Name 
        with get() = batch.Name 
        and set v = 
            if v <> batch.Name then
                batch.Name <- v
                %% "Name"
                setMainWindowTitle()
                save()

    



let party = Party()