module AppConfig

open System
open System.IO

open System
open System.Collections.ObjectModel
open System.ComponentModel
open System.ComponentModel.DataAnnotations
open System.Drawing.Design

open MyWinForms.Converters

[<TypeConverter(typeof<ExpandableObjectConverter>)>]
type TuneConfig = 
    {   [<DisplayName("Минимум начала шкалы")>]
        [<Description("Минимально допустимое значение сигнала начала шкалы")>]
        mutable ScaleNullMin : decimal

        [<DisplayName("Максимум начала шкалы")>]
        [<Description("Максимально допустимое значение сигнала начала шкалы")>]
        mutable ScaleNullMax : decimal

        [<DisplayName("Минимум конца шкалы")>]
        [<Description("Минимально допустимое значение сигнала конца шкалы")>]
        mutable ScaleEndMin : decimal

        [<DisplayName("Максимум конца шкалы")>]
        [<Description("Максимально допустимое значение сигнала конца шкалы")>]
        mutable ScaleEndMax : decimal

        [<DisplayName("Шаг")>]
        [<Description("Шаг подстройки сигнала")>]
        mutable Step : decimal

        [<DisplayName("Максимум шагов")>]
        [<Description("Максимальное количество шагов подстройки за одну итерацию")>]
        mutable MaxStepsCount : int

        [<DisplayName("Максимум циклов")>]
        [<Description("Максимальное циклов подстройки при выходе сигнала из допустимого диапазона")>]
        mutable TotalCount : int

        [<DataType(DataType.Time) >]
        [<Editor( typeof<TimePickerEditor>, typeof<UITypeEditor> )>]
        [<TypeConverter (typeof<TimeSpanConverter>) >]   
        [<DisplayName("Таймаут")>]
        [<Description("Таймаут подстройки в формате <часы : минуты : секунды>")>]
        mutable Timeout  : TimeSpan

        [<DisplayName("Задержка")>]
        [<Description("Длительность задержки в милисекундах от момента отправки команды подстройки до измерения сигнала ")>]
        mutable Delay  : int    

        [<DisplayName("Зона нечувствительности")>]
        [<Description("Минимальное примемлемое значение измеренного сигнала")>]
        mutable Unsense  : decimal    

        [<DisplayName("Шаг в зоне нечувствительности")>]
        [<Description("Шаг подстройки сигнала в зоне нечувствительности")>]
        mutable StepUnsense  : int    
        
        
        }
    override x.ToString() = "<...>"


[<TypeConverter(typeof<ExpandableObjectConverter>)>]
type GonfigStendConsI = 
    {   [<DisplayName("Канал")>]
        [<Description("Номер канала стенда")>]
        Addy : byte

        [<DisplayName("A")>]
        [<Description("Коэффициент тока канала нулевой степени")>]
        mutable ValueA : decimal

        [<DisplayName("B")>]
        [<Description("Коэффициент тока канала первой степени")>]
        mutable ValueB : decimal 

        [<DisplayName("Использовать")>]
        [<Description("Разрешить подстройку тока канала с коэффициентами нулевой и первой степени")>] 
        [<TypeConverter(typeof<YesNoConverter>)>]
        mutable On : bool }
    override x.ToString() = 
        if x.On && (x.ValueA<>1m || x.ValueB<>0m ) then
            sprintf "%d A=%M B=%M" x.Addy x.ValueA x.ValueB
        else
            sprintf "%d" x.Addy

[<TypeConverter(typeof<ExpandableObjectConverter>)>]
type StendConfig = 
    {   mutable MeasureTensionCount : int
        mutable MeasureTensionMXTime : int
        mutable MeasureCurrentCount : int
        mutable MeasureCurrentMXTime : int
        mutable MinChan2 : decimal
        mutable MaxChan2 : decimal
        mutable CurrA : decimal
        mutable CurrB : decimal

        mutable TensA : decimal
        mutable TensB : decimal

        mutable ChanA : decimal
        mutable ChanB : decimal

        mutable ConsK : decimal
        mutable ConsD : decimal 

        [<Browsable(false)>]
        ConsI : GonfigStendConsI list }

    override x.ToString() = "<...>"

    [<DisplayName("Подстройка тока каналов")>]
    [<Description("Подстройка тока каналов")>]
    member x.ConsI1  
        with get() = x.ConsI |> List.toArray
        and set xs =
            for y in x.ConsI do
                match xs |> Array.tryFind( fun (z:GonfigStendConsI) -> z.Addy=y.Addy ) with
                | None -> ()
                | Some x -> 
                    y.On <- x.On 
                    y.ValueA <- x.ValueA 
                    y.ValueB <- x.ValueB

[<TypeConverter(typeof<ExpandableObjectConverter>)>]
type Var = 
    {   [<DisplayName("Концентрация")>]
        [<Description("Концентрация СТМ-30, MODBUS")>]
        [<TypeConverter(typeof<YesNoConverter>)>]
        mutable Conc : bool

        [<DisplayName("Ток выхода")>]
        [<Description("Ток выхода СТМ-30, СТЕНД")>]
        [<TypeConverter(typeof<YesNoConverter>)>]
        mutable Curr : bool

        [<DisplayName("Напряжение выхода")>]
        [<Description("Напряжение СТМ-30, СТЕНД")>]
        [<TypeConverter(typeof<YesNoConverter>)>]
        mutable Tens : bool

        [<DisplayName("Реле")>]
        [<Description("Состояние контактов выходных реле СТМ-30, СТЕНД")>]
        [<TypeConverter(typeof<YesNoConverter>)>]
        mutable Rele : bool 

        [<DisplayName("Режим")>]
        [<Description("Режим СТМ-30, MODBUS")>]
        [<TypeConverter(typeof<YesNoConverter>)>]
        mutable Mode : bool   }

    override x.ToString() = "<...>"

    static member createNew () =
        {   Conc = true
            Curr = true
            Tens = true
            Rele = true 
            Mode = true  }

    static member hasOn (x:Var) =
        Microsoft.FSharp.Reflection.FSharpType.GetRecordFields( typeof<Var> )
        |> Array.exists( fun ri -> ri.GetValue(x,null) :?> bool )


module View = 
    type Grid =  
        {   mutable ColWidths : int list
            mutable ColumnHeaderHeight : int }

    type Config =  
        {   mutable PartyId : string
            mutable Grids : Map<string,Grid>   
            mutable ScnDetailTextSplitterDistance : int  
            mutable ProductsLayerSplitterDistance : int  }

[<TypeConverter(typeof<ExpandableObjectConverter>)>]
type ApplicatioConfig = 
    {   [<Category("Подстройка СТМ-30")>] 
        [<DisplayName("Подстройка тока")>]
        [<Description("Подстройка выходного тока СТМ-30")>]
        TuneCurr : TuneConfig
        
        [<Category("Подстройка СТМ-30")>] 
        [<DisplayName("Подстройка напряжения")>]
        [<Description("Подстройка выходного напряжения СТМ-30")>]
        TuneTens : TuneConfig
        
        [<Category("Стенд")>] 
        [<DisplayName("Измерения")>]
        [<Description("Параметры параметры стенда, которые относятся к измерениям тока и паряжения")>]
        Stend : StendConfig

        [<Category("Стенд")>] 
        [<DisplayName("Адрес пневмоблока")>]
        [<Description("Адрес MODBUS пневмоблока")>]
        mutable PneumoAddy : byte
        
        [<Category("СОМ порты")>]    
        [<DisplayName("СОМ порт СТМ30")>]
        [<Description("Параметры приёмопередачи СОМ порта, к которому подключены настраиваемые приборы чеерз преобразователь интерфейсов RS232, RS485")>]
        ComportProducts : ComportConfig.Config
        
        [<Category("СОМ порты")>]    
        [<DisplayName("СОМ порт стенда")>]
        [<Description("Параметры приёмопередачи СОМ порта, к которому подключен стенд")>]
        ComportStend : ComportConfig.Config
        
        [<Browsable(false)>]
        Var : Var    
                
        [<Browsable(false)>]
        View : View.Config  }
    static member create() = {   
        View = Runtime.generateFsharpObject<View.Config>()
        PneumoAddy = 100uy
        Var = Var.createNew()
        TuneCurr = 
            {   ScaleNullMin = 4M - 0.03m 
                ScaleNullMax = 4M + 0.03m 
                ScaleEndMin = 20M - 0.03m 
                ScaleEndMax = 20M + 0.03m 
                Step  = 0.1m
                MaxStepsCount = 50
                Timeout = new TimeSpan(0,1,0)
                Delay   = 300
                TotalCount = 3 
                Unsense = 0m
                StepUnsense = 1}
        TuneTens = 
            {   ScaleNullMin =  0m
                ScaleNullMax = 0.02m
                ScaleEndMin = 1m - 0.01m
                ScaleEndMax = 1m + 0.01m
                MaxStepsCount = 250
                Step  = 0.003m
                Timeout = new TimeSpan(0,1,0)
                Delay   = 300
                TotalCount = 3 
                Unsense = 0.01m
                StepUnsense = 1}

        ComportProducts = { ComportConfig.Config.dummy() with Description = "приборы"}

        ComportStend = 
            {   ComportConfig.Config.dummy() with 
                    Description = "стенд"
                    Timeout = 3000
                    Chartime = 100 }
            
        Stend = 
            {   CurrA = 0.01m
                CurrB = 0m
                TensA = 0.001m
                TensB = 0m
                MeasureTensionCount  = 1000
                MeasureTensionMXTime = 300
                MeasureCurrentCount  = 3000
                MeasureCurrentMXTime = 300
                MinChan2 = 3.5m
                MaxChan2 = 4.2m
                ChanA = 10m /17.4m
                ChanB = 0.374m
                ConsI = [ for n in 1uy..20uy -> { Addy = n; ValueA=1m; ValueB=0m; On=false } ] 
                ConsK = 1m
                ConsD = 0.75m*100m/21.67m  } }

let config, save = Logging.JsonConfig.create "app.config.json" ApplicatioConfig.create








