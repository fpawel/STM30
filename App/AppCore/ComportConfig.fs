module ComportConfig

open System
open System.IO

open System
open System.ComponentModel
open System.ComponentModel.DataAnnotations
open MyWinForms.Converters

[<TypeConverter(typeof<ExpandableObjectConverter>)>]
type Config =
    {   [<DisplayName("Порт")>]
        [<Description("Выбор имени используемого СОМ порта")>]
        [<TypeConverter (typeof<ComPortNamesConverter>) >]
        mutable PortName : string

        [<DisplayName("Таймаут, мс")>]
        [<Description("Длительность ожидания ответа от прибора в милисекундах")>]   
        mutable Timeout : int

        [<DisplayName("Задержка отправки, мс")>]
        [<Description("Задержка отправки запроса прибору в милисекундах")>]
        mutable Delay : int

        [<DisplayName("Время ожидания символа, мс")>]
        [<Description("Длительность ожидания символа ответа в милисекундах")>]
        mutable Chartime : int

        [<DisplayName("Колличество повторов запроса")>]
        [<Description("Колличество повторов запроса прибору")>]
        mutable RepeatCount : int

        [<DisplayName("Показывать посылки")>]
        [<Description("Разрешить показывать посылки приёмопередачи СОМ порта в консоли данного приложения")>] 
        [<TypeConverter(typeof<YesNoConverter>)>]
        mutable CanLog : bool 

        [<Description("Скорость передачи")>]        
        [<DisplayName("Скорость передачи СОМ порта, заданная в килобитах в секунду (бодах)")>]
        mutable BaudRate : int 

        [<Browsable(false)>]
        Description : string }  
    override x.ToString() = "<...>"

    static member dummy() = {   
        PortName = ""
        Timeout = 1000
        Delay = 0
        Chartime = 20
        RepeatCount = 0
        CanLog = false 
        BaudRate = 9600
        Description = "-" }

