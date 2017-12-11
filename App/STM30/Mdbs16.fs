module Mdbs16

type CmdAttribute(_code:int, _what:string) = 
    inherit System.Attribute()
    member __.What = _what
    member __.Code = _code

type Cmd =
    | [<Cmd(1, "Корректировка нуля")>]
      ADJ_0
    | [<Cmd(2, "Корректировка чувствительности")>]
      ADJ_S
    | [<Cmd(3, "Установка ПОРОГ1")>]
      POR_1
    | [<Cmd(4, "Установка ПОРОГ2")>]
      POR_2
    | [<Cmd(5, "Установка адреса")>]
      SetAddy
    | [<Cmd(6, "Подстройка 4 мА")>]
      SET_I_4
    | [<Cmd(7, "+/- 4 мА")>]
      INC_I_4
    | [<Cmd(8, "Подстройка 20 мА")>]
      SET_I_20
    | [<Cmd(9, "+/- 20 мА")>]
      INC_I_20
    | [<Cmd(10, "Подстройка 0В")>]
      SET_U_0
    | [<Cmd(11, "+/- 0В")>]
      INC_U_0
    | [<Cmd(12, "Подстройка 1В")>]
      SET_U_1
    | [<Cmd(13, "+/- 1В")>]
      INC_U_1
    | [<Cmd(0x0e, "Сброс реле")>]
      RESET
    

    member x.What = Cmd.what x
    member x.Code = Cmd.сode x
    
    static member private info x = 
        let a = tryGetCaseAttribute<CmdAttribute,Cmd> x
        a.Value

    static member сode x = (Cmd.info x).Code

    static member what x = 
        let a = Cmd.info x
        if a.What="" then sprintf "модбас-%d" a.Code else a.What

    static member values = unionCasesList<Cmd>

let write addy (cmd : Cmd) value = Mdbs.write addy cmd.Code cmd.What value