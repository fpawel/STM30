module STM30.Report

open System

open Tree
open Html

open STM30
open Alchemy

[<AutoOpen>]
module private Helpers = 

    let css = 
        IO.File.ReadAllText("report.css")

    let pt'gas = class' "pt-gas-name" 
    let param'name = class' "param-name" 
    
    let decToStr (specifier:string) (value:decimal) = value.ToString(specifier)
    let decOptToStr specifier value = 
        match value with
        | None -> ""
        | Some value -> decToStr specifier value

    let value'error specifier = function
        | None -> td [%% "&nbsp;"]
        | Some (v : Alchemy.ValueError) ->
            td[ %% decToStr specifier v.Conc
                class' ( if v.IsError then "invalid-error-value" else "valid-error-value" ) ]

    let var'title = function
        | Conc -> %% "Конц."
        | Curr -> span[ %%"I";  tag "sub" [ %% "вых."] ]
        | Tens -> span[ %%"U";  tag "sub" [ %% "вых."] ]


    let varblock b x var = 
        let (~&&) = value'error (STM30.VarType.stringFormat var)
        [   yield td [var'title var ; class' "var-name" ]
            for pt in PtGas.values do 
                yield && Alchemy.main b x var pt 
            if var=Conc then
                yield && Alchemy.adjust b x
                yield && Alchemy.variation b x ]
        |> tr 

    let releblock x = Rele.values |> List.map ( fun rele -> 
        let what,_,_,_ = Rele.info rele
        [   yield td [%% what ; class' "var-name" ]
            for pt in PtGas.values ->
                let valid = Rele.validState pt.Gas rele
                match Product.getRele pt rele x with
                | None -> td [%% "&nbsp;"]
                | Some value ->
                    td [%% (if value then "Вкл." else "Выкл.")
                        class' ( if value = valid then "invalid-error-value" else "valid-error-value" )] ] 
        |> tr )

    let testsblock x = 
        ProductionPoint.values |> List.map ( fun p -> 
        let (~&&) x = %% DateTime.format "dd.MM.yy HH:mm" x
        tr[
            yield td [%% p.What]
            match Product.getProduction p x with
            | None -> 
                yield td [ colspan 2; %% "не выполнялось" ]
                yield class' "not-performed-test-row"
            | Some (dt, Logging.Error, error) ->
                 yield  td [&& dt ]
                 yield  td [%% error ]
                 yield class' "failed-test-row"
            | Some (dt, _, text) ->
                 yield  td [&& dt ]
                 yield  td [%% text ]
                 yield  class' "successful-test-row" ] )


let product b (x:Product) = 
    let t = Batch.productType b
    
        
    let s' = 
        [   h1[ id' x.Id
                class' "product-title"            
                %% sprintf "Паспорт %s № %d" b.ProductType x.Serial ] 
            
            table[            
                thead[
                    tr[ th [%% "Дата изготовления" ]
                        th [%% "Шифр НТД"; param'name]                    
                        th [%% "ПГС1"; pt'gas]
                        th [%% "ПГС2"; pt'gas]
                        th [%% "ПГС3"; pt'gas] ] ]
                tbody[
                    tr[
                        td [%% sprintf "%s" ( DateTime.format "dd MMMM yyyy года" b.Date) ]
                        td [%% t.NormativeDoc ]
                        td [%% sprintf "%M" b.PGS1]
                        td [%% sprintf "%M" b.PGS2]
                        td [%% sprintf "%M" b.PGS3] ] ]]
                
            table[
                caption[ %% "Основная погрешность и вариация показаний"]
                thead[  
                    tr[ yield th [%% "&nbsp;"]
                        yield!
                            [   for pt in PtGas.values do yield pt.Gas.What
                                yield "Калибровка" 
                                yield  "Вариация" ] 
                            |> List.map( fun x ->  th[ %% x;  pt'gas ] ) ] ]
                tbody[
                    for var in VarType.values do
                        yield varblock b x var 
                    yield! releblock x ] ] 

            table[ 
                caption [%% "Настройка СТМ-30"]
                tbody (testsblock x) ] ]
        |> section

    let nav' = 
        tag "a" ["href" <! ("#" + x.Id); %% sprintf "№%d-#%d" x.Serial x.Addy]
        
        
    nav', s'

let batch b =
    let navs, xs = b.Products |> List.map (product b) |> List.unzip
    [   div[
            yield id' "left"
            yield! navs ]
        div[
            yield id' "right"
            yield! xs ] ] 
    |> html5 css "Индивидуальные паспорта СТМ-30"
    |> listToStr "\n" stringify


    
    