module Runtime

open System
open System.Collections.Generic
open System.ComponentModel
open Microsoft.FSharp.Reflection


module PropertyChanged =

    let get x = 
        try
            x |> box :?> INotifyPropertyChanged
        with _ ->
            failwithf "PropertyChanged.get x:%A is not IPropertyChanged" x 

    let private (~%%) = get
    
    let add x f =
        (%% x).PropertyChanged.Add f    

    let addAction x f =
        let f = ComponentModel.PropertyChangedEventHandler(f)
        (%% x ).PropertyChanged.AddHandler f 
        f

    let addHandler x f =
        (%% x).PropertyChanged.AddHandler f
    
    let removeAction x f =
        (x |> box :?> ComponentModel.INotifyPropertyChanged).PropertyChanged.RemoveHandler f

    let isPropertyCahnged (t:Type) = 
        t.GetInterfaces() |> Array.exists ( (=) typeof<INotifyPropertyChanged> )

let isEnumerable (tx:Type) =
    typeof<System.Collections.IEnumerable>.IsAssignableFrom tx || typeof<IEnumerable<_>>.IsAssignableFrom tx

let makeGenericType (baseType : Type) (types : Type list) =  
    if (not baseType.IsGenericTypeDefinition) then
        invalidArg "baseType" "The base type specified was not a generic type definition." 
    baseType.MakeGenericType ( types |> List.toArray )

let makeListOf itemType (items : obj list) = 
    let listType = 
        makeGenericType 
        <| typedefof<Microsoft.FSharp.Collections.List<_>> 
        <| [ itemType; ] 
    let add =  
        let cons =  listType.GetMethod ("Cons")            
        fun item list ->
            cons.Invoke (null, [| item; list; |])                 
    let list = 
        let empty = listType.GetProperty ("Empty") 
        empty.GetValue (null, [||]) 
    list
    |> List.foldBack add items

[<AutoOpen>]
module private Helpers1 = 
    let rec generateFsharpObject (t:Type) : obj  =  
        let fail() = failwithf "generateFsharpObject %A " t
        if t.IsValueType then Activator.CreateInstance(t) 
        elif t = typeof<string> then box ""
        elif isEnumerable t then 
            if t.IsArray then
                let valueType = t.GetElementType()
                let result = Array.CreateInstance( valueType, 0 )
                box result
            elif t.IsGenericType  then
                let deft = t.GetGenericTypeDefinition()
                if deft = typedefof<list<_>> then      
                    let valueType = t.GetGenericArguments().[0]          
                    makeListOf valueType []
                elif deft = typedefof<Set<_>> then      
                    let valueType = t.GetGenericArguments().[0]
                    Activator.CreateInstance( t,   makeListOf valueType [] )
                elif deft = typedefof<Map<_,_>> then                
                    let keyValuePairType = typedefof<System.Tuple<_,_>>.MakeGenericType( t.GetGenericArguments() )                
                    Activator.CreateInstance( t, makeListOf keyValuePairType [] )
                else 
                    let valueType = t.GetGenericArguments().[0]                
                    Activator.CreateInstance( t, makeListOf valueType [] )
            else fail()
        elif  FSharpType.IsTuple t then 
            FSharpValue.MakeTuple( FSharpType.GetTupleElements(t) |> Array.map generateFsharpObject, t)
        elif FSharpType.IsUnion t then
            let cases = FSharpType.GetUnionCases t        
            let tag = 0
            let case = cases |> Array.find( fun x -> x.Tag=tag)
            let values = case.GetFields() |> Array.map (fun px -> generateFsharpObject px.PropertyType ) 
            FSharpValue.MakeUnion(case, values)
        elif FSharpType.IsRecord t then
            let values = 
                FSharpType.GetRecordFields(t)
                |> Array.map ( fun px -> generateFsharpObject px.PropertyType)
            FSharpValue.MakeRecord( t, values)
        else fail()

let generateFsharpObject<'a>() = 
    generateFsharpObject (typeof<'a>)
    :?> 'a