namespace STM30.ViewModels

open System
open System.Collections.ObjectModel

open STM30
open STM30.Info

type Reader(getAddy) as this = 
    inherit ViewModelBase()
    let rec (~%%) = ViewModelBase.raisePropertyChanged this        
    let mutable vvls = Map.empty
    let mutable rele = None // Set<Rele> Value
    let mutable stat = None //: string Value 

    let readVar var = 
        let v = 
            match var with
            | Conc -> Mdbs.read3decimal (getAddy()) 0
            | Curr -> Stend.geti (getAddy())
            | Tens -> Stend.getu (getAddy())
        vvls <- Map.add var v vvls
        %% "Var"
        v

    member x.Var = vvls
        
    member x.Stat = stat

    member x.Rele = rele

    member x.ReadStatus() = 
        let v = Mdbs.readStatus (getAddy())
        stat <- Some v
        %% "Stat"
        v

    member x.ReadVar var = readVar var
    
    member x.ReadRele () = 
        let v = Stend.getRele (getAddy ())
        let v' =
            match v with
            | Ok portValue -> 
                STM30.Rele.values
                |> List.filter( fun rele -> STM30.Rele.getState portValue rele )
                |> Set.ofList
                |> Ok
            | Err e -> Err e
        rele <- Some v'
        %% "Rele"
        v

    member x.ReadReleState rele =         
        let v = x.ReadRele()
        match v with
        | Err e -> Err e
        | Ok b -> Rele.getState b rele |> Ok


    member x.Interrogate =
        let v = AppConfig.config.Var
        let (~%%) f = fun () -> Result.someErr (f())
        let conc = %% ( fun () -> readVar Conc)
        let tasks =
            [   (fun () -> v.Conc), conc
                (fun () -> v.Curr), %% ( fun () -> readVar Curr)
                (fun () -> v.Tens), %% ( fun () -> readVar Tens)
                (fun () -> v.Mode), %% ( fun () -> x.ReadStatus())                
                (fun () -> v.Rele), %% ( fun () -> x.ReadRele() ) ]

        fun x ->             
            match List.filter ( fun(f,_) -> f() ) tasks with
            | [] -> [ (fun _ -> true), conc]
            | _::_ as x -> x
            |> List.fold (fun a (canPerform, perform) -> 
                match a with
                | Some x -> Some x
                | _ -> if canPerform() |> not then None else perform x ) None
    member __.Mock() = 
        let rnd = Random()
        for var in VarType.values do
            vvls <- 
                (   if rnd.NextDouble() > 0.5 then
                        let value = 
                            if rnd.NextDouble() > 0.5 then
                                Ok (rnd.NextDouble() |> decimal)
                            else
                                Err "произошла ошибка"                        
                        Map.add var value
                    else 
                        Map.remove var) vvls
            %% "Var"

        rele <- 
            if rnd.NextDouble() > 0.5 then None else
                let v = 
                    if rnd.NextDouble() > 0.5 then Err "error" else
                        (   let bx = [|0uy|]
                            rnd.NextBytes(bx)
                            Rele.getStates bx.[0] |> Ok)
                Some v
        stat <- 
            if rnd.NextDouble() > 0.5 then None else
                let v = 
                    if rnd.NextDouble() > 0.5 then Err "error" else
                        Ok(rnd.Next(), rnd.Next(), "----")
                Some v
        %% "Rele"
        %% "Stat"