namespace WriteScheme.Builtins

open WriteScheme
open Type

[<AutoOpen>]
module SavedParameter =
    type SavedParameter =
        { Ref: SExpression ref
          SavedValue: SExpression ref }

    let eachParamBinding =
        function
        | SList [ param; expr ] -> param, expr
        | x -> Print.print x |> sprintf "'%s' invalid parameterize binding." |> failwith

    [<TailCall>]
    let rec bindParameterize envs cont body saved acc =
        function
        | [] ->
            let id = Context.getNextWinderId envs
            let savedRev = List.rev saved

            let swapThunk =
                SProcedure(fun _ cont' _ ->
                    savedRev
                    |> List.iter (fun s ->
                        let tmp = s.Ref.Value
                        s.Ref.Value <- s.SavedValue.Value
                        s.SavedValue.Value <- tmp)

                    SUnspecified |> cont')

            let winder =
                { id = id
                  before = swapThunk
                  after = swapThunk }

            Context.pushWinder envs winder

            body
            |> Eval.eachEval
                envs
                (fun res ->
                    Context.popWinder envs id
                    swapThunk |> Eval.apply envs (fun _ -> cont res) [])
                SEmpty
        | (param, expr) :: xs ->
            param
            |> Eval.eval envs (fun p ->
                expr
                |> Eval.eval envs (fun v ->
                    match p with
                    | SParameter(r, converterOpt) ->
                        match converterOpt with
                        | Some converter ->
                            converter
                            |> Eval.apply
                                envs
                                (fun converted -> setParameterize envs cont body saved acc xs r converted)
                                [ v ]
                        | None -> setParameterize envs cont body saved acc xs r v
                    | x -> Print.print x |> sprintf "'%s' is not a parameter object." |> failwith))

    and [<TailCall>] setParameterize envs cont body saved acc xs r newVal =
        let old = r.Value
        r.Value <- newVal
        let s = { Ref = r; SavedValue = ref old }
        xs |> bindParameterize envs cont body (s :: saved) acc

    let sMakeParameter envs cont =
        function
        | [ init ] -> SParameter(ref init, None) |> cont
        | [ init; converter ] ->
            converter
            |> Eval.apply envs (fun converted -> SParameter(ref converted, Some converter) |> cont) [ init ]
        | x -> x |> invalidParameter "'%s' invalid make-parameter parameter."
