namespace WriteScheme.Builtins

open WriteScheme
open Type

[<AutoOpen>]
module SavedParameter =
    type SavedParameter =
        { Ref: SExpression ref
          SavedValue: SExpression ref }

    let eachSavedParamBinding =
        function
        | SPair { car = param
                  cdr = SPair { car = expr; cdr = SEmpty, _ }, _ },
          _ -> Ok(param, expr)
        | x -> x |> invalid (snd x) "'%s' invalid parameterize binding."

    [<TailCall>]
    let rec bindParameterizeSaved envs pos cont body saved =
        function
        | [] ->
            let id = Context.getNextWinderId envs
            let savedRev = List.rev saved

            let swapThunk =
                SProcedure(fun _ pos' cont' _ ->
                    savedRev
                    |> List.iter (fun s ->
                        let tmp = s.Ref.Value
                        s.Ref.Value <- s.SavedValue.Value
                        s.SavedValue.Value <- tmp)

                    Ok(SUnspecified, pos') |> cont'),
                pos

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
                (Ok(SEmpty, pos))
        | (param, expr) :: xs ->
            param
            |> Eval.eval envs (function
                | Ok(SParameter(r, converterOpt), _) ->
                    expr
                    |> Eval.eval envs (function
                        | Ok v ->
                            match converterOpt with
                            | Some converter ->
                                converter
                                |> Eval.apply
                                    envs
                                    (function
                                    | Ok converted ->
                                        let old = r.Value
                                        r.Value <- converted
                                        let s = { Ref = r; SavedValue = ref old }
                                        bindParameterizeSaved envs (snd converter) cont body (s :: saved) xs
                                    | x -> x |> cont)
                                    [ v ]
                            | None ->
                                let old = r.Value
                                r.Value <- v
                                let s = { Ref = r; SavedValue = ref old }
                                bindParameterizeSaved envs pos cont body (s :: saved) xs
                        | x -> x |> cont)
                | Ok _ ->
                    Error(EvalError(sprintf "'%s' is not a parameter in parameterize." (Print.print param), snd param))
                    |> cont
                | x -> x |> cont)

    let sMakeParameter envs pos cont =
        function
        | [ init ] -> Ok(SParameter(ref init, None), pos) |> cont
        | [ init; converter ] ->
            converter
            |> Eval.apply
                envs
                (function
                | Ok converted -> Ok(SParameter(ref converted, Some converter), pos) |> cont
                | x -> x |> cont)
                [ init ]
        | x -> x |> invalidParameter pos "'%s' invalid make-parameter parameter." |> cont
