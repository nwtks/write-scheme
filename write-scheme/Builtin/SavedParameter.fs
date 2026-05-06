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
        | SPair { car = param
                  cdr = SPair { car = expr; cdr = SEmpty, _ }, _ },
          _ -> Ok(param, expr)
        | x -> x |> invalid (snd x) "'%s' invalid parameterize binding."

    [<TailCall>]
    let rec loopParameterize envs pos cont body acc =
        function
        | [] ->
            let triples = List.rev acc

            let before _ pos cont _ =
                triples
                |> List.iter (fun (pv: SExpression ref, nv: SExpression ref, ov: SExpression ref) ->
                    ov.Value <- pv.Value
                    pv.Value <- nv.Value)

                Ok(SUnspecified, pos) |> cont

            let after _ pos cont _ =
                triples
                |> List.iter (fun (pv: SExpression ref, nv: SExpression ref, ov: SExpression ref) ->
                    nv.Value <- pv.Value
                    pv.Value <- ov.Value)

                Ok(SUnspecified, pos) |> cont

            let thunk envs pos cont _ =
                body |> Eval.eachEval envs cont (Ok(SEmpty, pos))

            doAroundProc envs cont (SProcedure before, pos) (SProcedure thunk, pos) (SProcedure after, pos)
        | (param, value) :: parameters ->
            param
            |> Eval.eval envs (function
                | Ok(SParameter(paramVal, convOpt), _) ->
                    value
                    |> Eval.eval envs (function
                        | Ok newVal ->
                            match convOpt with
                            | Some converter ->
                                converter
                                |> Eval.apply
                                    envs
                                    (function
                                    | Ok converted ->
                                        let oldVal = paramVal.Value

                                        parameters
                                        |> loopParameterize
                                            envs
                                            pos
                                            cont
                                            body
                                            ((paramVal, ref converted, ref oldVal) :: acc)
                                    | x -> x |> cont)
                                    [ newVal ]
                            | None ->
                                let oldVal = paramVal.Value

                                parameters
                                |> loopParameterize envs pos cont body ((paramVal, ref newVal, ref oldVal) :: acc)
                        | x -> x |> cont)
                | Ok x -> x |> invalid (snd x) "'%s' is not a parameter."
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
