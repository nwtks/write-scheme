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
    let rec loopParameterize envs pos cont body triples =
        function
        | [] ->
            let triples = List.rev triples

            let before _ pos cont _ =
                triples
                |> List.iter (fun (r: SExpression ref, nv: SExpression ref, ov: SExpression ref) ->
                    ov.Value <- r.Value
                    r.Value <- nv.Value)

                Ok(SUnspecified, pos) |> cont

            let after _ pos cont _ =
                triples
                |> List.iter (fun (r: SExpression ref, nv: SExpression ref, ov: SExpression ref) ->
                    nv.Value <- r.Value
                    r.Value <- ov.Value)

                Ok(SUnspecified, pos) |> cont

            let thunk envs pos cont _ =
                body |> Eval.eachEval envs cont (Ok(SEmpty, pos))

            sDynamicWind envs pos cont [ SProcedure before, pos; SProcedure thunk, pos; SProcedure after, pos ]
        | (pExpr, vExpr) :: rest ->
            pExpr
            |> Eval.eval envs (function
                | Ok(SParameter(r, convOpt), _) ->
                    vExpr
                    |> Eval.eval envs (function
                        | Ok newVal ->
                            match convOpt with
                            | Some conv ->
                                conv
                                |> Eval.apply
                                    envs
                                    (function
                                    | Ok converted ->
                                        let oldVal = r.Value

                                        rest
                                        |> loopParameterize
                                            envs
                                            pos
                                            cont
                                            body
                                            ((r, ref converted, ref oldVal) :: triples)
                                    | x -> x |> cont)
                                    [ newVal ]
                            | None ->
                                let oldVal = r.Value

                                rest
                                |> loopParameterize envs pos cont body ((r, ref newVal, ref oldVal) :: triples)
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
