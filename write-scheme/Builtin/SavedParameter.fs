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
        | SPair { car = parameter
                  cdr = SPair { car = expression; cdr = SEmpty, _ }, _ },
          _ -> Ok(parameter, expression)
        | x -> x |> invalid (snd x) "'%s' invalid parameterize binding."

    [<TailCall>]
    let rec loopParameterize context pos cont body acc =
        function
        | [] ->
            let triples = acc |> List.rev

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

            let thunk context pos cont _ =
                body |> Eval.evalBody context cont (Ok(SUnspecified, pos))

            doAroundProc context cont (SProcedure before, pos) (SProcedure thunk, pos) (SProcedure after, pos)
        | (param, value) :: parameters ->
            param
            |> Eval.eval context (function
                | Ok(SParameter(paramVal, convOpt), _) ->
                    value
                    |> Eval.eval context (function
                        | Ok newVal ->
                            match convOpt with
                            | Some converter ->
                                converter
                                |> Eval.apply
                                    context
                                    (function
                                    | Ok converted ->
                                        let oldVal = paramVal.Value

                                        parameters
                                        |> loopParameterize
                                            context
                                            pos
                                            cont
                                            body
                                            ((paramVal, ref converted, ref oldVal) :: acc)
                                    | x -> x |> cont)
                                    [ newVal ]
                            | None ->
                                let oldVal = paramVal.Value

                                parameters
                                |> loopParameterize context pos cont body ((paramVal, ref newVal, ref oldVal) :: acc)
                        | x -> x |> cont)
                | Ok x -> x |> invalid (snd x) "'%s' is not a parameter."
                | x -> x |> cont)

    let sMakeParameter context pos cont =
        function
        | [ init ] -> Ok(SParameter(ref init, None), pos) |> cont
        | [ init; converter ] ->
            converter
            |> Eval.apply
                context
                (function
                | Ok converted -> Ok(SParameter(ref converted, Some converter), pos) |> cont
                | x -> x |> cont)
                [ init ]
        | x -> x |> invalidParameter pos "'%s' invalid make-parameter parameter." |> cont
