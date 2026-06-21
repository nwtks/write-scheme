namespace WriteScheme.Builtins

open WriteScheme
open Type

[<AutoOpen>]
module DynamicBinding =
    type SavedParameter =
        { Ref: SExpression ref
          SavedValue: SExpression ref }

    let eachParamBinding =
        function
        | SPair { car = parameter
                  cdr = SPair { car = expression; cdr = SEmpty, _ }, _ },
          _ -> Ok(parameter, expression)
        | x -> x |> invalid (snd x) "'%s' invalid parameterize binding."

    let parseParamBindings bindings =
        bindings |> toList |> Result.bind (mapResult eachParamBinding)

    let doParameterize context pos cont body acc =
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

    [<TailCall>]
    let rec loopParameterize context pos cont body acc =
        function
        | [] -> doParameterize context pos cont body acc
        | (param, value) :: parameters ->
            param
            |> Eval.eval context (function
                | Ok(SParameter(paramVal, converter), _) ->
                    evalParameterize context pos cont body acc paramVal converter value parameters
                | Ok x -> x |> invalid (snd x) "'%s' is not a parameter."
                | x -> x |> cont)

    and [<TailCall>] evalParameterize context pos cont body acc (paramVal: SExpression ref) convOpt value parameters =
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
                            |> loopParameterize context pos cont body ((paramVal, ref converted, ref oldVal) :: acc)
                        | x -> x |> cont)
                        [ newVal ]
                | None ->
                    let oldVal = paramVal.Value

                    parameters
                    |> loopParameterize context pos cont body ((paramVal, ref newVal, ref oldVal) :: acc)
            | x -> x |> cont)

    let sParameterize context pos cont =
        function
        | parameters :: body ->
            match parameters |> parseParamBindings with
            | Ok parameters' -> parameters' |> loopParameterize context pos cont body []
            | Error e -> Error e |> cont
        | x -> x |> invalidParameter pos "'%s' invalid parameterize parameter." |> cont

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
