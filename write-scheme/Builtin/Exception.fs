namespace WriteScheme.Builtins

open WriteScheme
open Type

[<AutoOpen>]
module Exception =
    let sWithExceptionHandler context pos cont =
        function
        | [ handler; thunk ] ->
            let before =
                fun context pos cont _ ->
                    handler |> Context.pushHandler context
                    Ok(SUnspecified, pos) |> cont

            let after =
                fun context pos cont _ ->
                    Context.popHandler context |> ignore
                    Ok(SUnspecified, pos) |> cont

            let thunk' = fun context _ cont _ -> thunk |> Eval.apply context cont []
            doAroundProc context cont (SProcedure before, pos) (SProcedure thunk', pos) (SProcedure after, pos)
        | x ->
            x
            |> invalidParameter pos "'%s' invalid with-exception-handler parameter."
            |> cont

    let doRaise continuable context pos cont obj =
        let handler = Context.popHandler context

        handler
        |> Eval.apply
            context
            (fun res ->
                handler |> Context.pushHandler context

                match res with
                | Ok _ when not continuable -> EvalError("Exception handler returned.", pos) |> Error |> cont
                | _ -> res |> cont)
            [ obj ]

    let sRaise context pos cont =
        function
        | [ obj ] -> obj |> doRaise false context pos cont
        | x -> x |> invalidParameter pos "'%s' invalid raise parameter." |> cont

    let sRaiseContinuable context pos cont =
        function
        | [ obj ] -> obj |> doRaise true context pos cont
        | x -> x |> invalidParameter pos "'%s' invalid raise-continuable parameter." |> cont

    let sError context pos cont =
        function
        | (SString message, _) :: irritants -> [ SError(message, irritants), pos ] |> sRaise context pos cont
        | x -> x |> invalidParameter pos "'%s' invalid error parameter." |> cont

    let isErrorObject context pos cont =
        function
        | [ SError _, _ ] -> Ok(STrue, pos) |> cont
        | [ _ ] -> Ok(SFalse, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid error-object? parameter." |> cont

    let sErrorObjectMessage context pos cont =
        function
        | [ SError(message, _), _ ] -> Ok(SString message, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid error-object-message parameter." |> cont

    let sErrorObjectIrritants context pos cont =
        function
        | [ SError(_, irritants), _ ] -> irritants |> toSPair |> Ok |> cont
        | x ->
            x
            |> invalidParameter pos "'%s' invalid error-object-irritants parameter."
            |> cont
