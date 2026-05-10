namespace WriteScheme.Builtins

open WriteScheme
open Type

[<AutoOpen>]
module Exception =
    let sWithExceptionHandler envs pos cont =
        function
        | [ handler; thunk ] ->
            let before =
                fun envs pos cont _ ->
                    handler |> Context.pushHandler envs
                    Ok(SUnspecified, pos) |> cont

            let after =
                fun envs pos cont _ ->
                    Context.popHandler envs |> ignore
                    Ok(SUnspecified, pos) |> cont

            let thunk' = fun envs _ cont _ -> thunk |> Eval.apply envs cont []
            doAroundProc envs cont (SProcedure before, pos) (SProcedure thunk', pos) (SProcedure after, pos)
        | x ->
            x
            |> invalidParameter pos "'%s' invalid with-exception-handler parameter."
            |> cont

    let doRaise continuable envs pos cont obj =
        let handler = Context.popHandler envs

        Eval.apply
            envs
            (fun res ->
                handler |> Context.pushHandler envs

                match res with
                | Ok _ when not continuable -> EvalError("Exception handler returned.", pos) |> Error |> cont
                | _ -> res |> cont)
            [ obj ]
            handler

    let sRaise envs pos cont =
        function
        | [ obj ] -> obj |> doRaise false envs pos cont
        | x -> x |> invalidParameter pos "'%s' invalid raise parameter." |> cont

    let sRaiseContinuable envs pos cont =
        function
        | [ obj ] -> obj |> doRaise true envs pos cont
        | x -> x |> invalidParameter pos "'%s' invalid raise-continuable parameter." |> cont

    let sError envs pos cont =
        function
        | (SString message, _) :: irritants -> [ SError(message, irritants), pos ] |> sRaise envs pos cont
        | x -> x |> invalidParameter pos "'%s' invalid error parameter." |> cont

    let isErrorObject envs pos cont =
        function
        | [ SError _, _ ] -> Ok(STrue, pos) |> cont
        | [ _ ] -> Ok(SFalse, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid error-object? parameter." |> cont

    let sErrorObjectMessage envs pos cont =
        function
        | [ SError(message, _), _ ] -> Ok(SString message, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid error-object-message parameter." |> cont

    let sErrorObjectIrritants envs pos cont =
        function
        | [ SError(_, irritants), _ ] -> irritants |> toSPair |> Ok |> cont
        | x ->
            x
            |> invalidParameter pos "'%s' invalid error-object-irritants parameter."
            |> cont
