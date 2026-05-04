namespace WriteScheme.Builtins

open WriteScheme
open Type

[<AutoOpen>]
module Exception =
    let sWithExceptionHandler envs pos cont =
        function
        | [ handler; thunk ] ->
            let oldHandler = envs.currentHandler.Value

            let before =
                fun envs pos cont _ ->
                    envs.currentHandler.Value <- handler
                    Ok(SUnspecified, pos) |> cont

            let after =
                fun envs pos cont _ ->
                    envs.currentHandler.Value <- oldHandler
                    Ok(SUnspecified, pos) |> cont

            let thunkProc = fun envs _ cont _ -> thunk |> Eval.apply envs cont []

            sDynamicWind envs pos cont [ SProcedure before, pos; SProcedure thunkProc, pos; SProcedure after, pos ]
        | x ->
            x
            |> invalidParameter pos "'%s' invalid with-exception-handler parameter."
            |> cont

    let sRaise envs pos cont =
        function
        | [ obj ] ->
            Eval.apply
                envs
                (function
                | Ok _ -> Error(EvalError("handler returned", pos)) |> cont
                | Error(SchemeRaise(obj', _)) -> Error(SchemeRaise(obj', pos)) |> cont
                | Error e -> Error e |> cont)
                [ obj ]
                envs.currentHandler.Value
        | x -> x |> invalidParameter pos "'%s' invalid raise parameter." |> cont

    let sError envs pos cont =
        function
        | (SString msg, _) :: irritants -> [ SError(msg, irritants), pos ] |> sRaise envs pos cont
        | x -> x |> invalidParameter pos "'%s' invalid error parameter." |> cont

    let isErrorObject envs pos cont =
        function
        | [ SError _, _ ] -> Ok(STrue, pos) |> cont
        | _ -> Ok(SFalse, pos) |> cont

    let sErrorObjectMessage envs pos cont =
        function
        | [ SError(msg, _), _ ] -> Ok(SString msg, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid error-object-message parameter." |> cont

    let sErrorObjectIrritants envs pos cont =
        function
        | [ SError(_, irritants), _ ] -> Ok(irritants |> toSPair) |> cont
        | x ->
            x
            |> invalidParameter pos "'%s' invalid error-object-irritants parameter."
            |> cont
