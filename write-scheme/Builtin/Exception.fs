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
                    envs.currentHandler.Value <- handler :: envs.currentHandler.Value
                    Ok(SUnspecified, pos) |> cont

            let after =
                fun envs pos cont _ ->
                    envs.currentHandler.Value <- envs.currentHandler.Value.Tail
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
            match envs.currentHandler.Value with
            | handler :: parents ->
                envs.currentHandler.Value <- parents

                Eval.apply
                    envs
                    (fun res ->
                        envs.currentHandler.Value <- handler :: parents

                        match res with
                        | Ok res' -> res' |> Ok |> cont
                        | Error(SchemeRaise(obj', _)) -> SchemeRaise(obj', pos) |> Error |> cont
                        | Error e -> Error e |> cont)
                    [ obj ]
                    handler
            | [] -> failwith "unreachable"
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
