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
                    Context.pushHandler envs handler
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

    let sRaise envs pos cont =
        function
        | [ obj ] ->
            let handler = Context.popHandler envs

            Eval.apply
                envs
                (fun res ->
                    Context.pushHandler envs handler

                    match res with
                    | Ok res' -> res' |> Ok |> cont
                    | Error(SchemeRaise(obj', _)) -> SchemeRaise(obj', pos) |> Error |> cont
                    | x -> x |> cont)
                [ obj ]
                handler
        | x -> x |> invalidParameter pos "'%s' invalid raise parameter." |> cont

    let sError envs pos cont =
        function
        | (SString message, _) :: irritants -> [ SError(message, irritants), pos ] |> sRaise envs pos cont
        | x -> x |> invalidParameter pos "'%s' invalid error parameter." |> cont

    let isErrorObject envs pos cont =
        function
        | [ SError _, _ ] -> Ok(STrue, pos) |> cont
        | _ -> Ok(SFalse, pos) |> cont

    let sErrorObjectMessage envs pos cont =
        function
        | [ SError(message, _), _ ] -> Ok(SString message, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid error-object-message parameter." |> cont

    let sErrorObjectIrritants envs pos cont =
        function
        | [ SError(_, irritants), _ ] -> Ok(irritants |> toSPair) |> cont
        | x ->
            x
            |> invalidParameter pos "'%s' invalid error-object-irritants parameter."
            |> cont
