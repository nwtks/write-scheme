namespace WriteScheme.Builtins

open WriteScheme
open Type

[<AutoOpen>]
module Exception =
    let sWithExceptionHandler envs pos cont =
        function
        | [ handlerProc; thunkProc ] ->
            let savedWinders = envs.currentWinders.Value

            try
                thunkProc |> Eval.apply envs cont []
            with SchemeRaise obj ->
                handlerProc
                |> Eval.apply envs (fun res -> doWind envs (fun _ -> cont res) savedWinders) [ SQuote obj, pos ]
        | x -> x |> invalidParameter pos "'%s' invalid with-exception-handler parameter."

    let sRaise envs pos cont =
        function
        | [ obj ] -> SchemeRaise obj |> raise
        | x -> x |> invalidParameter pos "'%s' invalid raise parameter."

    let sError envs pos cont =
        function
        | (SString msg, _) :: irritants -> SchemeRaise(SError(msg, irritants), pos) |> raise
        | x -> x |> invalidParameter pos "'%s' invalid error parameter."

    let isErrorObject envs pos cont =
        function
        | [ SError _, _ ] -> (STrue, pos) |> cont
        | _ -> (SFalse, pos) |> cont

    let sErrorObjectMessage envs pos cont =
        function
        | [ SError(msg, _), _ ] -> (SString msg, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid error-object-message parameter."

    let sErrorObjectIrritants envs pos cont =
        function
        | [ SError(_, irritants), _ ] -> irritants |> toSPair |> cont
        | x -> x |> invalidParameter pos "'%s' invalid error-object-irritants parameter."
