namespace WriteScheme.Builtins

open WriteScheme
open Type

[<AutoOpen>]
module Exception =
    let sWithExceptionHandler envs cont =
        function
        | [ handlerProc; thunkProc ] ->
            let savedWinders = envs.currentWinders.Value

            try
                thunkProc |> Eval.apply envs cont []
            with SchemeRaise obj ->
                handlerProc
                |> Eval.apply envs (fun res -> doWind envs (fun _ -> cont res) savedWinders) [ SQuote obj ]
        | x -> x |> invalidParameter "'%s' invalid with-exception-handler parameter."

    let sRaise envs cont =
        function
        | [ obj ] -> raise (SchemeRaise obj)
        | x -> x |> invalidParameter "'%s' invalid raise parameter."

    let sError envs cont =
        function
        | SString msg :: irritants -> raise (SchemeRaise(SError(msg, irritants)))
        | x -> x |> invalidParameter "'%s' invalid error parameter."

    let isErrorObject envs cont =
        function
        | [ SError _ ] -> STrue |> cont
        | _ -> SFalse |> cont

    let sErrorObjectMessage envs cont =
        function
        | [ SError(msg, _) ] -> SString msg |> cont
        | x -> x |> invalidParameter "'%s' invalid error-object-message parameter."

    let sErrorObjectIrritants envs cont =
        function
        | [ SError(_, irritants) ] -> irritants |> toSList |> cont
        | x -> x |> invalidParameter "'%s' invalid error-object-irritants parameter."
