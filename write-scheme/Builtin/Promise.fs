namespace WriteScheme.Builtins

open WriteScheme
open Type

[<AutoOpen>]
module Promise =
    let isPromise envs pos cont =
        function
        | [ SPromise _, _ ] -> Ok(STrue, pos) |> cont
        | _ -> Ok(SFalse, pos) |> cont

    [<TailCall>]
    let rec sForce envs pos cont =
        function
        | [ SPromise promise, _ ] ->
            match promise.Value with
            | true, value -> Ok value |> cont
            | false, thunk ->
                thunk
                |> Eval.apply
                    envs
                    (function
                    | Ok(SPromise r, p) ->
                        promise.Value <- r.Value
                        sForce envs p cont [ (SPromise promise, p) ]
                    | Ok value ->
                        promise.Value <- true, value
                        Ok value |> cont
                    | x -> x |> cont)
                    []
        | [ x ] -> Ok x |> cont
        | x -> x |> invalidParameter pos "'%s' invalid force parameter." |> cont

    let sMakePromise envs pos cont =
        function
        | [ SPromise _, _ as x ] -> Ok x |> cont
        | [ obj ] -> Ok(ref (true, obj) |> SPromise, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid make-promise parameter." |> cont
