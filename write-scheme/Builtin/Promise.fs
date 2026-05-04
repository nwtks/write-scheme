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
        | [ SPromise r, _ ] ->
            match r.Value with
            | true, value -> Ok value |> cont
            | false, thunk ->
                thunk
                |> Eval.apply
                    envs
                    (function
                    | Ok(SPromise r2, p2) ->
                        r.Value <- r2.Value
                        sForce envs p2 cont [ (SPromise r, p2) ]
                    | Ok value ->
                        r.Value <- true, value
                        Ok value |> cont
                    | x -> x |> cont)
                    []
        | [ x ] -> Ok x |> cont
        | x -> x |> invalidParameter pos "'%s' invalid force parameter." |> cont

    let sMakePromise envs pos cont =
        function
        | [ SPromise _, _ as p ] -> Ok p |> cont
        | [ x ] -> Ok(SPromise(ref (true, x)), pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid make-promise parameter." |> cont
