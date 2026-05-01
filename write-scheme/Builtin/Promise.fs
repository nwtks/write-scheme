namespace WriteScheme.Builtins

open WriteScheme
open Type

[<AutoOpen>]
module Promise =
    let isPromise envs pos cont =
        function
        | [ SPromise _, _ ] -> (STrue, pos) |> cont
        | _ -> (SFalse, pos) |> cont

    [<TailCall>]
    let rec sForce envs pos cont =
        function
        | [ SPromise r, _ ] ->
            match r.Value with
            | true, value -> value |> cont
            | false, thunk ->
                thunk
                |> Eval.apply
                    envs
                    (function
                    | SPromise r2, p2 ->
                        r.Value <- r2.Value
                        sForce envs p2 cont [ (SPromise r, p2) ]
                    | value ->
                        r.Value <- true, value
                        value |> cont)
                    []
        | [ x ] -> x |> cont
        | x -> x |> invalidParameter pos "'%s' invalid force parameter."

    let sMakePromise envs pos cont =
        function
        | [ SPromise _, _ as p ] -> p |> cont
        | [ x ] -> (SPromise(ref (true, x)), pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid make-promise parameter."
