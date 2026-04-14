namespace WriteScheme.Builtins

open WriteScheme
open Type

[<AutoOpen>]
module DynamicWind =
    let isPromise envs cont =
        function
        | [ SPromise _ ] -> STrue |> cont
        | _ -> SFalse |> cont

    [<TailCall>]
    let rec sForce envs cont =
        function
        | [ SPromise r ] ->
            match r.Value with
            | true, value -> value |> cont
            | false, thunk ->
                thunk
                |> Eval.apply
                    envs
                    (function
                    | SPromise r2 ->
                        r.Value <- r2.Value
                        sForce envs cont [ SPromise r ]
                    | value ->
                        r.Value <- true, value
                        value |> cont)
                    []
        | [ x ] -> x |> cont
        | x -> x |> invalidParameter "'%s' invalid force parameter."

    let sMakePromise envs cont =
        function
        | [ SPromise _ as p ] -> p |> cont
        | [ x ] -> SPromise(ref (true, x)) |> cont
        | x -> x |> invalidParameter "'%s' invalid make-promise parameter."

    let sMakeParameter envs cont =
        function
        | [ init ] -> SParameter(ref init, None) |> cont
        | [ init; converter ] ->
            converter
            |> Eval.apply envs (fun converted -> SParameter(ref converted, Some converter) |> cont) [ init ]
        | x -> x |> invalidParameter "'%s' invalid make-parameter parameter."
