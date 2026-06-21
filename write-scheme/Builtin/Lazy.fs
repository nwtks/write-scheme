namespace WriteScheme.Builtins

open WriteScheme
open Type

[<AutoOpen>]
module Lazy =
    let makeLazyPromise context pos expression =
        let thunk = closure context (SEmpty, pos) [ expression ]
        SPromise(ref (false, (SProcedure thunk, pos))), pos

    let sDelay context pos cont =
        function
        | [ expression ] -> expression |> makeLazyPromise context pos |> Ok |> cont
        | x -> x |> invalidParameter pos "'%s' invalid delay parameter." |> cont

    let sDelayForce context pos cont =
        function
        | [ expression ] -> expression |> makeLazyPromise context pos |> Ok |> cont
        | x -> x |> invalidParameter pos "'%s' invalid delay-force parameter." |> cont

    let isPromise context pos cont =
        function
        | [ SPromise _, _ ] -> Ok(STrue, pos) |> cont
        | _ -> Ok(SFalse, pos) |> cont

    [<TailCall>]
    let rec sForce context pos cont =
        function
        | [ SPromise promise, _ ] ->
            match promise.Value with
            | true, value -> Ok value |> cont
            | false, thunk ->
                thunk
                |> Eval.apply
                    context
                    (function
                    | Ok(SPromise r, _) ->
                        promise.Value <- r.Value
                        sForce context pos cont [ (SPromise promise, pos) ]
                    | Ok value ->
                        promise.Value <- true, value
                        Ok value |> cont
                    | x -> x |> cont)
                    []
        | [ x ] -> Ok x |> cont
        | x -> x |> invalidParameter pos "'%s' invalid force parameter." |> cont

    let sMakePromise context pos cont =
        function
        | [ SPromise _, _ as x ] -> Ok x |> cont
        | [ obj ] -> Ok(ref (true, obj) |> SPromise, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid make-promise parameter." |> cont
