namespace WriteScheme.Builtins

open WriteScheme
open Type

[<AutoOpen>]
module Procedure =
    let isProcedure context pos cont =
        function
        | [ SProcedure _, _ ]
        | [ SContinuation _, _ ]
        | [ SParameter _, _ ] -> Ok(STrue, pos) |> cont
        | [ _ ] -> Ok(SFalse, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid procedure? parameter." |> cont

    let coerceList name =
        function
        | SEmpty, _ -> Ok []
        | list when list |> isProperList -> list |> toList
        | x -> x |> invalid (snd x) ($"'%%s' invalid {name} parameter.")

    let coerceString pos name =
        function
        | SString s, _ -> s.runes |> Array.map (fun c -> SChar c, pos) |> Array.toList |> Ok
        | x -> x |> invalid (snd x) ($"'%%s' invalid {name} parameter.")

    let coerceVector name =
        function
        | SVector vector, _ -> Array.toList vector |> Ok
        | x -> x |> invalid (snd x) ($"'%%s' invalid {name} parameter.")

    [<TailCall>]
    let rec foldApply pos args =
        function
        | acc, [ SEmpty, _ ] -> acc |> List.rev |> Ok
        | acc, [ list ] when list |> isProperList -> list |> toList |> Result.map (fun l -> (acc |> List.rev) @ l)
        | _, [ _ ]
        | _, [] -> args |> invalidParameter pos "'%s' invalid apply parameter."
        | acc, h :: t -> (h :: acc, t) |> foldApply pos args

    let sApply context pos cont =
        function
        | proc :: args ->
            match ([], args) |> foldApply (snd proc) args with
            | Ok flatArgs -> proc |> Eval.apply context cont flatArgs
            | Error e -> Error e |> cont
        | x -> x |> invalidParameter pos "'%s' invalid apply parameter." |> cont

    [<TailCall>]
    let rec foldTranspose acc =
        function
        | 0, _
        | _, [] -> acc |> List.rev
        | n, lists -> foldTranspose ((lists |> List.map List.head) :: acc) (n - 1, lists |> List.map List.tail)

    let transposeList lists =
        (lists |> List.map List.length |> Seq.min, lists) |> foldTranspose []

    [<TailCall>]
    let rec mapMap context cont proc acc =
        function
        | [] -> acc |> List.rev |> toSPair |> Ok |> cont
        | list :: lists ->
            proc
            |> Eval.apply
                context
                (function
                | Ok a -> lists |> mapMap context cont proc (a :: acc)
                | x -> x |> cont)
                list

    let sMap context pos cont =
        function
        | [ _ ] as x -> x |> invalidParameter pos "'%s' invalid map parameter." |> cont
        | proc :: lists ->
            lists
            |> mapResult (coerceList "map")
            |> function
                | Ok lists' -> lists' |> transposeList |> mapMap context cont proc []
                | Error e -> Error e |> cont
        | x -> x |> invalidParameter pos "'%s' invalid map parameter." |> cont

    [<TailCall>]
    let rec mapStringMap context pos cont proc acc =
        function
        | [] ->
            acc
            |> List.rev
            |> mapResult (function
                | SChar c, _ -> Ok c
                | x -> x |> invalid (snd x) "'%s' is not a char in string-map.")
            |> Result.map (fun runes ->
                { runes = runes |> List.toArray
                  isImmutable = false }
                |> SString,
                pos)
            |> cont
        | str :: strings ->
            proc
            |> Eval.apply
                context
                (function
                | Ok a -> strings |> mapStringMap context pos cont proc (a :: acc)
                | x -> x |> cont)
                str

    let sStringMap context pos cont =
        function
        | [ _ ] as x -> x |> invalidParameter pos "'%s' invalid string-map parameter." |> cont
        | proc :: strings ->
            strings
            |> mapResult (coerceString pos "string-map")
            |> function
                | Ok strings' -> strings' |> transposeList |> mapStringMap context (snd proc) cont proc []
                | Error e -> Error e |> cont
        | x -> x |> invalidParameter pos "'%s' invalid string-map parameter." |> cont

    [<TailCall>]
    let rec mapVectorMap context pos cont proc acc =
        function
        | [] -> acc |> List.rev |> List.toArray |> SVector |> (fun x -> x, pos) |> Ok |> cont
        | vector :: vectors ->
            proc
            |> Eval.apply
                context
                (function
                | Ok a -> vectors |> mapVectorMap context pos cont proc (a :: acc)
                | x -> x |> cont)
                vector

    let sVectorMap context pos cont =
        function
        | [ _ ] as x -> x |> invalidParameter pos "'%s' invalid vector-map parameter." |> cont
        | proc :: vectors ->
            vectors
            |> mapResult (coerceVector "vector-map")
            |> function
                | Ok vectors' -> vectors' |> transposeList |> mapVectorMap context (snd proc) cont proc []
                | Error e -> Error e |> cont
        | x -> x |> invalidParameter pos "'%s' invalid vector-map parameter." |> cont

    [<TailCall>]
    let rec loopForEach context pos cont proc =
        function
        | [] -> Ok(SEmpty, pos) |> cont
        | list :: lists ->
            proc
            |> Eval.apply
                context
                (function
                | Ok _ -> lists |> loopForEach context pos cont proc
                | x -> x |> cont)
                list

    let sForEach context pos cont =
        function
        | [ _ ] as x -> x |> invalidParameter pos "'%s' invalid for-each parameter." |> cont
        | proc :: lists ->
            lists
            |> mapResult (coerceList "for-each")
            |> function
                | Ok lists' -> lists' |> transposeList |> loopForEach context (snd proc) cont proc
                | Error e -> Error e |> cont
        | x -> x |> invalidParameter pos "'%s' invalid for-each parameter." |> cont

    let sStringForEach context pos cont =
        function
        | [ _ ] as x -> x |> invalidParameter pos "'%s' invalid string-for-each parameter." |> cont
        | proc :: strings ->
            strings
            |> mapResult (coerceString pos "string-for-each")
            |> function
                | Ok strings' -> strings' |> transposeList |> loopForEach context (snd proc) cont proc
                | Error e -> Error e |> cont
        | x -> x |> invalidParameter pos "'%s' invalid string-for-each parameter." |> cont

    let sVectorForEach context pos cont =
        function
        | [ _ ] as x -> x |> invalidParameter pos "'%s' invalid vector-for-each parameter." |> cont
        | proc :: vectors ->
            vectors
            |> mapResult (coerceVector "vector-for-each")
            |> function
                | Ok vectors' -> vectors' |> transposeList |> loopForEach context (snd proc) cont proc
                | Error e -> Error e |> cont
        | x -> x |> invalidParameter pos "'%s' invalid vector-for-each parameter." |> cont

    let sCallCC context pos cont =
        function
        | [ proc ] ->
            let capturedWinders = context.winders.Value

            let wrappedCont arg =
                if context.winders.Value = capturedWinders then
                    cont arg
                else
                    doWind context cont capturedWinders arg

            proc |> Eval.apply context cont [ SContinuation wrappedCont, pos ]
        | x -> x |> invalidParameter pos "'%s' invalid call/cc parameter." |> cont

    let sValues context pos cont =
        function
        | [ obj ] -> obj |> Ok |> cont
        | values -> (SValues values, pos) |> Ok |> cont

    let sCallWithValues context pos cont =
        function
        | [ producer; consumer ] ->
            producer
            |> Eval.apply
                context
                (function
                | Ok(SValues values, _) -> consumer |> Eval.apply context cont values
                | Ok obj -> consumer |> Eval.apply context cont [ obj ]
                | x -> x |> cont)
                []
        | x -> x |> invalidParameter pos "'%s' invalid call-with-values parameter." |> cont

    let rec sDynamicWind context pos cont =
        function
        | [ before; thunk; after ] -> doAroundProc context cont before thunk after
        | x -> x |> invalidParameter pos "'%s' invalid dynamic-wind parameter." |> cont
