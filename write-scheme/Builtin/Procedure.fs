namespace WriteScheme.Builtins

open WriteScheme
open Type

[<AutoOpen>]
module Procedure =
    let isProcedure envs pos cont =
        function
        | [ SProcedure _, _ ]
        | [ SContinuation _, _ ]
        | [ SParameter _, _ ] -> Ok(STrue, pos) |> cont
        | [ _ ] -> Ok(SFalse, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid procedure? parameter." |> cont

    [<TailCall>]
    let rec foldApply pos args =
        function
        | acc, [ SEmpty, _ ] -> acc |> List.rev |> Ok
        | acc, [ list ] when isProperList list -> list |> toList |> Result.map (fun l -> (acc |> List.rev) @ l)
        | _, [ _ ]
        | _, [] -> args |> invalidParameter pos "'%s' invalid apply parameter."
        | acc, h :: t -> (h :: acc, t) |> foldApply pos args

    let sApply envs pos cont =
        function
        | proc :: args ->
            match ([], args) |> foldApply (snd proc) args with
            | Ok flatArgs -> proc |> Eval.apply envs cont flatArgs
            | Error e -> Error e |> cont
        | x -> x |> invalidParameter pos "'%s' invalid apply parameter." |> cont

    [<TailCall>]
    let rec foldTranspose acc =
        function
        | 0, _
        | _, [] -> List.rev acc
        | n, lists -> foldTranspose ((lists |> List.map List.head) :: acc) (n - 1, lists |> List.map List.tail)

    let transposeList lists =
        (lists |> List.map List.length |> Seq.min, lists) |> foldTranspose []

    [<TailCall>]
    let rec mapMap envs cont proc acc =
        function
        | [] -> List.rev acc |> toSPair |> Ok |> cont
        | list :: lists ->
            proc
            |> Eval.apply
                envs
                (function
                | Ok a -> lists |> mapMap envs cont proc (a :: acc)
                | x -> x |> cont)
                list

    let sMap envs pos cont =
        function
        | [ _ ] as x -> x |> invalidParameter pos "'%s' invalid map parameter." |> cont
        | proc :: lists ->
            lists
            |> mapResult (function
                | SEmpty, _ -> Ok []
                | list when isProperList list -> list |> toList
                | x -> x |> invalid (snd x) "'%s' invalid map parameter.")
            |> function
                | Ok lists' -> lists' |> transposeList |> mapMap envs cont proc []
                | Error e -> Error e |> cont
        | x -> x |> invalidParameter pos "'%s' invalid map parameter." |> cont

    [<TailCall>]
    let rec mapStringMap envs pos cont proc acc =
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
                envs
                (function
                | Ok a -> strings |> mapStringMap envs pos cont proc (a :: acc)
                | x -> x |> cont)
                str

    let sStringMap envs pos cont =
        function
        | [ _ ] as x -> x |> invalidParameter pos "'%s' invalid string-map parameter." |> cont
        | proc :: strings ->
            strings
            |> mapResult (function
                | SString s, _ -> s.runes |> Array.map (fun c -> SChar c, pos) |> Array.toList |> Ok
                | x -> x |> invalid (snd x) "'%s' invalid string-map parameter.")
            |> function
                | Ok strings' -> strings' |> transposeList |> mapStringMap envs (snd proc) cont proc []
                | Error e -> Error e |> cont
        | x -> x |> invalidParameter pos "'%s' invalid string-map parameter." |> cont

    [<TailCall>]
    let rec mapVectorMap envs pos cont proc acc =
        function
        | [] -> acc |> List.rev |> List.toArray |> SVector |> (fun x -> x, pos) |> Ok |> cont
        | vector :: vectors ->
            proc
            |> Eval.apply
                envs
                (function
                | Ok a -> vectors |> mapVectorMap envs pos cont proc (a :: acc)
                | x -> x |> cont)
                vector

    let sVectorMap envs pos cont =
        function
        | [ _ ] as x -> x |> invalidParameter pos "'%s' invalid vector-map parameter." |> cont
        | proc :: vectors ->
            vectors
            |> mapResult (function
                | SVector vector, _ -> Array.toList vector |> Ok
                | x -> x |> invalid (snd x) "'%s' invalid vector-map parameter.")
            |> function
                | Ok vectors' -> vectors' |> transposeList |> mapVectorMap envs (snd proc) cont proc []
                | Error e -> Error e |> cont
        | x -> x |> invalidParameter pos "'%s' invalid vector-map parameter." |> cont

    [<TailCall>]
    let rec loopForEach envs pos cont proc =
        function
        | [] -> Ok(SEmpty, pos) |> cont
        | list :: lists ->
            proc
            |> Eval.apply
                envs
                (function
                | Ok _ -> lists |> loopForEach envs pos cont proc
                | x -> x |> cont)
                list

    let sForEach envs pos cont =
        function
        | [ _ ] as x -> x |> invalidParameter pos "'%s' invalid for-each parameter." |> cont
        | proc :: lists ->
            lists
            |> mapResult (function
                | SEmpty, _ -> Ok []
                | list when isProperList list -> list |> toList
                | x -> x |> invalid (snd x) "'%s' invalid for-each parameter.")
            |> function
                | Ok lists' -> lists' |> transposeList |> loopForEach envs (snd proc) cont proc
                | Error e -> Error e |> cont
        | x -> x |> invalidParameter pos "'%s' invalid for-each parameter." |> cont

    let sStringForEach envs pos cont =
        function
        | [ _ ] as x -> x |> invalidParameter pos "'%s' invalid string-for-each parameter." |> cont
        | proc :: strings ->
            strings
            |> mapResult (function
                | SString s, _ -> s.runes |> Array.map (fun c -> SChar c, pos) |> Array.toList |> Ok
                | x -> x |> invalid (snd x) "'%s' invalid string-for-each parameter.")
            |> function
                | Ok strings' -> strings' |> transposeList |> loopForEach envs (snd proc) cont proc
                | Error e -> Error e |> cont
        | x -> x |> invalidParameter pos "'%s' invalid string-for-each parameter." |> cont

    let sVectorForEach envs pos cont =
        function
        | [ _ ] as x -> x |> invalidParameter pos "'%s' invalid vector-for-each parameter." |> cont
        | proc :: vectors ->
            vectors
            |> mapResult (function
                | SVector vector, _ -> Array.toList vector |> Ok
                | x -> x |> invalid (snd x) "'%s' invalid vector-for-each parameter.")
            |> function
                | Ok vectors' -> vectors' |> transposeList |> loopForEach envs (snd proc) cont proc
                | Error e -> Error e |> cont
        | x -> x |> invalidParameter pos "'%s' invalid vector-for-each parameter." |> cont

    let sCallCC envs pos cont =
        function
        | [ proc ] ->
            let capturedWinders = envs.winders.Value

            let wrappedCont arg =
                if envs.winders.Value = capturedWinders then
                    cont arg
                else
                    doWind envs cont capturedWinders arg

            proc |> Eval.apply envs cont [ SContinuation wrappedCont, pos ]
        | x -> x |> invalidParameter pos "'%s' invalid call/cc parameter." |> cont

    let sValues envs pos cont =
        function
        | [ obj ] -> obj |> Ok |> cont
        | values -> (SValues values, pos) |> Ok |> cont

    let sCallWithValues envs pos cont =
        function
        | [ producer; consumer ] ->
            producer
            |> Eval.apply
                envs
                (function
                | Ok(SValues values, _) -> consumer |> Eval.apply envs cont values
                | Ok obj -> consumer |> Eval.apply envs cont [ obj ]
                | x -> x |> cont)
                []
        | x -> x |> invalidParameter pos "'%s' invalid call-with-values parameter." |> cont

    let rec sDynamicWind envs pos cont =
        function
        | [ before; thunk; after ] -> doAroundProc envs cont before thunk after
        | x -> x |> invalidParameter pos "'%s' invalid dynamic-wind parameter." |> cont
