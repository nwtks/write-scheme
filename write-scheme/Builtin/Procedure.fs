namespace WriteScheme.Builtins

open WriteScheme
open Type

[<AutoOpen>]
module Procedure =
    let isProcedure envs pos cont =
        function
        | [ SSyntax _, _ ]
        | [ SProcedure _, _ ]
        | [ SContinuation _, _ ] -> Ok(STrue, pos) |> cont
        | _ -> Ok(SFalse, pos) |> cont

    [<TailCall>]
    let rec foldApply pos xs =
        function
        | acc, [ SEmpty, _ ] -> acc |> List.rev |> Ok
        | acc, [ x ] when isProperList x -> x |> toList |> Result.map (fun l -> (acc |> List.rev) @ l)
        | _, [ _ ]
        | _, [] -> xs |> invalidParameter pos "'%s' invalid apply parameter."
        | acc, x1 :: x2 -> (x1 :: acc, x2) |> foldApply pos xs

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
        | n, xs -> foldTranspose ((xs |> List.map List.head) :: acc) (n - 1, xs |> List.map List.tail)

    let transposeList lists =
        foldTranspose [] (lists |> List.map List.length |> Seq.min, lists)

    [<TailCall>]
    let rec mapMap envs cont proc acc =
        function
        | [] -> List.rev acc |> toSPair |> Ok |> cont
        | x :: xs ->
            proc
            |> Eval.apply
                envs
                (function
                | Ok a -> xs |> mapMap envs cont proc (a :: acc)
                | x -> x |> cont)
                x

    let sMap envs pos cont =
        function
        | [ _ ] as x -> x |> invalidParameter pos "'%s' invalid map parameter." |> cont
        | proc :: lists ->
            lists
            |> mapResult (function
                | SEmpty, _ -> Ok []
                | x when isProperList x -> x |> toList
                | x -> x |> invalid (snd x) "'%s' invalid map parameter.")
            |> function
                | Ok lists' -> lists' |> transposeList |> mapMap envs cont proc []
                | Error e -> Error e |> cont
        | x -> x |> invalidParameter pos "'%s' invalid map parameter." |> cont

    [<TailCall>]
    let rec mapStringMap envs pos cont proc acc =
        function
        | [] ->
            List.rev acc
            |> mapResult (function
                | SChar c, _ -> Ok c
                | x -> x |> invalid (snd x) "'%s' is not a char in string-map.")
            |> Result.map (fun runes ->
                { runes = runes |> List.toArray
                  isImmutable = false }
                |> SString,
                pos)
            |> cont
        | x :: xs ->
            proc
            |> Eval.apply
                envs
                (function
                | Ok a -> xs |> mapStringMap envs pos cont proc (a :: acc)
                | x -> x |> cont)
                x

    let sStringMap envs pos cont =
        function
        | [ _ ] as x -> x |> invalidParameter pos "'%s' invalid string-map parameter." |> cont
        | proc :: strings ->
            strings
            |> mapResult (function
                | SString s, _ -> s.runes |> Array.map (fun c -> SChar c, pos) |> Array.toList |> Ok
                | x -> x |> invalid (snd x) "'%s' invalid string-map parameter.")
            |> function
                | Ok lists' -> lists' |> transposeList |> mapStringMap envs (snd proc) cont proc []
                | Error e -> Error e |> cont
        | x -> x |> invalidParameter pos "'%s' invalid string-map parameter." |> cont

    [<TailCall>]
    let rec mapVectorMap envs pos cont proc acc =
        function
        | [] -> (List.rev acc |> List.toArray |> SVector, pos) |> Ok |> cont
        | x :: xs ->
            proc
            |> Eval.apply
                envs
                (function
                | Ok a -> xs |> mapVectorMap envs pos cont proc (a :: acc)
                | x -> x |> cont)
                x

    let sVectorMap envs pos cont =
        function
        | [ _ ] as x -> x |> invalidParameter pos "'%s' invalid vector-map parameter." |> cont
        | proc :: vectors ->
            vectors
            |> mapResult (function
                | SVector xs, _ -> Array.toList xs |> Ok
                | x -> x |> invalid (snd x) "'%s' invalid vector-map parameter.")
            |> function
                | Ok lists' -> lists' |> transposeList |> mapVectorMap envs (snd proc) cont proc []
                | Error e -> Error e |> cont
        | x -> x |> invalidParameter pos "'%s' invalid vector-map parameter." |> cont

    [<TailCall>]
    let rec loopForEach envs pos cont proc =
        function
        | [] -> Ok(SEmpty, pos) |> cont
        | x :: xs ->
            proc
            |> Eval.apply
                envs
                (function
                | Ok _ -> xs |> loopForEach envs pos cont proc
                | x -> x |> cont)
                x

    let sForEach envs pos cont =
        function
        | [ _ ] as x -> x |> invalidParameter pos "'%s' invalid for-each parameter." |> cont
        | proc :: lists ->
            lists
            |> mapResult (function
                | SEmpty, _ -> Ok []
                | x when isProperList x -> x |> toList
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
                | Ok lists' -> lists' |> transposeList |> loopForEach envs (snd proc) cont proc
                | Error e -> Error e |> cont
        | x -> x |> invalidParameter pos "'%s' invalid string-for-each parameter." |> cont

    let sVectorForEach envs pos cont =
        function
        | [ _ ] as x -> x |> invalidParameter pos "'%s' invalid vector-for-each parameter." |> cont
        | proc :: vectors ->
            vectors
            |> mapResult (function
                | SVector xs, _ -> Array.toList xs |> Ok
                | x -> x |> invalid (snd x) "'%s' invalid vector-for-each parameter.")
            |> function
                | Ok lists' -> lists' |> transposeList |> loopForEach envs (snd proc) cont proc
                | Error e -> Error e |> cont
        | x -> x |> invalidParameter pos "'%s' invalid vector-for-each parameter." |> cont

    let sCallCC envs pos cont =
        function
        | [ proc ] ->
            let capturedWinders = envs.currentWinders.Value

            let wrappedCont arg =
                if envs.currentWinders.Value = capturedWinders then
                    cont arg
                else
                    doWind envs cont capturedWinders arg

            proc |> Eval.apply envs cont [ SContinuation wrappedCont, pos ]
        | x -> x |> invalidParameter pos "'%s' invalid call/cc parameter." |> cont

    let sValues envs pos cont =
        function
        | [ x ] -> x |> Ok |> cont
        | xs -> (SValues xs, pos) |> Ok |> cont

    let sCallWithValues envs pos cont =
        function
        | [ producer; consumer ] ->
            producer
            |> Eval.apply
                envs
                (function
                | Ok(SValues xs, _) -> consumer |> Eval.apply envs cont xs
                | Ok x -> consumer |> Eval.apply envs cont [ x ]
                | x -> x |> cont)
                []
        | x -> x |> invalidParameter pos "'%s' invalid call-with-values parameter." |> cont

    let sDynamicWind envs pos cont =
        function
        | [ inProc; bodyProc; outProc ] ->
            let id = Context.getNextWinderId envs

            inProc
            |> Eval.apply
                envs
                (function
                | Ok _ ->
                    let winder =
                        { id = id
                          before = inProc
                          after = outProc }

                    Context.pushWinder envs winder

                    bodyProc
                    |> Eval.apply
                        envs
                        (fun res ->
                            Context.popWinder envs id
                            outProc |> Eval.apply envs (fun _ -> cont res) [])
                        []
                | x -> x |> cont)
                []
        | x -> x |> invalidParameter pos "'%s' invalid dynamic-wind parameter." |> cont
