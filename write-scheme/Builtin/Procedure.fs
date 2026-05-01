namespace WriteScheme.Builtins

open WriteScheme
open Type

[<AutoOpen>]
module Procedure =
    let isProcedure envs pos cont =
        function
        | [ SSyntax _, _ ]
        | [ SProcedure _, _ ]
        | [ SContinuation _, _ ] -> (STrue, pos) |> cont
        | _ -> (SFalse, pos) |> cont

    [<TailCall>]
    let rec foldApply pos xs =
        function
        | acc, [ SEmpty, _ ] -> acc |> List.rev
        | acc, [ x ] when isProperList x -> (acc |> List.rev) @ (x |> toList)
        | _, [ _ ]
        | _, [] -> xs |> invalidParameter pos "'%s' invalid apply parameter."
        | acc, x1 :: x2 -> (x1 :: acc, x2) |> foldApply pos xs

    let sApply envs pos cont =
        function
        | proc :: args -> proc |> Eval.apply envs cont (([], args) |> foldApply (snd proc) args)
        | x -> x |> invalidParameter pos "'%s' invalid apply parameter."

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
        | [] -> List.rev acc |> toSPair |> cont
        | x :: xs -> proc |> Eval.apply envs (fun a -> mapMap envs cont proc (a :: acc) xs) x

    let sMap envs pos cont =
        function
        | [ _ ] as x -> x |> invalidParameter pos "'%s' invalid map parameter."
        | proc :: lists ->
            lists
            |> List.map (function
                | SEmpty, _ -> []
                | x when isProperList x -> x |> toList
                | x -> x |> invalid (snd x) "'%s' invalid map parameter.")
            |> transposeList
            |> mapMap envs cont proc []
        | x -> x |> invalidParameter pos "'%s' invalid map parameter."

    [<TailCall>]
    let rec mapStringMap envs pos cont proc acc =
        function
        | [] ->
            let runes =
                List.rev acc
                |> List.map (function
                    | SChar c, _ -> c
                    | x -> x |> invalid (snd x) "'%s' is not a char in string-map.")
                |> List.toArray

            ({ runes = runes; isImmutable = false } |> SString, pos) |> cont
        | x :: xs ->
            proc
            |> Eval.apply envs (fun a -> mapStringMap envs pos cont proc (a :: acc) xs) x

    let sStringMap envs pos cont =
        function
        | [ _ ] as x -> x |> invalidParameter pos "'%s' invalid string-map parameter."
        | proc :: strings ->
            strings
            |> List.map (function
                | SString s, _ -> s.runes |> Array.map (fun c -> SChar c, pos) |> Array.toList
                | x -> x |> invalid (snd x) "'%s' invalid string-map parameter.")
            |> transposeList
            |> mapStringMap envs (snd proc) cont proc []
        | x -> x |> invalidParameter pos "'%s' invalid string-map parameter."

    [<TailCall>]
    let rec mapVectorMap envs pos cont proc acc =
        function
        | [] -> (List.rev acc |> List.toArray |> SVector, pos) |> cont
        | x :: xs ->
            proc
            |> Eval.apply envs (fun a -> mapVectorMap envs pos cont proc (a :: acc) xs) x

    let sVectorMap envs pos cont =
        function
        | [ _ ] as x -> x |> invalidParameter pos "'%s' invalid vector-map parameter."
        | proc :: vectors ->
            vectors
            |> List.map (function
                | SVector xs, _ -> Array.toList xs
                | x -> x |> invalid (snd x) "'%s' invalid vector-map parameter.")
            |> transposeList
            |> mapVectorMap envs (snd proc) cont proc []
        | x -> x |> invalidParameter pos "'%s' invalid vector-map parameter."

    [<TailCall>]
    let rec loopForEach envs pos cont proc =
        function
        | [] -> (SEmpty, pos) |> cont
        | x :: xs -> proc |> Eval.apply envs (fun _ -> loopForEach envs pos cont proc xs) x

    let sForEach envs pos cont =
        function
        | [ _ ] as x -> x |> invalidParameter pos "'%s' invalid for-each parameter."
        | proc :: lists ->
            lists
            |> List.map (function
                | SEmpty, _ -> []
                | x when isProperList x -> x |> toList
                | x -> x |> invalid (snd x) "'%s' invalid for-each parameter.")
            |> transposeList
            |> loopForEach envs (snd proc) cont proc
        | x -> x |> invalidParameter pos "'%s' invalid for-each parameter."

    let sStringForEach envs pos cont =
        function
        | [ _ ] as x -> x |> invalidParameter pos "'%s' invalid string-for-each parameter."
        | proc :: strings ->
            strings
            |> List.map (function
                | SString s, _ -> s.runes |> Array.map (fun c -> SChar c, pos) |> Array.toList
                | x -> x |> invalid (snd x) "'%s' invalid string-for-each parameter.")
            |> transposeList
            |> loopForEach envs (snd proc) cont proc
        | x -> x |> invalidParameter pos "'%s' invalid string-for-each parameter."

    let sVectorForEach envs pos cont =
        function
        | [ _ ] as x -> x |> invalidParameter pos "'%s' invalid vector-for-each parameter."
        | proc :: vectors ->
            vectors
            |> List.map (function
                | SVector xs, _ -> Array.toList xs
                | x -> x |> invalid (snd x) "'%s' invalid vector-for-each parameter.")
            |> transposeList
            |> loopForEach envs (snd proc) cont proc
        | x -> x |> invalidParameter pos "'%s' invalid vector-for-each parameter."

    let sCallCC envs pos cont =
        function
        | [ proc ] ->
            let capturedWinders = envs.currentWinders.Value

            let wrappedCont arg =
                if envs.currentWinders.Value = capturedWinders then
                    cont arg
                else
                    doWind envs (fun _ -> cont arg) capturedWinders

            proc |> Eval.apply envs cont [ SContinuation wrappedCont, pos ]
        | x -> x |> invalidParameter pos "'%s' invalid call/cc parameter."

    let sValues envs pos cont =
        function
        | [ x ] -> x |> cont
        | xs -> (SValues xs, pos) |> cont

    let sCallWithValues envs pos cont =
        function
        | [ producer; consumer ] ->
            producer
            |> Eval.apply
                envs
                (function
                | SValues xs, _ -> consumer |> Eval.apply envs cont xs
                | x -> consumer |> Eval.apply envs cont [ x ])
                []
        | x -> x |> invalidParameter pos "'%s' invalid call-with-values parameter."

    let sDynamicWind envs pos cont =
        function
        | [ inProc; bodyProc; outProc ] ->
            let id = Context.getNextWinderId envs

            inProc
            |> Eval.apply
                envs
                (fun _ ->
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
                        [])
                []
        | x -> x |> invalidParameter pos "'%s' invalid dynamic-wind parameter."
