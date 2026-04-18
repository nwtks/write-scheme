namespace WriteScheme.Builtins

open WriteScheme
open Type

[<AutoOpen>]
module Procedure =
    let isProcedure envs cont =
        function
        | [ SSyntax _ ]
        | [ SProcedure _ ]
        | [ SContinuation _ ] -> STrue |> cont
        | _ -> SFalse |> cont

    [<TailCall>]
    let rec foldApply xs =
        function
        | acc, [ SEmpty ] -> List.rev acc
        | acc, [ SList x ] -> List.rev acc @ x
        | acc, [ _ ]
        | acc, [] -> xs |> invalidParameter "'%s' invalid apply parameter."
        | acc, x1 :: x2 -> (x1 :: acc, x2) |> foldApply xs

    let sApply envs cont =
        function
        | proc :: args -> proc |> Eval.apply envs cont (([], args) |> foldApply args)
        | x -> x |> invalidParameter "'%s' invalid apply parameter."

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
        | [] -> List.rev acc |> toSList |> cont
        | x :: xs -> proc |> Eval.apply envs (fun a -> mapMap envs cont proc (a :: acc) xs) x

    let sMap envs cont =
        function
        | [ _ ] as x -> x |> invalidParameter "'%s' invalid map parameter."
        | proc :: lists as x ->
            lists
            |> List.map (function
                | SEmpty -> []
                | SList xs -> xs
                | _ -> x |> invalidParameter "'%s' invalid map parameter.")
            |> transposeList
            |> mapMap envs cont proc []
        | x -> x |> invalidParameter "'%s' invalid map parameter."

    [<TailCall>]
    let rec mapStringMap envs cont proc acc =
        function
        | [] ->
            List.rev acc
            |> List.map (function
                | SChar c -> c
                | x -> Print.print x |> sprintf "'%s' is not a char." |> failwith)
            |> String.concat ""
            |> SString
            |> cont
        | x :: xs -> proc |> Eval.apply envs (fun a -> mapStringMap envs cont proc (a :: acc) xs) x

    let sStringMap envs cont =
        function
        | [ _ ] as x -> x |> invalidParameter "'%s' invalid string-map parameter."
        | proc :: strings as x ->
            strings
            |> List.map (function
                | SString s -> s |> Seq.map (fun c -> SChar(string c)) |> Seq.toList
                | _ -> x |> invalidParameter "'%s' invalid string-map parameter.")
            |> transposeList
            |> mapStringMap envs cont proc []
        | x -> x |> invalidParameter "'%s' invalid string-map parameter."

    [<TailCall>]
    let rec mapVectorMap envs cont proc acc =
        function
        | [] -> List.rev acc |> List.toArray |> SVector |> cont
        | x :: xs -> proc |> Eval.apply envs (fun a -> mapVectorMap envs cont proc (a :: acc) xs) x

    let sVectorMap envs cont =
        function
        | [ _ ] as x -> x |> invalidParameter "'%s' invalid vector-map parameter."
        | proc :: vectors as x ->
            vectors
            |> List.map (function
                | SVector xs -> Array.toList xs
                | _ -> x |> invalidParameter "'%s' invalid vector-map parameter.")
            |> transposeList
            |> mapVectorMap envs cont proc []
        | x -> x |> invalidParameter "'%s' invalid vector-map parameter."

    [<TailCall>]
    let rec loopForEach envs cont proc =
        function
        | [] -> SEmpty |> cont
        | x :: xs -> proc |> Eval.apply envs (fun _ -> loopForEach envs cont proc xs) x

    let sForEach envs cont =
        function
        | [ _ ] as x -> x |> invalidParameter "'%s' invalid for-each parameter."
        | proc :: lists as x ->
            lists
            |> List.map (function
                | SEmpty -> []
                | SList xs -> xs
                | _ -> x |> invalidParameter "'%s' invalid for-each parameter.")
            |> transposeList
            |> loopForEach envs cont proc
        | x -> x |> invalidParameter "'%s' invalid for-each parameter."

    let sStringForEach envs cont =
        function
        | [ _ ] as x -> x |> invalidParameter "'%s' invalid string-for-each parameter."
        | proc :: strings as x ->
            strings
            |> List.map (function
                | SString s -> s |> Seq.map (fun c -> SChar(string c)) |> Seq.toList
                | _ -> x |> invalidParameter "'%s' invalid string-for-each parameter.")
            |> transposeList
            |> loopForEach envs cont proc
        | x -> x |> invalidParameter "'%s' invalid string-for-each parameter."

    let sVectorForEach envs cont =
        function
        | [ _ ] as x -> x |> invalidParameter "'%s' invalid vector-for-each parameter."
        | proc :: vectors as x ->
            vectors
            |> List.map (function
                | SVector xs -> Array.toList xs
                | _ -> x |> invalidParameter "'%s' invalid vector-for-each parameter.")
            |> transposeList
            |> loopForEach envs cont proc
        | x -> x |> invalidParameter "'%s' invalid vector-for-each parameter."

    let sCallCC envs cont =
        function
        | [ proc ] ->
            let capturedWinders = envs.currentWinders.Value

            let wrappedCont arg =
                if envs.currentWinders.Value = capturedWinders then
                    cont arg
                else
                    doWind envs (fun _ -> cont arg) capturedWinders

            proc |> Eval.apply envs cont [ SContinuation wrappedCont ]
        | x -> x |> invalidParameter "'%s' invalid call/cc parameter."

    let sValues envs cont =
        function
        | [ x ] -> x |> cont
        | xs -> SValues xs |> cont

    let sCallWithValues envs cont =
        function
        | [ producer; consumer ] ->
            producer
            |> Eval.apply
                envs
                (function
                | SValues xs -> consumer |> Eval.apply envs cont xs
                | x -> consumer |> Eval.apply envs cont [ x ])
                []
        | x -> x |> invalidParameter "'%s' invalid call-with-values parameter."

    let sDynamicWind envs cont =
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
        | x -> x |> invalidParameter "'%s' invalid dynamic-wind parameter."
