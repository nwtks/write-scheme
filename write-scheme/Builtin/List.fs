namespace WriteScheme.Builtins

open WriteScheme
open Type

[<AutoOpen>]
module List =
    let isPair envs cont =
        function
        | [ SList [] ] -> SFalse |> cont
        | [ SList _ ]
        | [ SPair _ ] -> STrue |> cont
        | _ -> SFalse |> cont

    let sCons envs cont =
        function
        | [ x; SEmpty ] -> [ x ] |> toSList |> cont
        | [ x; SList xs ] -> x :: xs |> toSList |> cont
        | [ x; SPair(y1, y2) ] -> SPair(x :: y1, y2) |> cont
        | [ x; y ] -> SPair([ x ], y) |> cont
        | x -> x |> invalidParameter "'%s' invalid cons parameter."

    let sCar envs cont =
        function
        | [ SList(x :: _) ] -> x |> cont
        | [ SPair(x :: _, _) ] -> x |> cont
        | x -> x |> invalidParameter "'%s' invalid car parameter."

    let sCdr envs cont =
        function
        | [ SList(_ :: xs) ] -> xs |> toSList |> cont
        | [ SPair([ _ ], x2) ] -> x2 |> cont
        | [ SPair(_ :: xs, x2) ] -> SPair(xs, x2) |> cont
        | x -> x |> invalidParameter "'%s' invalid cdr parameter."

    let isNull envs cont =
        function
        | [ SEmpty ] -> STrue |> cont
        | _ -> SFalse |> cont

    let isList envs cont =
        function
        | [ SList _ ]
        | [ SEmpty ] -> STrue |> cont
        | _ -> SFalse |> cont

    let sList envs cont xs = xs |> toSList |> cont

    [<TailCall>]
    let rec foldAppend xs =
        function
        | [], []
        | [], [ SEmpty ] -> SEmpty
        | [], [ x ] -> x
        | acc, []
        | acc, [ SEmpty ] -> acc |> toSList
        | acc, [ SList x ] -> acc @ x |> toSList
        | acc, [ SPair(x1, x2) ] -> SPair(acc @ x1, x2)
        | acc, [ x ] -> SPair(acc, x)
        | acc, SEmpty :: x -> (acc, x) |> foldAppend xs
        | acc, SList x1 :: x2 -> (acc @ x1, x2) |> foldAppend xs
        | _ -> xs |> invalidParameter "'%s' invalid append parameter."

    let sAppend envs cont xs = ([], xs) |> foldAppend xs |> cont
