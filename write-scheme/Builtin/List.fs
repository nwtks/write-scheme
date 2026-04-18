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

    let getCar =
        function
        | SList(x :: _) -> x
        | SPair(x :: _, _) -> x
        | x -> Print.print x |> sprintf "'%s' invalid car parameter." |> failwith

    let getCdr =
        function
        | SList(_ :: xs) -> xs |> toSList
        | SPair([ _ ], x2) -> x2
        | SPair(_ :: xs, x2) -> SPair(xs, x2)
        | x -> Print.print x |> sprintf "'%s' invalid cdr parameter." |> failwith

    let sCar envs cont =
        function
        | [ x ] -> x |> getCar |> cont
        | x -> x |> invalidParameter "'%s' invalid car parameter."

    let sCdr envs cont =
        function
        | [ x ] -> x |> getCdr |> cont
        | x -> x |> invalidParameter "'%s' invalid cdr parameter."

    let sCaar envs cont =
        function
        | [ x ] -> x |> getCar |> getCar |> cont
        | x -> x |> invalidParameter "'%s' invalid caar parameter."

    let sCadr envs cont =
        function
        | [ x ] -> x |> getCdr |> getCar |> cont
        | x -> x |> invalidParameter "'%s' invalid cadr parameter."

    let sCdar envs cont =
        function
        | [ x ] -> x |> getCar |> getCdr |> cont
        | x -> x |> invalidParameter "'%s' invalid cdar parameter."

    let sCddr envs cont =
        function
        | [ x ] -> x |> getCdr |> getCdr |> cont
        | x -> x |> invalidParameter "'%s' invalid cddr parameter."

    let isNull envs cont =
        function
        | [ SEmpty ] -> STrue |> cont
        | _ -> SFalse |> cont

    let isList envs cont =
        function
        | [ SList _ ]
        | [ SEmpty ] -> STrue |> cont
        | _ -> SFalse |> cont

    let sMakeList envs cont =
        function
        | [ SRational(k, d) ] when d = 1I && k >= 0I -> List.replicate (int k) SUnspecified |> toSList |> cont
        | [ SRational(k, d); fill ] when d = 1I && k >= 0I -> List.replicate (int k) fill |> toSList |> cont
        | x -> x |> invalidParameter "'%s' invalid make-list parameter."

    let sList envs cont xs = xs |> toSList |> cont

    let sLength envs cont =
        function
        | [ SList xs ] -> newSRational (bigint xs.Length) 1I |> cont
        | [ SEmpty ] -> SZero |> cont
        | x -> x |> invalidParameter "'%s' invalid length parameter."

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

    let sReverse envs cont =
        function
        | [ SList xs ] -> xs |> List.rev |> toSList |> cont
        | [ SEmpty ] -> SEmpty |> cont
        | x -> x |> invalidParameter "'%s' invalid reverse parameter."

    let sListTail envs cont =
        function
        | [ SList xs; SRational(k, d) ] when d = 1I && k >= 0I && k <= bigint xs.Length ->
            xs |> List.skip (int k) |> toSList |> cont
        | [ SEmpty; SRational(k, d) ] when d = 1I && k = 0I -> SEmpty |> cont
        | [ SPair(xs, y); SRational(k, d) ] when d = 1I && k >= 0I && k <= bigint xs.Length ->
            if k = bigint xs.Length then
                y |> cont
            else
                SPair(xs |> List.skip (int k), y) |> cont
        | x -> x |> invalidParameter "'%s' invalid list-tail parameter."

    let sListRef envs cont =
        function
        | [ SList xs; SRational(k, d) ] when d = 1I && k >= 0I && k < bigint xs.Length -> xs.[int k] |> cont
        | [ SPair(xs, _); SRational(k, d) ] when d = 1I && k >= 0I && k < bigint xs.Length -> xs.[int k] |> cont
        | x -> x |> invalidParameter "'%s' invalid list-ref parameter."

    [<TailCall>]
    let rec findMember comparer obj =
        function
        | SEmpty -> SFalse
        | SList(x :: xs) ->
            if comparer obj x then
                SList(x :: xs)
            else
                xs |> toSList |> findMember comparer obj
        | SPair(x :: xs, y) ->
            if comparer obj x then
                SPair(x :: xs, y)
            else
                (if xs.IsEmpty then y else SPair(xs, y)) |> findMember comparer obj
        | _ -> SFalse

    let sMemq envs cont =
        function
        | [ obj; lst ] -> lst |> findMember (fun a b -> eqv (a, b)) obj |> cont
        | x -> x |> invalidParameter "'%s' invalid memq parameter."

    let sMemv envs cont =
        function
        | [ obj; lst ] -> lst |> findMember (fun a b -> eqv (a, b)) obj |> cont
        | x -> x |> invalidParameter "'%s' invalid memv parameter."

    let sMember envs cont =
        function
        | [ obj; lst ] -> lst |> findMember (fun a b -> loopEqual [ a, b ]) obj |> cont
        | x -> x |> invalidParameter "'%s' invalid member parameter."

    [<TailCall>]
    let rec findAssoc comparer obj =
        function
        | SEmpty -> SFalse
        | SList(x :: xs) ->
            if comparer obj (getCar x) then
                x
            else
                xs |> toSList |> findAssoc comparer obj
        | SPair(x :: xs, y) ->
            if comparer obj (getCar x) then
                x
            else
                (if xs.IsEmpty then y else SPair(xs, y)) |> findAssoc comparer obj
        | _ -> SFalse

    let sAssq envs cont =
        function
        | [ obj; lst ] -> lst |> findAssoc (fun a b -> eqv (a, b)) obj |> cont
        | x -> x |> invalidParameter "'%s' invalid assq parameter."

    let sAssv envs cont =
        function
        | [ obj; lst ] -> lst |> findAssoc (fun a b -> eqv (a, b)) obj |> cont
        | x -> x |> invalidParameter "'%s' invalid assv parameter."

    let sAssoc envs cont =
        function
        | [ obj; lst ] -> lst |> findAssoc (fun a b -> loopEqual [ a, b ]) obj |> cont
        | x -> x |> invalidParameter "'%s' invalid assoc parameter."

    let sListCopy envs cont =
        function
        | [ SList xs ] -> [ for x in xs -> x ] |> toSList |> cont
        | [ SEmpty ] -> SEmpty |> cont
        | [ SPair(xs, y) ] -> SPair([ for x in xs -> x ], y) |> cont
        | [ x ] -> x |> cont
        | x -> x |> invalidParameter "'%s' invalid list-copy parameter."
