namespace WriteScheme.Builtins

open WriteScheme
open Type

[<AutoOpen>]
module List =
    let isPair envs cont =
        function
        | [ SPair _ ] -> STrue |> cont
        | _ -> SFalse |> cont

    let sCons envs cont =
        function
        | [ x; y ] -> SPair { car = x; cdr = y } |> cont
        | x -> x |> invalidParameter "'%s' invalid cons parameter."

    let getCar =
        function
        | SPair p -> p.car
        | x -> x |> invalid "'%s' invalid car parameter."

    let getCdr =
        function
        | SPair p -> p.cdr
        | x -> x |> invalid "'%s' invalid cdr parameter."

    let sCar envs cont =
        function
        | [ x ] -> x |> getCar |> cont
        | x -> x |> invalidParameter "'%s' invalid car parameter."

    let sCdr envs cont =
        function
        | [ x ] -> x |> getCdr |> cont
        | x -> x |> invalidParameter "'%s' invalid cdr parameter."

    let sSetCar envs cont =
        function
        | [ SPair p; x ] ->
            p.car <- x
            SUnspecified |> cont
        | x -> x |> invalidParameter "'%s' invalid set-car! parameter."

    let sSetCdr envs cont =
        function
        | [ SPair p; x ] ->
            p.cdr <- x
            SUnspecified |> cont
        | x -> x |> invalidParameter "'%s' invalid set-cdr! parameter."

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
        | [ x ] -> x |> isProperList |> toSBool |> cont
        | _ -> SFalse |> cont

    let sMakeList envs cont =
        function
        | [ SRational(k, d) ] when d = 1I && k >= 0I -> List.replicate (int k) SUnspecified |> toSPair |> cont
        | [ SRational(k, d); fill ] when d = 1I && k >= 0I -> List.replicate (int k) fill |> toSPair |> cont
        | x -> x |> invalidParameter "'%s' invalid make-list parameter."

    let sList envs cont xs = xs |> toSPair |> cont

    [<TailCall>]
    let rec loopLength acc =
        function
        | SEmpty -> acc
        | SPair p -> p.cdr |> loopLength (acc + 1I)
        | _ -> failwith "not a proper list"

    let sLength envs cont =
        function
        | [ x ] when isProperList x -> newSRational (x |> loopLength 0I) 1I |> cont
        | x -> x |> invalidParameter "'%s' invalid length parameter."

    let appendTwo a b =
        List.foldBack (fun h acc -> SPair { car = h; cdr = acc }) (a |> toList) b

    [<TailCall>]
    let rec loopAppend acc =
        function
        | [] -> acc
        | [ last ] -> appendTwo acc last
        | h :: t -> t |> loopAppend (appendTwo acc h)

    let sAppend envs cont =
        function
        | [] -> SEmpty |> cont
        | [ x ] -> x |> cont
        | x :: xs -> xs |> loopAppend x |> cont

    [<TailCall>]
    let rec loopReverse acc =
        function
        | SEmpty -> acc
        | SPair p -> p.cdr |> loopReverse (SPair { car = p.car; cdr = acc })
        | _ -> failwith "impossible"

    let sReverse envs cont =
        function
        | [ x ] when isProperList x -> x |> loopReverse SEmpty |> cont
        | x -> x |> invalidParameter "'%s' invalid reverse parameter."

    [<TailCall>]
    let rec loopListTail n curr =
        if n = 0I then
            curr
        else
            match curr with
            | SPair p -> p.cdr |> loopListTail (n - 1I)
            | _ -> failwith "index out of range"

    let sListTail envs cont =
        function
        | [ x; SRational(k, d) ] when d = 1I && k >= 0I -> x |> loopListTail k |> cont
        | x -> x |> invalidParameter "'%s' invalid list-tail parameter."

    [<TailCall>]
    let rec loopListRef n =
        function
        | SPair p -> if n = 0I then p.car else p.cdr |> loopListRef (n - 1I)
        | _ -> failwith "index out of range"

    let sListRef envs cont =
        function
        | [ x; SRational(k, d) ] when d = 1I && k >= 0I -> x |> loopListRef k |> cont
        | x -> x |> invalidParameter "'%s' invalid list-ref parameter."

    [<TailCall>]
    let rec findMember comparer obj =
        function
        | SEmpty -> SFalse
        | SPair p ->
            if comparer obj p.car then
                SPair p
            else
                p.cdr |> findMember comparer obj
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
        | SPair p ->
            if comparer obj (getCar p.car) then
                p.car
            else
                p.cdr |> findAssoc comparer obj
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

    [<TailCall>]
    let rec loopListCopy acc =
        function
        | SPair p -> p.cdr |> loopListCopy (p.car :: acc)
        | x -> List.foldBack (fun h t -> SPair { car = h; cdr = t }) (acc |> List.rev) x

    let sListCopy envs cont =
        function
        | [ x ] -> x |> loopListCopy [] |> cont
        | x -> x |> invalidParameter "'%s' invalid list-copy parameter."
