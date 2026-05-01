namespace WriteScheme.Builtins

open WriteScheme
open Type

[<AutoOpen>]
module List =
    let isPair envs pos cont =
        function
        | [ SPair _, _ ] -> (STrue, pos) |> cont
        | _ -> (SFalse, pos) |> cont

    let sCons envs pos cont =
        function
        | [ x; y ] -> (SPair { car = x; cdr = y }, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid cons parameter."

    let getCar =
        function
        | SPair p, _ -> p.car
        | x -> x |> invalid (snd x) "'%s' invalid car parameter."

    let getCdr =
        function
        | SPair p, _ -> p.cdr
        | x -> x |> invalid (snd x) "'%s' invalid cdr parameter."

    let sCar envs pos cont =
        function
        | [ x ] -> x |> getCar |> cont
        | x -> x |> invalidParameter pos "'%s' invalid car parameter."

    let sCdr envs pos cont =
        function
        | [ x ] -> x |> getCdr |> cont
        | x -> x |> invalidParameter pos "'%s' invalid cdr parameter."

    let sSetCar envs pos cont =
        function
        | [ SPair p, _; x ] ->
            p.car <- x
            (SUnspecified, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid set-car! parameter."

    let sSetCdr envs pos cont =
        function
        | [ SPair p, _; x ] ->
            p.cdr <- x
            (SUnspecified, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid set-cdr! parameter."

    let sCaar envs pos cont =
        function
        | [ x ] -> x |> getCar |> getCar |> cont
        | x -> x |> invalidParameter pos "'%s' invalid caar parameter."

    let sCadr envs pos cont =
        function
        | [ x ] -> x |> getCdr |> getCar |> cont
        | x -> x |> invalidParameter pos "'%s' invalid cadr parameter."

    let sCdar envs pos cont =
        function
        | [ x ] -> x |> getCar |> getCdr |> cont
        | x -> x |> invalidParameter pos "'%s' invalid cdar parameter."

    let sCddr envs pos cont =
        function
        | [ x ] -> x |> getCdr |> getCdr |> cont
        | x -> x |> invalidParameter pos "'%s' invalid cddr parameter."

    let isNull envs pos cont =
        function
        | [ SEmpty, _ ] -> (STrue, pos) |> cont
        | _ -> (SFalse, pos) |> cont

    let isList envs pos cont =
        function
        | [ x ] -> (x |> isProperList |> toSBool, pos) |> cont
        | _ -> (SFalse, pos) |> cont

    let sMakeList envs pos cont =
        function
        | [ SRational(k, d), _ ] when d = 1I && k >= 0I -> List.replicate (int k) (SUnspecified, pos) |> toSPair |> cont
        | [ SRational(k, d), _; fill ] when d = 1I && k >= 0I -> List.replicate (int k) fill |> toSPair |> cont
        | x -> x |> invalidParameter pos "'%s' invalid make-list parameter."

    let sList envs pos cont = toSPair >> cont

    [<TailCall>]
    let rec loopLength acc =
        function
        | SEmpty, _ -> acc
        | SPair pair, _ -> pair.cdr |> loopLength (acc + 1I)
        | x -> x |> invalid (snd x) "'%s' is not a proper list in length."

    let sLength envs pos cont =
        function
        | [ x ] when isProperList x -> (newSRational (x |> loopLength 0I) 1I, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid length parameter."

    let appendTwo a b =
        List.foldBack (fun h acc -> SPair { car = h; cdr = acc }, snd h) (a |> toList) b

    [<TailCall>]
    let rec loopAppend acc =
        function
        | [] -> acc
        | [ last ] -> appendTwo acc last
        | h :: t -> t |> loopAppend (appendTwo acc h)

    let sAppend envs pos cont =
        function
        | [] -> (SEmpty, pos) |> cont
        | [ x ] -> x |> cont
        | x :: xs -> xs |> loopAppend x |> cont

    [<TailCall>]
    let rec loopReverse acc =
        function
        | SEmpty, _ -> acc
        | SPair pair, _ -> pair.cdr |> loopReverse (SPair { car = pair.car; cdr = acc }, snd pair.car)
        | _ -> failwith "unreachable."

    let sReverse envs pos cont =
        function
        | [ x ] when isProperList x -> x |> loopReverse (SEmpty, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid reverse parameter."

    [<TailCall>]
    let rec loopListTail n curr =
        if n = 0I then
            curr
        else
            match curr with
            | SPair p, _ -> p.cdr |> loopListTail (n - 1I)
            | x -> x |> invalid (snd x) "'%s' invalid list-tail parameter."

    let sListTail envs pos cont =
        function
        | [ x; SRational(k, d), _ ] when d = 1I && k >= 0I -> x |> loopListTail k |> cont
        | x -> x |> invalidParameter pos "'%s' invalid list-tail parameter."

    [<TailCall>]
    let rec loopListRef n =
        function
        | SPair p, _ -> if n = 0I then p.car else p.cdr |> loopListRef (n - 1I)
        | x -> x |> invalid (snd x) "'%s' invalid list-ref parameter."

    let sListRef envs pos cont =
        function
        | [ x; SRational(k, d), _ ] when d = 1I && k >= 0I -> x |> loopListRef k |> cont
        | x -> x |> invalidParameter pos "'%s' invalid list-ref parameter."

    [<TailCall>]
    let rec findMember pos comparer obj =
        function
        | SEmpty, _ -> SFalse, pos
        | SPair p, _ ->
            if comparer obj p.car then
                SPair p, pos
            else
                p.cdr |> findMember pos comparer obj
        | _ -> SFalse, pos

    let sMemq envs pos cont =
        function
        | [ obj; lst ] -> lst |> findMember pos (fun a b -> eqv (a, b)) obj |> cont
        | x -> x |> invalidParameter pos "'%s' invalid memq parameter."

    let sMemv envs pos cont =
        function
        | [ obj; lst ] -> lst |> findMember pos (fun a b -> eqv (a, b)) obj |> cont
        | x -> x |> invalidParameter pos "'%s' invalid memv parameter."

    let sMember envs pos cont =
        function
        | [ obj; lst ] -> lst |> findMember pos (fun a b -> loopEqual [ a, b ]) obj |> cont
        | x -> x |> invalidParameter pos "'%s' invalid member parameter."

    [<TailCall>]
    let rec findAssoc pos comparer obj =
        function
        | SEmpty, _ -> SFalse, pos
        | SPair p, _ ->
            if comparer obj (getCar p.car) then
                p.car
            else
                p.cdr |> findAssoc pos comparer obj
        | _ -> SFalse, pos

    let sAssq envs pos cont =
        function
        | [ obj; lst ] -> lst |> findAssoc pos (fun a b -> eqv (a, b)) obj |> cont
        | x -> x |> invalidParameter pos "'%s' invalid assq parameter."

    let sAssv envs pos cont =
        function
        | [ obj; lst ] -> lst |> findAssoc pos (fun a b -> eqv (a, b)) obj |> cont
        | x -> x |> invalidParameter pos "'%s' invalid assv parameter."

    let sAssoc envs pos cont =
        function
        | [ obj; lst ] -> lst |> findAssoc pos (fun a b -> loopEqual [ a, b ]) obj |> cont
        | x -> x |> invalidParameter pos "'%s' invalid assoc parameter."

    [<TailCall>]
    let rec loopListCopy acc =
        function
        | SPair p, _ -> p.cdr |> loopListCopy (p.car :: acc)
        | x -> List.foldBack (fun h t -> SPair { car = h; cdr = t }, snd h) (acc |> List.rev) x

    let sListCopy envs pos cont =
        function
        | [ x ] -> x |> loopListCopy [] |> cont
        | x -> x |> invalidParameter pos "'%s' invalid list-copy parameter."
