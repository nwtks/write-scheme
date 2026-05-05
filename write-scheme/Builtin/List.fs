namespace WriteScheme.Builtins

open WriteScheme
open Type

[<AutoOpen>]
module List =
    let isPair envs pos cont =
        function
        | [ SPair _, _ ] -> Ok(STrue, pos) |> cont
        | _ -> Ok(SFalse, pos) |> cont

    let sCons envs pos cont =
        function
        | [ x; y ] -> Ok(SPair { car = x; cdr = y }, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid cons parameter." |> cont

    let getCar =
        function
        | SPair p, _ -> Ok p.car
        | x -> x |> invalid (snd x) "'%s' invalid car parameter."

    let getCdr =
        function
        | SPair p, _ -> Ok p.cdr
        | x -> x |> invalid (snd x) "'%s' invalid cdr parameter."

    let sCar envs pos cont =
        function
        | [ x ] -> x |> getCar |> cont
        | x -> x |> invalidParameter pos "'%s' invalid car parameter." |> cont

    let sCdr envs pos cont =
        function
        | [ x ] -> x |> getCdr |> cont
        | x -> x |> invalidParameter pos "'%s' invalid cdr parameter." |> cont

    let sSetCar envs pos cont =
        function
        | [ SPair p, _; x ] ->
            p.car <- x
            Ok(SUnspecified, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid set-car! parameter." |> cont

    let sSetCdr envs pos cont =
        function
        | [ SPair p, _; x ] ->
            p.cdr <- x
            Ok(SUnspecified, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid set-cdr! parameter." |> cont

    let sCaar envs pos cont =
        function
        | [ x ] -> x |> getCar |> Result.bind getCar |> cont
        | x -> x |> invalidParameter pos "'%s' invalid caar parameter." |> cont

    let sCadr envs pos cont =
        function
        | [ x ] -> x |> getCdr |> Result.bind getCar |> cont
        | x -> x |> invalidParameter pos "'%s' invalid cadr parameter." |> cont

    let sCdar envs pos cont =
        function
        | [ x ] -> x |> getCar |> Result.bind getCdr |> cont
        | x -> x |> invalidParameter pos "'%s' invalid cdar parameter." |> cont

    let sCddr envs pos cont =
        function
        | [ x ] -> x |> getCdr |> Result.bind getCdr |> cont
        | x -> x |> invalidParameter pos "'%s' invalid cddr parameter." |> cont

    let isNull envs pos cont =
        function
        | [ SEmpty, _ ] -> Ok(STrue, pos) |> cont
        | _ -> Ok(SFalse, pos) |> cont

    let isList envs pos cont =
        function
        | [ x ] -> Ok(x |> isProperList |> toSBool, pos) |> cont
        | _ -> Ok(SFalse, pos) |> cont

    let sMakeList envs pos cont =
        function
        | [ SRational(k, d), _ ] when d = 1I && k >= 0I ->
            Ok(List.replicate (int k) (SUnspecified, pos) |> toSPair) |> cont
        | [ SRational(k, d), _; fill ] when d = 1I && k >= 0I -> Ok(List.replicate (int k) fill |> toSPair) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid make-list parameter." |> cont

    let sList envs pos cont = toSPair >> Ok >> cont

    let sLength envs pos cont =
        let length expr =
            match expr with
            | SEmpty, _ -> Ok 0I
            | SPair _, _ ->
                match loopListInfo expr expr 0I None with
                | Ok(_, len) -> Ok len
                | Error msg -> Error(EvalError(sprintf "'%s' %s" (expr |> Print.print) msg, snd expr))
            | _, p -> Error(EvalError(sprintf "'%s' not a proper list." (expr |> Print.print), p))

        function
        | [ x ] ->
            match length x with
            | Ok len -> Ok(newInteger len, pos) |> cont
            | Error e -> Error e |> cont
        | x -> x |> invalidParameter pos "'%s' invalid length parameter." |> cont

    let appendTwo a b =
        a
        |> toList
        |> Result.map (fun alist -> List.foldBack (fun h acc -> SPair { car = h; cdr = acc }, snd h) alist b)

    [<TailCall>]
    let rec loopAppend acc =
        function
        | [] -> Ok acc
        | [ last ] -> appendTwo acc last
        | h :: t ->
            match appendTwo acc h with
            | Ok res -> t |> loopAppend res
            | Error e -> Error e

    let sAppend envs pos cont =
        function
        | [] -> Ok(SEmpty, pos) |> cont
        | [ x ] -> Ok x |> cont
        | x :: xs -> xs |> loopAppend x |> cont

    [<TailCall>]
    let rec loopReverse acc =
        function
        | SEmpty, _ -> Ok acc
        | SPair pair, _ -> pair.cdr |> loopReverse (SPair { car = pair.car; cdr = acc }, snd pair.car)
        | x -> x |> invalid (snd x) "'%s' is not a proper list in reverse."

    let sReverse envs pos cont =
        function
        | [ x ] -> x |> loopReverse (SEmpty, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid reverse parameter." |> cont

    [<TailCall>]
    let rec loopListTail n curr =
        if n = 0I then
            Ok curr
        else
            match curr with
            | SPair p, _ -> p.cdr |> loopListTail (n - 1I)
            | x -> x |> invalid (snd x) "'%s' invalid list-tail parameter."

    let sListTail envs pos cont =
        function
        | [ x; SRational(k, d), _ ] when d = 1I && k >= 0I -> x |> loopListTail k |> cont
        | x -> x |> invalidParameter pos "'%s' invalid list-tail parameter." |> cont

    [<TailCall>]
    let rec loopListRef n =
        function
        | SPair p, _ -> if n = 0I then Ok p.car else p.cdr |> loopListRef (n - 1I)
        | x -> x |> invalid (snd x) "'%s' invalid list-ref parameter."

    let sListRef envs pos cont =
        function
        | [ x; SRational(k, d), _ ] when d = 1I && k >= 0I -> x |> loopListRef k |> cont
        | x -> x |> invalidParameter pos "'%s' invalid list-ref parameter." |> cont

    let sListSet envs pos cont =
        function
        | [ lst; SRational(k, d), _; obj ] when d = 1I && k >= 0I ->
            lst
            |> loopListTail k
            |> Result.bind (function
                | SPair p, _ ->
                    p.car <- obj
                    Ok(SUnspecified, pos)
                | x -> Error(EvalError("Out of range or not a pair in list-set!.", snd x)))
            |> cont
        | x -> x |> invalidParameter pos "'%s' invalid list-set! parameter." |> cont

    [<TailCall>]
    let rec findMember pos comparer obj =
        function
        | SEmpty, _ -> Ok(SFalse, pos)
        | SPair p, _ as x ->
            if comparer obj p.car then
                Ok x
            else
                p.cdr |> findMember pos comparer obj
        | x -> Ok(SFalse, pos)

    let sMemq envs pos cont =
        function
        | [ obj; lst ] -> lst |> findMember pos (fun a b -> eqv (a, b)) obj |> cont
        | x -> x |> invalidParameter pos "'%s' invalid memq parameter." |> cont

    let sMemv envs pos cont =
        function
        | [ obj; lst ] -> lst |> findMember pos (fun a b -> eqv (a, b)) obj |> cont
        | x -> x |> invalidParameter pos "'%s' invalid memv parameter." |> cont

    [<TailCall>]
    let rec loopMember envs pos cont obj proc =
        function
        | SEmpty, _ -> Ok(SFalse, pos) |> cont
        | SPair p, _ as x ->
            proc
            |> Eval.apply
                envs
                (function
                | Ok(SBool false, _) -> p.cdr |> loopMember envs pos cont obj proc
                | Ok _ -> Ok x |> cont
                | Error e -> Error e |> cont)
                [ obj; p.car ]
        | x -> Ok(SFalse, pos) |> cont

    let sMember envs pos cont =
        function
        | [ obj; lst ] -> lst |> findMember pos (fun a b -> loopEqual [ a, b ]) obj |> cont
        | [ obj; lst; proc ] -> lst |> loopMember envs pos cont obj proc
        | x -> x |> invalidParameter pos "'%s' invalid member parameter." |> cont

    [<TailCall>]
    let rec findAssoc pos comparer obj =
        function
        | SEmpty, _ -> Ok(SFalse, pos)
        | SPair p, _ ->
            match p.car |> getCar with
            | Ok car ->
                if comparer obj car then
                    Ok p.car
                else
                    p.cdr |> findAssoc pos comparer obj
            | Error e -> Error e
        | _ -> Ok(SFalse, pos)

    let sAssq envs pos cont =
        function
        | [ obj; lst ] -> lst |> findAssoc pos (fun a b -> eqv (a, b)) obj |> cont
        | x -> x |> invalidParameter pos "'%s' invalid assq parameter." |> cont

    let sAssv envs pos cont =
        function
        | [ obj; lst ] -> lst |> findAssoc pos (fun a b -> eqv (a, b)) obj |> cont
        | x -> x |> invalidParameter pos "'%s' invalid assv parameter." |> cont

    [<TailCall>]
    let rec loopAssoc envs pos cont obj proc =
        function
        | SEmpty, _ -> Ok(SFalse, pos) |> cont
        | SPair p, _ ->
            match p.car |> getCar with
            | Ok car ->
                proc
                |> Eval.apply
                    envs
                    (function
                    | Ok(SBool false, _) -> p.cdr |> loopAssoc envs pos cont obj proc
                    | Ok _ -> Ok p.car |> cont
                    | Error e -> Error e |> cont)
                    [ obj; car ]
            | Error e -> Error e |> cont
        | _ -> Ok(SFalse, pos) |> cont

    let sAssoc envs pos cont =
        function
        | [ obj; lst ] -> lst |> findAssoc pos (fun a b -> loopEqual [ a, b ]) obj |> cont
        | [ obj; lst; proc ] -> lst |> loopAssoc envs pos cont obj proc
        | x -> x |> invalidParameter pos "'%s' invalid assoc parameter." |> cont

    [<TailCall>]
    let rec loopListCopy acc =
        function
        | SPair p, _ -> p.cdr |> loopListCopy (p.car :: acc)
        | x -> Ok(List.foldBack (fun h t -> SPair { car = h; cdr = t }, snd h) (acc |> List.rev) x)

    let sListCopy envs pos cont =
        function
        | [ x ] -> x |> loopListCopy [] |> cont
        | x -> x |> invalidParameter pos "'%s' invalid list-copy parameter." |> cont
