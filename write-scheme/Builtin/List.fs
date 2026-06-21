namespace WriteScheme.Builtins

open WriteScheme
open Type

[<AutoOpen>]
module List =
    let isPair context pos cont =
        function
        | [ SPair _, _ ] -> Ok(STrue, pos) |> cont
        | [ _ ] -> Ok(SFalse, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid pair? parameter." |> cont

    let sCons context pos cont =
        function
        | [ x; y ] -> Ok(SPair { car = x; cdr = y }, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid cons parameter." |> cont

    let getCar =
        function
        | SPair pair, _ -> Ok pair.car
        | x -> x |> invalid (snd x) "'%s' invalid car parameter."

    let getCdr =
        function
        | SPair pair, _ -> Ok pair.cdr
        | x -> x |> invalid (snd x) "'%s' invalid cdr parameter."

    let sCar context pos cont =
        function
        | [ pair ] -> pair |> getCar |> cont
        | x -> x |> invalidParameter pos "'%s' invalid car parameter." |> cont

    let sCdr context pos cont =
        function
        | [ pair ] -> pair |> getCdr |> cont
        | x -> x |> invalidParameter pos "'%s' invalid cdr parameter." |> cont

    let sSetCarBang context pos cont =
        function
        | [ SPair pair, _; obj ] ->
            pair.car <- obj
            Ok(SUnspecified, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid set-car! parameter." |> cont

    let sSetCdrBang context pos cont =
        function
        | [ SPair pair, _; obj ] ->
            pair.cdr <- obj
            Ok(SUnspecified, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid set-cdr! parameter." |> cont

    let sCaar context pos cont =
        function
        | [ pair ] -> pair |> getCar |> Result.bind getCar |> cont
        | x -> x |> invalidParameter pos "'%s' invalid caar parameter." |> cont

    let sCadr context pos cont =
        function
        | [ pair ] -> pair |> getCdr |> Result.bind getCar |> cont
        | x -> x |> invalidParameter pos "'%s' invalid cadr parameter." |> cont

    let sCdar context pos cont =
        function
        | [ pair ] -> pair |> getCar |> Result.bind getCdr |> cont
        | x -> x |> invalidParameter pos "'%s' invalid cdar parameter." |> cont

    let sCddr context pos cont =
        function
        | [ pair ] -> pair |> getCdr |> Result.bind getCdr |> cont
        | x -> x |> invalidParameter pos "'%s' invalid cddr parameter." |> cont

    let isNull context pos cont =
        function
        | [ SEmpty, _ ] -> Ok(STrue, pos) |> cont
        | [ _ ] -> Ok(SFalse, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid null? parameter." |> cont

    let isList context pos cont =
        function
        | [ obj ] -> Ok(obj |> isProperList |> toSBool, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid list? parameter." |> cont

    let sMakeList context pos cont =
        function
        | [ SRational(len, d), _ ] when d = 1I && len >= 0I ->
            Ok(List.replicate (int len) (SUnspecified, pos) |> toSPair) |> cont
        | [ SRational(len, d), _; fill ] when d = 1I && len >= 0I ->
            Ok(List.replicate (int len) fill |> toSPair) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid make-list parameter." |> cont

    let sList context pos cont = toSPair >> Ok >> cont

    let computeLength list =
        match list with
        | SEmpty, _ -> Ok 0I
        | SPair _, _ ->
            match loopListInfo list list 0I [] with
            | Ok(_, len) -> Ok len
            | Error msg -> Error msg
        | _ -> Error "not a proper list."

    let sLength context pos cont =
        function
        | [ list ] ->
            match computeLength list with
            | Ok len -> Ok(newInteger len, pos)
            | Error msg -> EvalError(sprintf "'%s' %s" (list |> Print.print) msg, snd list) |> Error
            |> cont
        | x -> x |> invalidParameter pos "'%s' invalid length parameter." |> cont

    let appendTwo a b =
        a
        |> toList
        |> Result.map (fun alist -> b |> List.foldBack (fun h acc -> SPair { car = h; cdr = acc }, snd h) alist)

    [<TailCall>]
    let rec loopAppend acc =
        function
        | [] -> Ok acc
        | [ x ] -> appendTwo acc x
        | h :: t ->
            match appendTwo acc h with
            | Ok res -> t |> loopAppend res
            | x -> x

    let sAppend context pos cont =
        function
        | [] -> Ok(SEmpty, pos) |> cont
        | [ list ] -> Ok list |> cont
        | list :: lists -> lists |> loopAppend list |> cont

    [<TailCall>]
    let rec loopReverse acc =
        function
        | SEmpty, _ -> Ok acc
        | SPair pair, _ -> pair.cdr |> loopReverse (SPair { car = pair.car; cdr = acc }, snd pair.car)
        | x -> x |> invalid (snd x) "'%s' is not a proper list in reverse."

    let sReverse context pos cont =
        function
        | [ list ] -> list |> loopReverse (SEmpty, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid reverse parameter." |> cont

    [<TailCall>]
    let rec loopListTail n list =
        if n = 0I then
            Ok list
        else
            match list with
            | SPair pair, _ -> pair.cdr |> loopListTail (n - 1I)
            | x -> x |> invalid (snd x) "'%s' invalid list-tail parameter."

    let sListTail context pos cont =
        function
        | [ list; SRational(n, d), _ ] when d = 1I && n >= 0I -> list |> loopListTail n |> cont
        | x -> x |> invalidParameter pos "'%s' invalid list-tail parameter." |> cont

    [<TailCall>]
    let rec loopListRef n =
        function
        | SPair pair, _ ->
            if n = 0I then
                Ok pair.car
            else
                pair.cdr |> loopListRef (n - 1I)
        | x -> x |> invalid (snd x) "'%s' invalid list-ref parameter."

    let sListRef context pos cont =
        function
        | [ list; SRational(n, d), _ ] when d = 1I && n >= 0I -> list |> loopListRef n |> cont
        | x -> x |> invalidParameter pos "'%s' invalid list-ref parameter." |> cont

    let sListSetBang context pos cont =
        function
        | [ list; SRational(n, d), _; obj ] when d = 1I && n >= 0I ->
            list
            |> loopListTail n
            |> Result.bind (function
                | SPair pair, _ ->
                    pair.car <- obj
                    Ok(SUnspecified, pos)
                | x -> EvalError("Out of range or not a pair in list-set!.", snd x) |> Error)
            |> cont
        | x -> x |> invalidParameter pos "'%s' invalid list-set! parameter." |> cont

    [<TailCall>]
    let rec findMember pos compare obj =
        function
        | SEmpty, _ -> Ok(SFalse, pos)
        | SPair p, _ as pair ->
            if compare obj p.car then
                Ok pair
            else
                p.cdr |> findMember pos compare obj
        | x -> Ok(SFalse, pos)

    let sMemq context pos cont =
        function
        | [ obj; list ] -> list |> findMember pos (fun a b -> eqv (a, b)) obj |> cont
        | x -> x |> invalidParameter pos "'%s' invalid memq parameter." |> cont

    let sMemv context pos cont =
        function
        | [ obj; list ] -> list |> findMember pos (fun a b -> eqv (a, b)) obj |> cont
        | x -> x |> invalidParameter pos "'%s' invalid memv parameter." |> cont

    [<TailCall>]
    let rec loopMember context pos cont compare obj =
        function
        | SEmpty, _ -> Ok(SFalse, pos) |> cont
        | SPair p, _ as pair ->
            compare
            |> Eval.apply
                context
                (function
                | Ok(SBool false, _) -> p.cdr |> loopMember context pos cont compare obj
                | Ok _ -> Ok pair |> cont
                | x -> x |> cont)
                [ obj; p.car ]
        | x -> Ok(SFalse, pos) |> cont

    let sMember context pos cont =
        function
        | [ obj; list ] -> list |> findMember pos (fun a b -> [ a, b ] |> loopEqual []) obj |> cont
        | [ obj; list; compare ] -> list |> loopMember context pos cont compare obj
        | x -> x |> invalidParameter pos "'%s' invalid member parameter." |> cont

    [<TailCall>]
    let rec findAssoc pos compare obj =
        function
        | SEmpty, _ -> Ok(SFalse, pos)
        | SPair pair, _ ->
            match pair.car |> getCar with
            | Ok car ->
                if compare obj car then
                    Ok pair.car
                else
                    pair.cdr |> findAssoc pos compare obj
            | x -> x
        | _ -> Ok(SFalse, pos)

    let sAssq context pos cont =
        function
        | [ obj; list ] -> list |> findAssoc pos (fun a b -> eqv (a, b)) obj |> cont
        | x -> x |> invalidParameter pos "'%s' invalid assq parameter." |> cont

    let sAssv context pos cont =
        function
        | [ obj; list ] -> list |> findAssoc pos (fun a b -> eqv (a, b)) obj |> cont
        | x -> x |> invalidParameter pos "'%s' invalid assv parameter." |> cont

    [<TailCall>]
    let rec loopAssoc context pos cont compare obj =
        function
        | SEmpty, _ -> Ok(SFalse, pos) |> cont
        | SPair pair, _ ->
            match pair.car |> getCar with
            | Ok car ->
                compare
                |> Eval.apply
                    context
                    (function
                    | Ok(SBool false, _) -> pair.cdr |> loopAssoc context pos cont compare obj
                    | Ok _ -> Ok pair.car |> cont
                    | x -> x |> cont)
                    [ obj; car ]
            | x -> x |> cont
        | _ -> Ok(SFalse, pos) |> cont

    let sAssoc context pos cont =
        function
        | [ obj; list ] -> list |> findAssoc pos (fun a b -> [ a, b ] |> loopEqual []) obj |> cont
        | [ obj; list; compare ] -> list |> loopAssoc context pos cont compare obj
        | x -> x |> invalidParameter pos "'%s' invalid assoc parameter." |> cont

    [<TailCall>]
    let rec loopListCopy acc =
        function
        | SPair pair, _ -> pair.cdr |> loopListCopy (pair.car :: acc)
        | obj ->
            obj
            |> List.foldBack (fun h t -> SPair { car = h; cdr = t }, snd h) (acc |> List.rev)
            |> Ok

    let sListCopy context pos cont =
        function
        | [ obj ] -> obj |> loopListCopy [] |> cont
        | x -> x |> invalidParameter pos "'%s' invalid list-copy parameter." |> cont
