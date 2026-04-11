namespace WriteScheme.Builtins

open WriteScheme
open Type

[<AutoOpen>]
module Math =
    [<TailCall>]
    let rec eqv =
        function
        | SBool a, SBool b -> a = b
        | SRational(a1, a2), SRational(b1, b2) -> a1 = b1 && a2 = b2
        | SReal a, SReal b -> a = b
        | SChar a, SChar b -> a = b
        | SSymbol a, SSymbol b -> a = b
        | SQuote a, SQuote b -> (a, b) |> eqv
        | SUnquote a, SUnquote b -> (a, b) |> eqv
        | a, b -> a = b

    [<TailCall>]
    let rec loopEqual =
        function
        | [] -> true
        | (a, b) :: xs ->
            match a, b with
            | SList la, SList lb ->
                if la.Length <> lb.Length then
                    false
                else
                    List.zip la lb @ xs |> loopEqual
            | SPair(la, ra), SPair(lb, rb) ->
                if la.Length <> lb.Length then
                    false
                else
                    List.zip la lb @ (ra, rb) :: xs |> loopEqual
            | SQuote a', SQuote b' -> (a', b') :: xs |> loopEqual
            | SUnquote a', SUnquote b' -> (a', b') :: xs |> loopEqual
            | SBool a', SBool b' -> a' = b' && loopEqual xs
            | SRational(a1, a2), SRational(b1, b2) -> a1 = b1 && a2 = b2 && loopEqual xs
            | SReal a', SReal b' -> a' = b' && loopEqual xs
            | SString a', SString b' -> a' = b' && loopEqual xs
            | SChar a', SChar b' -> a' = b' && loopEqual xs
            | SSymbol a', SSymbol b' -> a' = b' && loopEqual xs
            | a', b' -> a' = b' && loopEqual xs

    let equal (a, b) = [ a, b ] |> loopEqual

    let isEqv envs cont =
        function
        | [ a; b ] -> (a, b) |> eqv |> toSBool |> cont
        | _ -> SFalse |> cont

    let isEqual envs cont =
        function
        | [ a; b ] -> (a, b) |> equal |> toSBool |> cont
        | _ -> SFalse |> cont

    let isNumber envs cont =
        function
        | [ SRational _ ]
        | [ SReal _ ] -> STrue |> cont
        | _ -> SFalse |> cont

    let toFloat x y = float x / float y

    let comparePred pred1 pred2 =
        function
        | SRational(a1, a2), SRational(b1, b2) -> pred1 (a1 * b2) (b1 * a2)
        | SRational(a1, a2), SReal b -> pred2 (toFloat a1 a2) b
        | SReal a, SRational(b1, b2) -> pred2 a (toFloat b1 b2)
        | SReal a, SReal b -> pred2 a b
        | _ -> false

    [<TailCall>]
    let rec compare pred1 pred2 n =
        function
        | [] -> STrue
        | x :: xs ->
            if comparePred pred1 pred2 (n, x) then
                compare pred1 pred2 x xs
            else
                SFalse

    let compareNumber pred1 pred2 envs cont =
        function
        | [] -> STrue |> cont
        | x :: xs -> compare pred1 pred2 x xs |> cont

    let equalNumber envs cont args = compareNumber (=) (=) envs cont args
    let lessNumber envs cont args = compareNumber (<) (<) envs cont args
    let greaterNumber envs cont args = compareNumber (>) (>) envs cont args
    let lessEqualNumber envs cont args = compareNumber (<=) (<=) envs cont args
    let greaterEqualNumber envs cont args = compareNumber (>=) (>=) envs cont args

    let isZero envs cont =
        function
        | [ x ] -> equalNumber envs cont [ x; SZero ]
        | _ -> SFalse |> cont

    let isPositive envs cont =
        function
        | [ x ] -> greaterNumber envs cont [ x; SZero ]
        | _ -> SFalse |> cont

    let isNegative envs cont =
        function
        | [ x ] -> lessNumber envs cont [ x; SZero ]
        | _ -> SFalse |> cont

    let calc op1 op2 ident1 ident2 envs cont =
        let op x y =
            match x, y with
            | SRational(a1, a2), SRational(b1, b2) -> op1 a1 a2 b1 b2
            | SRational(a1, a2), SReal b -> op2 (toFloat a1 a2) b
            | SReal a, SRational(b1, b2) -> op2 a (toFloat b1 b2)
            | SReal a, SReal b -> op2 a b
            | a, b -> sprintf "'%s', '%s' not number." (Print.print a) (Print.print b) |> failwith

        function
        | [] -> SRational(ident1, 1I) |> cont
        | [ SRational(x1, x2) ] -> op1 ident1 1I x1 x2 |> cont
        | [ SReal x ] -> op2 ident2 x |> cont
        | x :: xs -> List.fold op x xs |> cont

    let addNumber envs cont args =
        calc
            (fun a1 a2 b1 b2 -> newSRational (a1 * b2 + b1 * a2) (a2 * b2))
            (fun n1 n2 -> n1 + n2 |> SReal)
            0I
            0.0
            envs
            cont
            args

    let multiplyNumber envs cont args =
        calc (fun a1 a2 b1 b2 -> newSRational (a1 * b1) (a2 * b2)) (fun n1 n2 -> n1 * n2 |> SReal) 1I 1.0 envs cont args

    let subtractNumber envs cont args =
        calc
            (fun a1 a2 b1 b2 -> newSRational (a1 * b2 - b1 * a2) (a2 * b2))
            (fun n1 n2 -> n1 - n2 |> SReal)
            0I
            0.0
            envs
            cont
            args

    let divideNumber envs cont args =
        calc (fun a1 a2 b1 b2 -> newSRational (a1 * b2) (a2 * b1)) (fun n1 n2 -> n1 / n2 |> SReal) 1I 1.0 envs cont args
