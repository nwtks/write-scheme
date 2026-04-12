namespace WriteScheme.Builtins

open WriteScheme
open Type

[<AutoOpen>]
module Math =
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

    let compareNumber pred1 pred2 cont =
        function
        | [] -> STrue |> cont
        | x :: xs -> xs |> compare pred1 pred2 x |> cont

    let equalNumber envs cont args = compareNumber (=) (=) cont args
    let lessNumber envs cont args = compareNumber (<) (<) cont args
    let greaterNumber envs cont args = compareNumber (>) (>) cont args
    let lessEqualNumber envs cont args = compareNumber (<=) (<=) cont args
    let greaterEqualNumber envs cont args = compareNumber (>=) (>=) cont args

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

    let calc op1 op2 ident1 ident2 cont =
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
            cont
            args

    let multiplyNumber envs cont args =
        calc (fun a1 a2 b1 b2 -> newSRational (a1 * b1) (a2 * b2)) (fun n1 n2 -> n1 * n2 |> SReal) 1I 1.0 cont args

    let subtractNumber envs cont args =
        calc
            (fun a1 a2 b1 b2 -> newSRational (a1 * b2 - b1 * a2) (a2 * b2))
            (fun n1 n2 -> n1 - n2 |> SReal)
            0I
            0.0
            cont
            args

    let divideNumber envs cont args =
        calc (fun a1 a2 b1 b2 -> newSRational (a1 * b2) (a2 * b1)) (fun n1 n2 -> n1 / n2 |> SReal) 1I 1.0 cont args
