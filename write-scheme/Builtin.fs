module Builtin

open Type
open Eval

let isEqual cont =
    let rec equal =
        function
        | SBool a, SBool b -> a = b
        | SRational (a1, a2), SRational (b1, b2) -> a1 = b1 && a2 = b2
        | SReal a, SReal b -> a = b
        | SString a, SString b -> a = b
        | SChar a, SChar b -> a = b
        | SSymbol a, SSymbol b -> a = b
        | SQuote a, SQuote b -> (a, b) |> equal
        | SUnquote a, SUnquote b -> (a, b) |> equal
        | SList a, SList b ->
            a.Length = b.Length
            && List.zip a b |> List.forall equal
        | SPair (a1, a2), SPair (b1, b2) ->
            a1.Length = b1.Length
            && List.zip a1 b1 |> List.forall equal
            && equal (a2, b2)
        | a, b -> a = b

    function
    | [ a; b ] -> (a, b) |> equal |> newSBool |> cont
    | _ -> SFalse |> cont

let isBoolean cont =
    function
    | [ SBool _ ] -> STrue |> cont
    | _ -> SFalse |> cont

let isString cont =
    function
    | [ SString _ ] -> STrue |> cont
    | _ -> SFalse |> cont

let isNumber cont =
    function
    | [ SRational _ ]
    | [ SReal _ ] -> STrue |> cont
    | _ -> SFalse |> cont

let isSymbol cont =
    function
    | [ SSymbol _ ] -> STrue |> cont
    | _ -> SFalse |> cont

let isChar cont =
    function
    | [ SChar _ ] -> STrue |> cont
    | _ -> SFalse |> cont

let isPair cont =
    function
    | [ SList _ ]
    | [ SPair _ ] -> STrue |> cont
    | _ -> SFalse |> cont

let isNull cont =
    function
    | [ SEmpty ] -> STrue |> cont
    | _ -> SFalse |> cont

let isProcedure cont =
    function
    | [ SClosure _ ]
    | [ FFunction _ ] -> STrue |> cont
    | _ -> SFalse |> cont

let toFloat n1 n2 = (float) n1 / (float) n2

let calc op1 op2 ident1 ident2 cont =
    let op n1 n2 =
        match n1, n2 with
        | SRational (a1, a2), SRational (b1, b2) -> op1 a1 a2 b1 b2
        | SRational (a1, a2), SReal b -> op2 (toFloat a1 a2) b
        | SReal a, SRational (b1, b2) -> op2 a (toFloat b1 b2)
        | SReal a, SReal b -> op2 a b

    function
    | [] -> SRational(ident1, 1I) |> cont
    | [ SRational (n1, n2) ] -> op1 ident1 1I n1 n2 |> cont
    | [ SReal n ] -> op2 ident2 n |> cont
    | n :: ns -> List.fold op n ns |> cont

let addNumber =
    calc (fun a1 a2 b1 b2 -> newSRational (a1 * b2 + b1 * a2) (a2 * b2)) (fun n1 n2 -> n1 + n2 |> SReal) 0I 0.0

let subtractNumber =
    calc (fun a1 a2 b1 b2 -> newSRational (a1 * b2 - b1 * a2) (a2 * b2)) (fun n1 n2 -> n1 - n2 |> SReal) 0I 0.0

let multiplyNumber =
    calc (fun a1 a2 b1 b2 -> newSRational (a1 * b1) (a2 * b2)) (fun n1 n2 -> n1 * n2 |> SReal) 1I 1.0

let divideNumber =
    calc (fun a1 a2 b1 b2 -> newSRational (a1 * b2) (a2 * b1)) (fun n1 n2 -> n1 / n2 |> SReal) 1I 1.0

let compareNumber pred1 pred2 cont =
    let pred =
        function
        | SRational (a1, a2), SRational (b1, b2) -> pred1 (a1 * b2) (b1 * a2)
        | SRational (a1, a2), SReal b -> pred2 (toFloat a1 a2) b
        | SReal a, SRational (b1, b2) -> pred2 a (toFloat b1 b2)
        | SReal a, SReal b -> pred2 a b
        | _ -> false

    let rec compare n =
        function
        | [] -> STrue
        | x :: xs ->
            if pred (n, x) then
                compare x xs
            else
                SFalse

    function
    | [] -> STrue
    | n :: ns -> compare n ns |> cont

let equalNumber = compareNumber (=) (=)
let greaterNumber = compareNumber (>) (>)
let greaterEqualNumber = compareNumber (>=) (>=)
let lessNumber = compareNumber (<) (<)
let lessEqualNumber = compareNumber (<=) (<=)

let sCar cont =
    function
    | [ SList (x :: _) ] -> x |> cont
    | [ SPair (x :: _, _) ] -> x |> cont

let sCdr cont =
    function
    | [ SList (_ :: xs) ] -> SList xs |> cont
    | [ SPair ([ _ ], x) ] -> x |> cont
    | [ SPair (_ :: xs, x) ] -> SPair(xs, x) |> cont

let sCons cont =
    function
    | [ x; SList xs ] -> x :: xs |> SList |> cont
    | [ x; SPair (xs, rest) ] -> SPair(x :: xs, rest) |> cont
    | [ x; SEmpty ] -> SList [ x ] |> cont
    | [ x; y ] -> SPair([ x ], y) |> cont

let sList cont args = SList args |> cont

let builtin =
    extendEnvs [] [
        "equal?", FFunction isEqual |> ref
        "boolean?", FFunction isBoolean |> ref
        "string?", FFunction isString |> ref
        "number?", FFunction isNumber |> ref
        "symbol?", FFunction isSymbol |> ref
        "char?", FFunction isChar |> ref
        "pair?", FFunction isPair |> ref
        "null?", FFunction isNull |> ref
        "procedure?", FFunction isProcedure |> ref
        "+", FFunction addNumber |> ref
        "-", FFunction subtractNumber |> ref
        "*", FFunction multiplyNumber |> ref
        "/", FFunction divideNumber |> ref
        "=", FFunction equalNumber |> ref
        ">", FFunction greaterNumber |> ref
        ">=", FFunction greaterEqualNumber |> ref
        "<", FFunction lessNumber |> ref
        "<=", FFunction lessEqualNumber |> ref
        "car", FFunction sCar |> ref
        "cdr", FFunction sCdr |> ref
        "cons", FFunction sCons |> ref
        "list", FFunction sList |> ref
    ]
