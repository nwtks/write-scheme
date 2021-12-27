module Builtin

open Type
open Eval

let isEqual cont =
    let rec equal =
        function
        | SBool a, SBool b -> a = b
        | SNumber a, SNumber b -> a = b
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
    | [ a; b ] -> (a, b) |> equal |> SBool |> cont

let isBoolean cont =
    function
    | [ SBool _ ] -> SBool true |> cont
    | _ -> SBool false |> cont

let isString cont =
    function
    | [ SString _ ] -> SBool true |> cont
    | _ -> SBool false |> cont

let isNumber cont =
    function
    | [ SNumber _ ] -> SBool true |> cont
    | _ -> SBool false |> cont

let isSymbol cont =
    function
    | [ SSymbol _ ] -> SBool true |> cont
    | _ -> SBool false |> cont

let isChar cont =
    function
    | [ SChar _ ] -> SBool true |> cont
    | _ -> SBool false |> cont

let isPair cont =
    function
    | [ SList _ ]
    | [ SPair _ ] -> SBool true |> cont
    | _ -> SBool false |> cont

let isNull cont =
    function
    | [ SEmpty ] -> SBool true |> cont
    | _ -> SBool false |> cont

let isProcedure cont =
    function
    | [ SClosure _ ]
    | [ FFunction _ ] -> SBool true |> cont
    | _ -> SBool false |> cont

let calc op ident cont =
    let op' x =
        function
        | SNumber a -> op x a

    function
    | [] -> SNumber ident |> cont
    | [ SNumber n ] -> op ident n |> SNumber |> cont
    | SNumber n :: ns -> List.fold op' n ns |> SNumber |> cont

let addNumber = calc (+) 0.0
let subtractNumber = calc (-) 0.0
let multiplyNumber = calc (*) 1.0
let divideNumber = calc (/) 1.0
let modulusNumber = calc (%) 0.0

let compareNumber pred cont =
    let rec compare x =
        function
        | [] -> true
        | (SNumber a :: xs) -> if pred x a then compare a xs else false

    function
    | SNumber n :: ns -> compare n ns |> SBool |> cont

let equalNumber = compareNumber (=)
let greaterNumber = compareNumber (>)
let greaterEqualNumber = compareNumber (>=)
let lessNumber = compareNumber (<)
let lessEqualNumber = compareNumber (<=)

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
        "%", FFunction modulusNumber |> ref
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
