module Builtin

open Type
open Eval
open Read

let zipArgs args =
    let zip xs args' =
        List.zip xs args'
        |> List.map (function
            | (SSymbol p, a) -> p, a)

    function
    | SSymbol x -> [ (x, args |> SList |> SQuote) ]
    | SList xs -> zip xs args
    | SPair (xs, SSymbol rest) ->
        zip xs (args |> List.take (List.length xs))
        @ [ (rest,
             args
             |> List.skip (List.length xs)
             |> SList
             |> SQuote) ]

let sLambda envs cont =
    function
    | [ parameters; body ] ->
        let rec bind acc envs' cont' =
            function
            | [] ->
                body
                |> eval (List.rev acc |> extendEnvs (envs @ envs')) cont'
            | (p, a) :: xs ->
                a
                |> eval envs' (fun a' -> bind ((p, ref a') :: acc) envs' cont' xs)

        let closure envs' cont' args =
            zipArgs args parameters |> bind [] envs' cont'

        SClosure closure |> cont

let sDefineMacro envs cont =
    function
    | [ parameters; body ] ->
        let closure envs' cont' args =
            body
            |> eval
                (zipArgs args parameters
                 |> List.map (fun (p, a) -> p, ref a)
                 |> extendEnvs envs)
                (eval envs' cont')

        SClosure closure |> cont

let sIf envs cont =
    let if' test conseq alter =
        test
        |> eval envs (function
            | SBool false -> eval envs cont alter
            | _ -> eval envs cont conseq)

    function
    | [ test; conseq; alter ] -> if' test conseq alter
    | [ test; conseq ] -> if' test conseq SEmpty

let sSet envs cont =
    function
    | [ SSymbol var; expr ] ->
        expr
        |> eval envs (fun x ->
            (lookupEnvs envs var).Value <- x
            SEmpty |> cont)

let sDefine (envs: SEnv list) cont =
    let define' var expr =
        envs.Head.TryAdd(var, ref SEmpty) |> ignore

        expr
        |> eval envs (fun x ->
            envs.Head.[var].Value <- x
            SEmpty |> cont)

    function
    | [ SSymbol var; expr ] -> define' var expr
    | [ SList [ SSymbol var; formals ]; body ] -> sLambda envs cont [ formals; body ] |> define' var
    | [ SPair ([ SSymbol var ], formal); body ] -> sLambda envs cont [ formal; body ] |> define' var

let rec sCond envs cont =
    let loop test xs conseq =
        test
        |> eval envs (function
            | SBool false -> sCond envs cont xs
            | res -> conseq res)

    let rec each res =
        function
        | [] -> res |> cont
        | [ e ] -> eval envs cont e
        | e :: exs -> eval envs (fun _ -> each res exs) e

    function
    | [] -> SEmpty |> cont
    | [ SList (SSymbol "else" :: exs) ] -> each SEmpty exs
    | SList [ test; SSymbol "=>"; op ] :: xs -> loop test xs (fun res -> SList [ op; SQuote res ] |> eval envs cont)
    | SList (test :: exs) :: xs -> loop test xs (fun res -> each res exs)

let rec sAnd envs cont =
    function
    | [] -> STrue |> cont
    | [ t ] ->
        t
        |> eval envs (function
            | SBool false -> SFalse
            | x -> x)
    | t :: ts ->
        t
        |> eval envs (function
            | SBool false -> SFalse
            | _ -> sAnd envs cont ts)

let rec sOr envs cont =
    function
    | [] -> SFalse |> cont
    | t :: ts ->
        t
        |> eval envs (function
            | SBool false -> sOr envs cont ts
            | x -> x)

let sLet envs cont =
    let rec bind acc body =
        function
        | [] -> eval (List.rev acc |> extendEnvs envs) cont body
        | SList [ SSymbol s; e ] :: xs -> eval envs (fun x -> bind ((s, ref x) :: acc) body xs) e

    function
    | [ SList bindings; body ] -> bind [] body bindings

let sLetStar envs cont =
    let rec bind envs' body =
        function
        | [] -> eval envs' cont body
        | SList [ SSymbol s; e ] :: xs -> eval envs' (fun x -> bind ([ s, ref x ] |> extendEnvs envs') body xs) e

    function
    | [ SList bindings; body ] -> bind envs body bindings

let sLetRec envs cont =
    function
    | [ SList bindings; body ] ->
        let envs' =
            bindings
            |> List.map (function
                | SList [ SSymbol s; _ ] -> (s, ref SEmpty))
            |> extendEnvs envs

        let rec update =
            function
            | [] -> eval envs' cont body
            | SList [ SSymbol s; e ] :: xs ->
                eval
                    envs'
                    (fun x ->
                        envs'.Head.[s].Value <- x
                        update xs)
                    e

        update bindings

let sLetRecStar envs cont =
    function
    | [ SList bindings; body ] ->
        let envs', refs =
            bindings
            |> List.fold
                (fun (envs'', refs') ->
                    function
                    | SList [ SSymbol s; _ ] ->
                        let r = ref SEmpty
                        [ s, r ] |> extendEnvs envs'', r :: refs')
                (envs, [])

        let rec update =
            function
            | ([], []) -> eval envs' cont body
            | (SList [ _; e ] :: xs, r :: rs) ->
                eval
                    envs'
                    (fun x ->
                        r := x
                        update (xs, rs))
                    e

        update (bindings, List.rev refs)

let sBegin envs cont =
    let rec each res =
        function
        | [] -> res |> cont
        | x :: xs -> x |> eval envs (fun a -> each a xs)

    each SEmpty

let isEqv envs cont =
    let rec eqv =
        function
        | SBool a, SBool b -> a = b
        | SRational (a1, a2), SRational (b1, b2) -> a1 = b1 && a2 = b2
        | SReal a, SReal b -> a = b
        | SString a, SString b -> a = b
        | SChar a, SChar b -> a = b
        | SSymbol a, SSymbol b -> a = b
        | SQuote a, SQuote b -> (a, b) |> eqv
        | SUnquote a, SUnquote b -> (a, b) |> eqv
        | a, b -> a = b

    function
    | [ a; b ] ->
        a
        |> eval envs (fun a' ->
            b
            |> eval envs (fun b' -> (a', b') |> eqv |> newBool))
    | _ -> SFalse |> cont

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
    | [ a; b ] -> (a, b) |> equal |> newBool |> cont
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
    calc (fun a1 a2 b1 b2 -> newRational (a1 * b2 + b1 * a2) (a2 * b2)) (fun n1 n2 -> n1 + n2 |> SReal) 0I 0.0

let subtractNumber =
    calc (fun a1 a2 b1 b2 -> newRational (a1 * b2 - b1 * a2) (a2 * b2)) (fun n1 n2 -> n1 - n2 |> SReal) 0I 0.0

let multiplyNumber =
    calc (fun a1 a2 b1 b2 -> newRational (a1 * b1) (a2 * b2)) (fun n1 n2 -> n1 * n2 |> SReal) 1I 1.0

let divideNumber =
    calc (fun a1 a2 b1 b2 -> newRational (a1 * b2) (a2 * b1)) (fun n1 n2 -> n1 / n2 |> SReal) 1I 1.0

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
    | [] -> STrue |> cont
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

let sLoad envs cont =
    function
    | [ SString file ] ->
        System.IO.File.ReadAllText(file)
        |> read
        |> eval envs id
        |> ignore

        SSymbol(sprintf "Loaded '%s'." file) |> cont

let builtin =
    extendEnvs [] [
        "lambda", SClosure sLambda |> ref
        "define-macro", SClosure sDefineMacro |> ref
        "if", SClosure sIf |> ref
        "set!", SClosure sSet |> ref
        "define", SClosure sDefine |> ref
        "cond", SClosure sCond |> ref
        "and", SClosure sAnd |> ref
        "or", SClosure sOr |> ref
        "let", SClosure sLet |> ref
        "let*", SClosure sLetStar |> ref
        "letrec", SClosure sLetRec |> ref
        "letrec*", SClosure sLetRecStar |> ref
        "begin", SClosure sBegin |> ref
        "eqv?", SClosure isEqv |> ref
        "eq?", SClosure isEqv |> ref
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
        "load", SClosure sLoad |> ref
    ]
