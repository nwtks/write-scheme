module Builtin

open Type
open Eval
open Read

let zipArgs args =
    let zip xs args' =
        List.zip xs args'
        |> List.map (function
            | (SSymbol x, y) -> x, y)

    function
    | SSymbol x -> [ (x, args |> newList |> SQuote) ]
    | SEmpty -> []
    | SList xs -> zip xs args
    | SPair (xs, SSymbol y) ->
        zip xs (args |> List.take (List.length xs))
        @ [ (y,
             args
             |> List.skip (List.length xs)
             |> newList
             |> SQuote) ]

let sLambda envs cont =
    function
    | [ args; body ] ->
        let rec bind acc envs' cont' =
            function
            | [] ->
                body
                |> eval (List.rev acc |> extendEnvs (envs @ envs')) cont'
            | (x, y) :: xs ->
                y
                |> eval envs' (fun a -> xs |> bind ((x, ref a) :: acc) envs' cont')

        let closure envs' cont' args' =
            zipArgs args' args |> bind [] envs' cont'

        SClosure closure |> cont

let sDefineMacro envs cont =
    function
    | [ args; body ] ->
        let closure envs' cont' args' =
            body
            |> eval
                (zipArgs args' args
                 |> List.map (fun (x, y) -> x, ref y)
                 |> extendEnvs envs)
                (eval envs' cont')

        SClosure closure |> cont

let sIf envs cont =
    let if' test conseq alter =
        test
        |> eval envs (function
            | SBool false -> alter |> eval envs cont
            | _ -> conseq |> eval envs cont)

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
    let loop test conseq xs =
        test
        |> eval envs (function
            | SBool false -> xs |> sCond envs cont
            | x -> conseq x)

    let rec each acc =
        function
        | [] -> acc |> cont
        | [ x ] -> x |> eval envs cont
        | x :: xs -> x |> eval envs (fun _ -> xs |> each acc)

    function
    | [] -> SEmpty |> cont
    | [ SList (SSymbol "else" :: xs) ] -> xs |> each SEmpty
    | SList [ test; SSymbol "=>"; op ] :: xs ->
        xs
        |> loop test (fun a -> [ op; SQuote a ] |> newList |> eval envs cont)
    | SList (test :: ys) :: xs -> xs |> loop test (fun a -> ys |> each a)

let rec sAnd envs cont =
    function
    | [] -> STrue |> cont
    | [ x ] ->
        x
        |> eval envs (function
            | SBool false -> SFalse
            | a -> a)
        |> cont
    | x :: xs ->
        x
        |> eval envs (function
            | SBool false -> SFalse
            | _ -> xs |> sAnd envs cont)
        |> cont

let rec sOr envs cont =
    function
    | [] -> SFalse |> cont
    | x :: xs ->
        x
        |> eval envs (function
            | SBool false -> xs |> sOr envs cont
            | a -> a)
        |> cont

let sLet envs cont =
    let rec bind acc body =
        function
        | [] ->
            body
            |> eval (List.rev acc |> extendEnvs envs) cont
        | SList [ SSymbol x; y ] :: xs ->
            y
            |> eval envs (fun a -> xs |> bind ((x, ref a) :: acc) body)

    function
    | [ SList bindings; body ] -> bindings |> bind [] body

let sLetStar envs cont =
    let rec bind envs' body =
        function
        | [] -> body |> eval envs' cont
        | SList [ SSymbol x; y ] :: xs ->
            y
            |> eval envs' (fun a -> xs |> bind ([ x, ref a ] |> extendEnvs envs') body)

    function
    | [ SList bindings; body ] -> bindings |> bind envs body

let sLetRec envs cont =
    function
    | [ SList bindings; body ] ->
        let envs' =
            bindings
            |> List.map (function
                | SList [ SSymbol x; _ ] -> (x, ref SEmpty))
            |> extendEnvs envs

        let rec update =
            function
            | [] -> body |> eval envs' cont
            | SList [ SSymbol x; y ] :: xs ->
                y
                |> eval envs' (fun a ->
                    envs'.Head.[x].Value <- a
                    update xs)

        update bindings

let sLetRecStar envs cont =
    function
    | [ SList bindings; body ] ->
        let envs', refs =
            bindings
            |> List.fold
                (fun (envs'', refs') ->
                    function
                    | SList [ SSymbol x; _ ] ->
                        let r = ref SEmpty
                        [ x, r ] |> extendEnvs envs'', r :: refs')
                (envs, [])

        let rec update =
            function
            | ([], []) -> body |> eval envs' cont
            | (SList [ _; y ] :: xs, r :: rs) ->
                y
                |> eval envs' (fun a ->
                    r := a
                    update (xs, rs))

        update (bindings, List.rev refs)

let sBegin envs cont =
    let rec each acc =
        function
        | [] -> acc |> cont
        | x :: xs -> x |> eval envs (fun a -> xs |> each a)

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
        |> cont
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

let toFloat x y = (float) x / (float) y

let calc op1 op2 ident1 ident2 cont =
    let op x y =
        match x, y with
        | SRational (a1, a2), SRational (b1, b2) -> op1 a1 a2 b1 b2
        | SRational (a1, a2), SReal b -> op2 (toFloat a1 a2) b
        | SReal a, SRational (b1, b2) -> op2 a (toFloat b1 b2)
        | SReal a, SReal b -> op2 a b

    function
    | [] -> SRational(ident1, 1I) |> cont
    | [ SRational (x1, x2) ] -> op1 ident1 1I x1 x2 |> cont
    | [ SReal x ] -> op2 ident2 x |> cont
    | x :: xs -> List.fold op x xs |> cont

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
    | x :: xs -> compare x xs |> cont

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
    | [ SList (_ :: xs) ] -> xs |> newList |> cont
    | [ SPair ([ _ ], y) ] -> y |> cont
    | [ SPair (_ :: xs, y) ] -> SPair(xs, y) |> cont

let sCons cont =
    function
    | [ x; SEmpty ] -> [ x ] |> newList |> cont
    | [ x; SList xs ] -> x :: xs |> newList |> cont
    | [ x; SPair (xs, y) ] -> SPair(x :: xs, y) |> cont
    | [ x; y ] -> SPair([ x ], y) |> cont

let sList cont xs = xs |> newList |> cont

let sLoad envs cont =
    function
    | [ SString f ] ->
        System.IO.File.ReadAllText(f)
        |> read
        |> eval envs id
        |> ignore

        sprintf "Loaded '%s'." f |> SSymbol |> cont

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
