module Builtin

open Type
open Eval
open Read
open Print

let rec evalEach envs cont acc =
    function
    | [] -> acc |> cont
    | x :: xs ->
        x
        |> eval envs (fun a -> xs |> evalEach envs cont a)

let zipFormals args =
    let zip xs args' =
        List.zip xs args'
        |> List.map (function
            | SSymbol x, y -> x, ref y
            | x, _ -> print x |> sprintf "'%s' not symbol." |> failwith)

    let argVal =
        function
        | [] -> SEmpty
        | [ x ] -> x
        | xs -> xs |> SList |> SQuote

    function
    | SSymbol x -> [ x, args |> argVal |> ref ]
    | SEmpty -> []
    | SList xs -> zip xs args
    | SPair (xs, SSymbol y) ->
        zip xs (args |> List.take (List.length xs))
        @ [ y,
            args
            |> List.skip (List.length xs)
            |> argVal
            |> ref ]
    | x -> print x |> sprintf "'%s' not symbol." |> failwith

let eachBinding =
    function
    | SList [ SSymbol x; y ] -> x, y
    | x -> print x |> sprintf "'%s' not symbol." |> failwith

let sLambda envs cont =
    let rec bind body envs' cont' acc =
        function
        | [] ->
            body
            |> evalEach (List.rev acc |> extendEnvs envs') cont' SEmpty
        | (x1, (x2: SExpression ref)) :: xs ->
            x2.Value
            |> eval envs' (fun a -> xs |> bind body envs' cont' ((x1, ref a) :: acc))

    let closure formals body envs' cont' args =
        formals
        |> zipFormals args
        |> bind body (envs' @ envs) cont' []

    function
    | formals :: body -> closure formals body |> SSyntax |> cont
    | x ->
        x
        |> newList
        |> print
        |> sprintf "'%s' invalid lambda parameter."
        |> failwith

let sMacro envs cont =
    let closure formals body envs' cont' args =
        body
        |> evalEach (formals |> zipFormals args |> extendEnvs envs) (eval envs' cont') SEmpty

    function
    | formals :: body -> closure formals body |> SSyntax |> cont
    | x ->
        x
        |> newList
        |> print
        |> sprintf "'%s' invalid define-macro parameter."
        |> failwith

let sIf envs cont =
    let if' test conseq alter =
        test
        |> eval envs (function
            | SBool false -> alter |> eval envs cont
            | _ -> conseq |> eval envs cont)

    function
    | [ test; conseq; alter ] -> if' test conseq alter
    | [ test; conseq ] -> if' test conseq SEmpty
    | x ->
        x
        |> newList
        |> print
        |> sprintf "'%s' invalid if parameter."
        |> failwith

let sSet envs cont =
    function
    | [ SSymbol var; expr ] ->
        expr
        |> eval envs (fun x ->
            (lookupEnvs envs var).Value <- x
            x |> cont)
    | x ->
        x
        |> newList
        |> print
        |> sprintf "'%s' invalid set! parameter."
        |> failwith

let sDefine (envs: SEnv list) cont =
    let define' var =
        envs.Head.TryAdd(var, ref SEmpty) |> ignore

        eval envs (fun x ->
            envs.Head.[var].Value <- x
            var |> SSymbol |> cont)

    function
    | [ SSymbol var; expr ] -> expr |> define' var
    | SList (SSymbol var :: formals) :: body ->
        sLambda envs cont (SList(formals) :: body)
        |> define' var
    | SPair ([ SSymbol var ], formal) :: body -> sLambda envs cont (formal :: body) |> define' var
    | x ->
        x
        |> newList
        |> print
        |> sprintf "'%s' invalid define parameter."
        |> failwith

let rec sCond envs cont =
    let loop conseq xs =
        eval envs (function
            | SBool false -> xs |> sCond envs cont
            | x -> conseq x)

    function
    | [] -> SEmpty |> cont
    | [ SList (SSymbol "else" :: xs) ] -> xs |> evalEach envs cont SEmpty
    | SList [ test; SSymbol "=>"; op ] :: xs ->
        test
        |> loop (fun a -> [ op; SQuote a ] |> newList |> eval envs cont) xs
    | SList (test :: ys) :: xs ->
        test
        |> loop (fun a -> ys |> evalEach envs cont a) xs
    | x ->
        x
        |> newList
        |> print
        |> sprintf "'%s' invalid cond parameter."
        |> failwith

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
    let rec bind body acc =
        function
        | [] ->
            body
            |> evalEach (List.rev acc |> extendEnvs envs) cont SEmpty
        | (x1, x2) :: xs ->
            x2
            |> eval envs (fun a -> xs |> bind body ((x1, ref a) :: acc))

    function
    | SList bindings :: body -> bindings |> List.map eachBinding |> bind body []
    | x ->
        x
        |> newList
        |> print
        |> sprintf "'%s' invalid let parameter."
        |> failwith

let sLetStar envs cont =
    let rec bind body envs' =
        function
        | [] -> body |> evalEach envs' cont SEmpty
        | (x1, x2) :: xs ->
            x2
            |> eval envs' (fun a ->
                xs
                |> bind body ([ x1, ref a ] |> extendEnvs envs'))

    function
    | SList bindings :: body -> bindings |> List.map eachBinding |> bind body envs
    | x ->
        x
        |> newList
        |> print
        |> sprintf "'%s' invalid let* parameter."
        |> failwith

let sLetRec envs cont =
    let bindRef bindings =
        bindings
        |> List.map (function
            | (x, _) -> x, ref SEmpty)
        |> extendEnvs envs

    let rec update body envs' =
        function
        | [] -> body |> evalEach envs' cont SEmpty
        | (x1, x2) :: xs ->
            x2
            |> eval envs' (fun a ->
                envs'.Head.[x1].Value <- a
                xs |> update body envs')

    function
    | SList bindings :: body ->
        let bindings' = bindings |> List.map eachBinding
        bindings' |> update body (bindings' |> bindRef)
    | x ->
        x
        |> newList
        |> print
        |> sprintf "'%s' invalid letrec parameter."
        |> failwith

let sLetRecStar envs cont =
    let eachRef (envs', refs) =
        function
        | (x, _) ->
            let r = ref SEmpty
            [ x, r ] |> extendEnvs envs', r :: refs

    let bindRef bindings =
        let envs', refs =
            bindings |> List.fold eachRef (envs, [])

        envs', List.rev refs

    let rec update body envs' =
        function
        | [], _
        | _, [] -> body |> evalEach envs' cont SEmpty
        | (_, x) :: xs, (r: SExpression ref) :: rs ->
            x
            |> eval envs' (fun a ->
                r.Value <- a
                (xs, rs) |> update body envs')

    function
    | SList bindings :: body ->
        let bindings' = bindings |> List.map eachBinding
        let envs', refs = bindRef bindings'
        (bindings', refs) |> update body envs'
    | x ->
        x
        |> newList
        |> print
        |> sprintf "'%s' invalid letrec* parameter."
        |> failwith

let sBegin envs cont = evalEach envs cont SEmpty

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

let isEqual envs cont =
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

let isBoolean envs cont =
    function
    | [ SBool _ ] -> STrue |> cont
    | _ -> SFalse |> cont

let isString envs cont =
    function
    | [ SString _ ] -> STrue |> cont
    | _ -> SFalse |> cont

let isNumber envs cont =
    function
    | [ SRational _ ]
    | [ SReal _ ] -> STrue |> cont
    | _ -> SFalse |> cont

let isSymbol envs cont =
    function
    | [ SSymbol _ ] -> STrue |> cont
    | _ -> SFalse |> cont

let isChar envs cont =
    function
    | [ SChar _ ] -> STrue |> cont
    | _ -> SFalse |> cont

let isPair envs cont =
    function
    | [ SList _ ]
    | [ SPair _ ] -> STrue |> cont
    | _ -> SFalse |> cont

let isNull envs cont =
    function
    | [ SEmpty ] -> STrue |> cont
    | _ -> SFalse |> cont

let isProcedure envs cont =
    function
    | [ SSyntax _ ]
    | [ SProcedure _ ] -> STrue |> cont
    | _ -> SFalse |> cont

let toFloat x y = (float) x / (float) y

let calc op1 op2 ident1 ident2 envs cont =
    let op x y =
        match x, y with
        | SRational (a1, a2), SRational (b1, b2) -> op1 a1 a2 b1 b2
        | SRational (a1, a2), SReal b -> op2 (toFloat a1 a2) b
        | SReal a, SRational (b1, b2) -> op2 a (toFloat b1 b2)
        | SReal a, SReal b -> op2 a b
        | a, b ->
            sprintf "'%s', '%s' not number." (print a) (print b)
            |> failwith

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

let compareNumber pred1 pred2 envs cont =
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

let sCar envs cont =
    function
    | [ SList (x :: _) ] -> x |> cont
    | [ SPair (x :: _, _) ] -> x |> cont
    | x ->
        x
        |> newList
        |> print
        |> sprintf "'%s' invalid car parameter."
        |> failwith

let sCdr envs cont =
    function
    | [ SList (_ :: xs) ] -> xs |> newList |> cont
    | [ SPair ([ _ ], y) ] -> y |> cont
    | [ SPair (_ :: xs, y) ] -> SPair(xs, y) |> cont
    | x ->
        x
        |> newList
        |> print
        |> sprintf "'%s' invalid cdr parameter."
        |> failwith

let sCons envs cont =
    function
    | [ x; SEmpty ] -> [ x ] |> newList |> cont
    | [ x; SList xs ] -> x :: xs |> newList |> cont
    | [ x; SPair (xs, y) ] -> SPair(x :: xs, y) |> cont
    | [ x; y ] -> SPair([ x ], y) |> cont
    | x ->
        x
        |> newList
        |> print
        |> sprintf "'%s' invalid cons parameter."
        |> failwith

let sList envs cont xs = xs |> newList |> cont

let sCallCC envs cont =
    function
    | [ proc ] ->
        proc
        |> eval envs (function
            | SProcedure fn -> fn envs cont [ SContinuation cont ]
            | SSyntax fn -> fn envs cont [ SContinuation cont ]
            | x ->
                x
                |> print
                |> sprintf "'%s' invalid call/cc parameter."
                |> failwith)
    | x ->
        x
        |> newList
        |> print
        |> sprintf "'%s' invalid call/cc parameter."
        |> failwith

let sDisplay envs cont =
    function
    | [ SString x ] ->
        x |> printf "%s"
        SEmpty |> cont
    | [ SChar x ] ->
        x |> printf "%s"
        SEmpty |> cont
    | [ x ] ->
        x |> print |> printf "%s"
        SEmpty |> cont
    | x ->
        x
        |> newList
        |> print
        |> sprintf "'%s' invalid display parameter."
        |> failwith

let sLoad envs cont =
    function
    | [ SString f ] ->
        System.IO.File.ReadAllText(f)
        |> read
        |> eval envs id
        |> ignore

        sprintf "Loaded '%s'." f |> SSymbol |> cont
    | x ->
        x
        |> newList
        |> print
        |> sprintf "'%s' invalid load parameter."
        |> failwith

let builtin =
    extendEnvs [] [
        "lambda", SSyntax sLambda |> ref
        "macro", SSyntax sMacro |> ref
        "if", SSyntax sIf |> ref
        "set!", SSyntax sSet |> ref
        "cond", SSyntax sCond |> ref
        "and", SSyntax sAnd |> ref
        "or", SSyntax sOr |> ref
        "let", SSyntax sLet |> ref
        "let*", SSyntax sLetStar |> ref
        "letrec", SSyntax sLetRec |> ref
        "letrec*", SSyntax sLetRecStar |> ref
        "begin", SSyntax sBegin |> ref
        "define", SSyntax sDefine |> ref
        "eqv?", SProcedure isEqv |> ref
        "eq?", SProcedure isEqv |> ref
        "equal?", SProcedure isEqual |> ref
        "number?", SProcedure isNumber |> ref
        "=", SProcedure equalNumber |> ref
        "<", SProcedure lessNumber |> ref
        ">", SProcedure greaterNumber |> ref
        "<=", SProcedure lessEqualNumber |> ref
        ">=", SProcedure greaterEqualNumber |> ref
        "+", SProcedure addNumber |> ref
        "*", SProcedure multiplyNumber |> ref
        "-", SProcedure subtractNumber |> ref
        "/", SProcedure divideNumber |> ref
        "boolean?", SProcedure isBoolean |> ref
        "pair?", SProcedure isPair |> ref
        "cons", SProcedure sCons |> ref
        "car", SProcedure sCar |> ref
        "cdr", SProcedure sCdr |> ref
        "null?", SProcedure isNull |> ref
        "list", SProcedure sList |> ref
        "symbol?", SProcedure isSymbol |> ref
        "char?", SProcedure isChar |> ref
        "string?", SProcedure isString |> ref
        "procedure?", SProcedure isProcedure |> ref
        "call/cc", SProcedure sCallCC |> ref
        "call-with-current-continuation", SProcedure sCallCC |> ref
        "display", SProcedure sDisplay |> ref
        "load", SProcedure sLoad |> ref
    ]
