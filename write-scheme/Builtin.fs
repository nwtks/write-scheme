module Builtin

open Type
open Eval
open Read
open Print

let invalidParameter fmt =
    newList >> print >> sprintf fmt >> failwith

let rec eachEval envs cont acc =
    function
    | [] -> acc |> cont
    | x :: xs ->
        x
        |> eval envs (fun a -> xs |> eachEval envs cont a)

let zipFormals args =
    let zipVarArg vars args' =
        let varsLen = List.length vars
        let argsLen = List.length args'

        if argsLen < varsLen then
            sprintf "%d parameters requires, but %d." varsLen argsLen
            |> failwith

        List.zip vars (args' |> List.take varsLen)
        |> List.map (function
            | SSymbol var, expr -> var, expr
            | x, _ -> print x |> sprintf "'%s' not symbol." |> failwith)

    let argsExpr =
        function
        | [] -> SEmpty
        | [ x ] -> x
        | xs -> xs |> SList |> SQuote

    function
    | SSymbol var -> [ var, args |> argsExpr ]
    | SEmpty -> []
    | SList vars -> zipVarArg vars args
    | SPair (vars, SSymbol var) ->
        let varsLen = List.length vars

        zipVarArg vars (args |> List.take varsLen)
        @ [ var, args |> List.skip varsLen |> argsExpr ]
    | x -> print x |> sprintf "'%s' not symbol." |> failwith

let eachBinding =
    function
    | SList [ SSymbol var; expr ] -> var, expr
    | x -> print x |> sprintf "'%s' not symbol." |> failwith

let sQuote envs cont =
    function
    | [ x ] -> x |> cont
    | x ->
        x
        |> invalidParameter "'%s' invalid quote parameter."

let sLambda envs cont =
    let rec bindArgs body envs' cont' acc =
        function
        | [] ->
            body
            |> eachEval (List.rev acc |> extendEnvs envs') cont' SEmpty
        | (var, expr) :: xs ->
            expr
            |> eval envs' (fun a ->
                xs
                |> bindArgs body envs' cont' ((var, ref a) :: acc))

    let closure formals body envs' cont' args =
        formals
        |> zipFormals args
        |> bindArgs body (envs' @ envs) cont' []

    function
    | formals :: body -> closure formals body |> SSyntax |> cont
    | x ->
        x
        |> invalidParameter "'%s' invalid lambda parameter."

let sMacro envs cont =
    let closure formals body envs' cont' args =
        body
        |> eachEval
            (formals
             |> zipFormals args
             |> List.map (fun (var, expr) -> (var, ref expr))
             |> extendEnvs envs)
            (eval envs' cont')
            SEmpty

    function
    | formals :: body -> closure formals body |> SSyntax |> cont
    | x ->
        x
        |> invalidParameter "'%s' invalid macro parameter."

let sIf envs cont =
    let if' test conseq alter =
        test
        |> eval envs (function
            | SBool false -> alter |> eval envs cont
            | _ -> conseq |> eval envs cont)

    function
    | [ test; conseq; alter ] -> if' test conseq alter
    | [ test; conseq ] -> if' test conseq SEmpty
    | x -> x |> invalidParameter "'%s' invalid if parameter."

let sSet envs cont =
    function
    | [ SSymbol var; expr ] ->
        expr
        |> eval envs (fun x ->
            (lookupEnvs envs var).Value <- x
            x |> cont)
    | x ->
        x
        |> invalidParameter "'%s' invalid set! parameter."

let rec sCond envs cont =
    let eachTest conseq clauses =
        eval envs (function
            | SBool false -> clauses |> sCond envs cont
            | x -> conseq x)

    function
    | [] -> SEmpty |> cont
    | [ SList (SSymbol "else" :: exprs) ] -> exprs |> eachEval envs cont SEmpty
    | SList [ test; SSymbol "=>"; expr ] :: clauses ->
        test
        |> eachTest (fun a -> [ expr; SQuote a ] |> newList |> eval envs cont) clauses
    | SList (test :: exprs) :: clauses ->
        test
        |> eachTest (fun a -> exprs |> eachEval envs cont a) clauses
    | x ->
        x
        |> invalidParameter "'%s' invalid cond parameter."

let rec sAnd envs cont =
    function
    | [] -> STrue |> cont
    | [ test ] ->
        test
        |> eval envs (function
            | SBool false -> SFalse |> cont
            | x -> x |> cont)
    | test :: tests ->
        test
        |> eval envs (function
            | SBool false -> SFalse |> cont
            | _ -> tests |> sAnd envs cont)

let rec sOr envs cont =
    function
    | [] -> SFalse |> cont
    | test :: tests ->
        test
        |> eval envs (function
            | SBool false -> tests |> sOr envs cont
            | x -> x |> cont)

let sWhen envs cont =
    function
    | test :: exprs ->
        test
        |> eval envs (function
            | SBool false -> SEmpty |> cont
            | _ -> exprs |> eachEval envs cont SEmpty)
    | x ->
        x
        |> invalidParameter "'%s' invalid when parameter."

let sUnless envs cont =
    function
    | test :: exprs ->
        test
        |> eval envs (function
            | SBool false -> exprs |> eachEval envs cont SEmpty
            | _ -> SEmpty |> cont)
    | x ->
        x
        |> invalidParameter "'%s' invalid unless parameter."

let sLet envs cont =
    let rec bind body acc =
        function
        | [] ->
            body
            |> eachEval (List.rev acc |> extendEnvs envs) cont SEmpty
        | (var, expr) :: xs ->
            expr
            |> eval envs (fun a -> xs |> bind body ((var, ref a) :: acc))

    function
    | SList bindings :: body -> bindings |> List.map eachBinding |> bind body []
    | x ->
        x
        |> invalidParameter "'%s' invalid let parameter."

let sLetStar envs cont =
    let rec bind body envs' =
        function
        | [] -> body |> eachEval envs' cont SEmpty
        | (var, expr) :: xs ->
            expr
            |> eval envs' (fun a ->
                xs
                |> bind body ([ var, ref a ] |> extendEnvs envs'))

    function
    | SList bindings :: body -> bindings |> List.map eachBinding |> bind body envs
    | x ->
        x
        |> invalidParameter "'%s' invalid let* parameter."

let sLetRec envs cont =
    let bindRef bindings =
        bindings
        |> List.map (function
            | (var, _) -> var, ref SEmpty)
        |> extendEnvs envs

    let rec bindExpr body envs' =
        function
        | [] -> body |> eachEval envs' cont SEmpty
        | (var, expr) :: xs ->
            expr
            |> eval envs' (fun a ->
                envs'.Head.[var].Value <- a
                xs |> bindExpr body envs')

    function
    | SList bindings :: body ->
        let bindings' = bindings |> List.map eachBinding
        bindings' |> bindExpr body (bindings' |> bindRef)
    | x ->
        x
        |> invalidParameter "'%s' invalid letrec parameter."

let sLetRecStar envs cont =
    let eachRef (envs', refs) =
        function
        | (var, _) ->
            let r = ref SEmpty
            [ var, r ] |> extendEnvs envs', r :: refs

    let bindRef bindings =
        let envs', refs =
            bindings |> List.fold eachRef (envs, [])

        envs', List.rev refs

    let rec bindExpr body envs' =
        function
        | [], _
        | _, [] -> body |> eachEval envs' cont SEmpty
        | (_, expr) :: xs, (r: SExpression ref) :: rs ->
            expr
            |> eval envs' (fun a ->
                r.Value <- a
                (xs, rs) |> bindExpr body envs')

    function
    | SList bindings :: body ->
        let bindings' = bindings |> List.map eachBinding
        let envs', refs = bindRef bindings'
        (bindings', refs) |> bindExpr body envs'
    | x ->
        x
        |> invalidParameter "'%s' invalid letrec* parameter."

let sBegin envs cont = eachEval envs cont SEmpty

let sQuasiquote envs cont =
    let cons x =
        function
        | SEmpty -> [ x ] |> newList
        | SList ys -> x :: ys |> newList
        | y -> [ x; y ] |> newList

    let join =
        function
        | SEmpty, SEmpty -> SEmpty
        | SList xs, SEmpty -> xs |> newList
        | SEmpty, SList ys -> ys |> newList
        | SList xs, SList ys -> xs @ ys |> newList
        | x, y -> [ x; y ] |> newList

    let rec replace n next =
        function
        | SEmpty -> SEmpty |> next
        | SList xs -> replaceList n xs |> next
        | SPair (x1, x2) ->
            match replaceList n x1 with
            | SList ys -> SPair(ys, replaceDatum n x2) |> next
            | _ -> replaceDatum n x2 |> next
        | x -> replaceDatum n x |> next

    and replaceList n =
        function
        | [] -> SEmpty
        | SUnquote x :: xs
        | SList [ SSymbol "unquote"; x ] :: xs ->
            if n = 0 then
                x
                |> eval envs (fun a ->
                    xs
                    |> newList
                    |> replace n (fun b -> cons a b |> cont))
            else
                x
                |> replace (n - 1) (fun a ->
                    xs
                    |> newList
                    |> replace n (fun b -> cons (SUnquote a) b))
        | SUnquoteSplicing x :: xs
        | SList [ SSymbol "unquote-splicing"; x ] :: xs ->
            if n = 0 then
                x
                |> eval envs (fun a ->
                    xs
                    |> newList
                    |> replace n (fun b -> join (a, b) |> cont))
            else
                x
                |> replace (n - 1) (fun a ->
                    xs
                    |> newList
                    |> replace n (fun b -> cons (SUnquoteSplicing a) b))
        | SQuasiquote x :: xs
        | SList [ SSymbol "quasiquote"; x ] :: xs ->
            x
            |> replace (n + 1) (fun a ->
                xs
                |> newList
                |> replace n (fun b -> cons (SQuasiquote a) b))
        | SQuote x :: xs
        | SList [ SSymbol "quote"; x ] :: xs ->
            x
            |> replace n (fun a ->
                xs
                |> newList
                |> replace n (fun b -> cons (SQuote a) b))
        | x :: xs ->
            x
            |> replace n (fun a -> xs |> newList |> replace n (fun b -> cons a b))

    and replaceDatum n =
        function
        | SUnquote x
        | SList [ SSymbol "unquote"; x ] ->
            if n = 0 then
                x |> eval envs cont
            else
                x |> replace (n - 1) SUnquote
        | SUnquoteSplicing x
        | SList [ SSymbol "unquote-splicing"; x ] ->
            if n = 0 then
                x |> eval envs cont
            else
                x |> replace (n - 1) SUnquoteSplicing
        | SQuasiquote x
        | SList [ SSymbol "quasiquote"; x ] -> x |> replace (n + 1) SQuasiquote
        | SQuote x
        | SList [ SSymbol "quote"; x ] -> x |> replace n SQuote
        | x -> x

    function
    | [ x ] -> replace 0 id x
    | x ->
        x
        |> invalidParameter "'%s' invalid quasiquote parameter."

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
        |> invalidParameter "'%s' invalid define parameter."

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
    | [ a; b ] -> (a, b) |> eqv |> newBool |> cont
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

let isNumber envs cont =
    function
    | [ SRational _ ]
    | [ SReal _ ] -> STrue |> cont
    | _ -> SFalse |> cont

let toFloat x y = (float) x / (float) y

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
let lessNumber = compareNumber (<) (<)
let greaterNumber = compareNumber (>) (>)
let lessEqualNumber = compareNumber (<=) (<=)
let greaterEqualNumber = compareNumber (>=) (>=)

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

let multiplyNumber =
    calc (fun a1 a2 b1 b2 -> newRational (a1 * b1) (a2 * b2)) (fun n1 n2 -> n1 * n2 |> SReal) 1I 1.0

let subtractNumber =
    calc (fun a1 a2 b1 b2 -> newRational (a1 * b2 - b1 * a2) (a2 * b2)) (fun n1 n2 -> n1 - n2 |> SReal) 0I 0.0

let divideNumber =
    calc (fun a1 a2 b1 b2 -> newRational (a1 * b2) (a2 * b1)) (fun n1 n2 -> n1 / n2 |> SReal) 1I 1.0

let sNot envs cont =
    function
    | [ SBool false ] -> STrue |> cont
    | _ -> SFalse |> cont

let isBoolean envs cont =
    function
    | [ SBool _ ] -> STrue |> cont
    | _ -> SFalse |> cont

let isPair envs cont =
    function
    | [ SList _ ]
    | [ SPair _ ] -> STrue |> cont
    | _ -> SFalse |> cont

let sCons envs cont =
    function
    | [ x; SEmpty ] -> [ x ] |> newList |> cont
    | [ x; SList xs ] -> x :: xs |> newList |> cont
    | [ x; SPair (y1, y2) ] -> SPair(x :: y1, y2) |> cont
    | [ x; y ] -> SPair([ x ], y) |> cont
    | x ->
        x
        |> invalidParameter "'%s' invalid cons parameter."

let sCar envs cont =
    function
    | [ SList (x :: _) ] -> x |> cont
    | [ SPair (x :: _, _) ] -> x |> cont
    | x ->
        x
        |> invalidParameter "'%s' invalid car parameter."

let sCdr envs cont =
    function
    | [ SList (_ :: xs) ] -> xs |> newList |> cont
    | [ SPair ([ _ ], x2) ] -> x2 |> cont
    | [ SPair (_ :: xs, x2) ] -> SPair(xs, x2) |> cont
    | x ->
        x
        |> invalidParameter "'%s' invalid cdr parameter."

let isNull envs cont =
    function
    | [ SEmpty ] -> STrue |> cont
    | _ -> SFalse |> cont

let isList envs cont =
    function
    | [ SList _ ]
    | [ SEmpty ] -> STrue |> cont
    | _ -> SFalse |> cont

let sList envs cont xs = xs |> newList |> cont

let sAppend envs cont xs =
    let rec fold =
        function
        | [], []
        | [], [ SEmpty ] -> SEmpty
        | [], [ x ] -> x
        | acc, []
        | acc, [ SEmpty ] -> acc |> newList
        | acc, [ SList x ] -> acc @ x |> newList
        | acc, [ SPair (x1, x2) ] -> SPair(acc @ x1, x2)
        | acc, [ x ] -> SPair(acc, x)
        | acc, SEmpty :: x -> (acc, x) |> fold
        | acc, SList x1 :: x2 -> (acc @ x1, x2) |> fold
        | _ ->
            xs
            |> invalidParameter "'%s' invalid append parameter."

    ([], xs) |> fold |> cont

let isSymbol envs cont =
    function
    | [ SSymbol _ ] -> STrue |> cont
    | _ -> SFalse |> cont

let isChar envs cont =
    function
    | [ SChar _ ] -> STrue |> cont
    | _ -> SFalse |> cont

let isString envs cont =
    function
    | [ SString _ ] -> STrue |> cont
    | _ -> SFalse |> cont

let isProcedure envs cont =
    function
    | [ SSyntax _ ]
    | [ SProcedure _ ]
    | [ SContinuation _ ] -> STrue |> cont
    | _ -> SFalse |> cont

let sApply envs cont =
    let rec fold =
        function
        | acc, []
        | acc, [ SEmpty ] -> List.rev acc
        | acc, [ SList x ] -> List.rev acc @ x
        | acc, [ SPair (x1, x2) ] -> [ SPair(List.rev acc @ x1, x2) ]
        | acc, x1 :: x2 -> (x1 :: acc, x2) |> fold

    function
    | proc :: args -> apply envs cont (([], args) |> fold) proc
    | x ->
        x
        |> invalidParameter "'%s' invalid apply parameter."

let sCallCC envs cont =
    function
    | [ proc ] -> apply envs cont [ SContinuation cont ] proc
    | x ->
        x
        |> invalidParameter "'%s' invalid call/cc parameter."

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
        |> invalidParameter "'%s' invalid display parameter."

let sLoad envs cont =
    function
    | [ SString f ] ->
        System.IO.File.ReadAllText(f)
        |> read
        |> eval envs cont
        |> ignore

        sprintf "Loaded '%s'." f |> SSymbol |> cont
    | x ->
        x
        |> invalidParameter "'%s' invalid load parameter."

let builtin =
    extendEnvs [] [
        "quote", SSyntax sQuote |> ref
        "lambda", SSyntax sLambda |> ref
        "macro", SSyntax sMacro |> ref
        "if", SSyntax sIf |> ref
        "set!", SSyntax sSet |> ref
        "cond", SSyntax sCond |> ref
        "and", SSyntax sAnd |> ref
        "or", SSyntax sOr |> ref
        "when", SSyntax sWhen |> ref
        "unless", SSyntax sUnless |> ref
        "let", SSyntax sLet |> ref
        "let*", SSyntax sLetStar |> ref
        "letrec", SSyntax sLetRec |> ref
        "letrec*", SSyntax sLetRecStar |> ref
        "begin", SSyntax sBegin |> ref
        "quasiquote", SSyntax sQuasiquote |> ref
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
        "zero?", SProcedure isZero |> ref
        "positive?", SProcedure isPositive |> ref
        "negative?", SProcedure isNegative |> ref
        "+", SProcedure addNumber |> ref
        "*", SProcedure multiplyNumber |> ref
        "-", SProcedure subtractNumber |> ref
        "/", SProcedure divideNumber |> ref
        "not", SProcedure sNot |> ref
        "boolean?", SProcedure isBoolean |> ref
        "pair?", SProcedure isPair |> ref
        "cons", SProcedure sCons |> ref
        "car", SProcedure sCar |> ref
        "cdr", SProcedure sCdr |> ref
        "null?", SProcedure isNull |> ref
        "list?", SProcedure isList |> ref
        "list", SProcedure sList |> ref
        "append", SProcedure sAppend |> ref
        "symbol?", SProcedure isSymbol |> ref
        "char?", SProcedure isChar |> ref
        "string?", SProcedure isString |> ref
        "procedure?", SProcedure isProcedure |> ref
        "apply", SProcedure sApply |> ref
        "call/cc", SProcedure sCallCC |> ref
        "call-with-current-continuation", SProcedure sCallCC |> ref
        "display", SProcedure sDisplay |> ref
        "load", SProcedure sLoad |> ref
    ]
