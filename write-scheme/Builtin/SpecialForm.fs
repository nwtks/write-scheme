namespace WriteScheme.Builtins

open WriteScheme
open Type

[<AutoOpen>]
module SpecialForm =
    let sQuote envs pos cont =
        function
        | [ x ] -> x |> cont
        | x -> x |> invalidParameter pos "'%s' invalid quote parameter."

    [<TailCall>]
    let rec loopZipFormals pos acc args =
        function
        | SEmpty, _ ->
            if List.isEmpty args then
                acc
            else
                failwithf "Too many arguments.%s" (pos |> formatPosition)
        | SSymbol var, _ -> (var, (args |> toSPair |> SQuote, pos)) :: acc
        | SPair p, _ ->
            match args with
            | h :: t ->
                match p.car with
                | SSymbol var, _ -> p.cdr |> loopZipFormals pos ((var, h) :: acc) t
                | x -> x |> invalid (snd x) "'%s' is not a symbol."
            | [] -> failwithf "Not enough arguments.%s" (pos |> formatPosition)
        | x -> x |> invalid (snd x) "'%s' is not a symbol."

    let zipFormals pos args =
        function
        | SSymbol var, _ -> [ var, (args |> toSPair |> SQuote, pos) ]
        | x -> x |> loopZipFormals pos [] args |> List.rev

    [<TailCall>]
    let rec bindArgs envs pos cont body acc =
        function
        | [] ->
            body
            |> Eval.eachEval (acc |> List.rev |> Context.extendEnvs envs) cont (SEmpty, pos)
        | (var, expr) :: xs ->
            expr
            |> Eval.eval envs (fun a -> xs |> bindArgs envs pos cont body ((var, ref a) :: acc))

    let closure captureEnvs formals body envs pos cont args =
        formals
        |> zipFormals pos args
        |> bindArgs (Context.mergeEnvs envs captureEnvs) pos cont body []

    [<TailCall>]
    let rec loopZipFormalsRef pos acc args =
        function
        | SEmpty, _ ->
            if List.isEmpty args then
                acc
            else
                failwithf "Too many arguments.%s" (pos |> formatPosition)
        | SSymbol var, _ -> (var, ref (args |> toSPair |> SQuote, pos)) :: acc
        | SPair pair, _ ->
            match args with
            | h :: t ->
                match pair.car with
                | SSymbol var, _ -> pair.cdr |> loopZipFormalsRef pos ((var, ref h) :: acc) t
                | x -> x |> invalid (snd x) "'%s' is not a symbol."
            | [] -> failwithf "Not enough arguments.%s" (pos |> formatPosition)
        | x -> x |> invalid (snd x) "'%s' is not a symbol."

    let zipFormalsRef pos args =
        function
        | SSymbol var, _ -> [ var, ref (args |> toSPair |> SQuote, pos) ]
        | x -> x |> loopZipFormalsRef pos [] args |> List.rev

    let sLambda envs pos cont =
        function
        | formals :: body -> (closure envs formals body |> SSyntax, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid lambda parameter."

    let sIf envs pos cont =
        let if' test conseq alter =
            test
            |> Eval.eval envs (function
                | SBool false, _ -> alter |> Eval.eval envs cont
                | _ -> conseq |> Eval.eval envs cont)

        function
        | [ test; conseq; alter ] -> if' test conseq alter
        | [ test; conseq ] -> if' test conseq (SEmpty, pos)
        | x -> x |> invalidParameter pos "'%s' invalid if parameter."

    let sSet envs pos cont =
        function
        | [ SSymbol var, pos'; expr ] ->
            expr
            |> Eval.eval envs (fun x ->
                (Context.lookupEnvs envs pos' var).Value <- x
                x |> cont)
        | x -> x |> invalidParameter pos "'%s' invalid set! parameter."

    [<TailCall>]
    let rec sCond envs pos cont =
        function
        | [] -> (SEmpty, pos) |> cont
        | clause :: clauses ->
            match clause with
            | SPair { car = SSymbol "else", _; cdr = exprs }, _ ->
                exprs |> toList |> Eval.eachEval envs cont (SEmpty, pos)
            | SPair { car = test
                      cdr = SPair { car = SSymbol "=>", _
                                    cdr = SPair { car = expr; cdr = SEmpty, _ }, _ },
                            _ },
              _ ->
                test
                |> Eval.eval envs (function
                    | SBool false, _ -> clauses |> sCond envs pos cont
                    | a -> [ expr; SQuote a, pos ] |> toSPair |> Eval.eval envs cont)
            | SPair { car = test; cdr = exprs }, _ ->
                test
                |> Eval.eval envs (function
                    | SBool false, _ -> clauses |> sCond envs pos cont
                    | a -> exprs |> toList |> Eval.eachEval envs cont a)
            | x -> x |> invalid pos "'%s' invalid cond clause."

    [<TailCall>]
    let rec testCase envs pos cont key =
        function
        | [] -> (SEmpty, pos) |> cont
        | clause :: clauses ->
            match clause with
            | SPair { car = SSymbol "else", _
                      cdr = SPair { car = SSymbol "=>", _
                                    cdr = SPair { car = expr; cdr = SEmpty, _ }, _ },
                            _ },
              _ -> [ expr; SQuote key, pos ] |> toSPair |> Eval.eval envs cont
            | SPair { car = SSymbol "else", _; cdr = exprs }, _ ->
                exprs |> toList |> Eval.eachEval envs cont (SEmpty, pos)
            | SPair { car = datums
                      cdr = SPair { car = SSymbol "=>", _
                                    cdr = SPair { car = expr; cdr = SEmpty, _ }, _ },
                            _ },
              _ ->
                if datums |> toList |> List.exists (fun d -> eqv (key, d)) then
                    [ expr; SQuote key, pos ] |> toSPair |> Eval.eval envs cont
                else
                    clauses |> testCase envs pos cont key
            | SPair { car = datums; cdr = exprs }, _ ->
                if datums |> toList |> List.exists (fun d -> eqv (key, d)) then
                    exprs |> toList |> Eval.eachEval envs cont (SEmpty, pos)
                else
                    clauses |> testCase envs pos cont key
            | _, p as x -> x |> invalid p "'%s' invalid case clause."

    let sCase envs pos cont =
        function
        | key :: clauses -> key |> Eval.eval envs (fun k -> testCase envs pos cont k clauses)
        | x -> x |> invalidParameter pos "'%s' invalid case parameter."

    [<TailCall>]
    let rec sAnd envs pos cont =
        function
        | [] -> (STrue, pos) |> cont
        | [ test ] ->
            test
            |> Eval.eval envs (function
                | SBool false, _ -> (SFalse, pos) |> cont
                | x -> x |> cont)
        | test :: tests ->
            test
            |> Eval.eval envs (function
                | SBool false, _ -> (SFalse, pos) |> cont
                | _ -> tests |> sAnd envs pos cont)

    [<TailCall>]
    let rec sOr envs pos cont =
        function
        | [] -> (SFalse, pos) |> cont
        | test :: tests ->
            test
            |> Eval.eval envs (function
                | SBool false, _ -> tests |> sOr envs pos cont
                | x -> x |> cont)

    let sWhen envs pos cont =
        function
        | test :: exprs ->
            test
            |> Eval.eval envs (function
                | SBool false, _ -> (SEmpty, pos) |> cont
                | _ -> exprs |> Eval.eachEval envs cont (SEmpty, pos))
        | x -> x |> invalidParameter pos "'%s' invalid when parameter."

    let sUnless envs pos cont =
        function
        | test :: exprs ->
            test
            |> Eval.eval envs (function
                | SBool false, _ -> exprs |> Eval.eachEval envs cont (SEmpty, pos)
                | _ -> (SEmpty, pos) |> cont)
        | x -> x |> invalidParameter pos "'%s' invalid unless parameter."

    [<TailCall>]
    let rec bindLet envs pos cont body acc =
        function
        | [] ->
            body
            |> Eval.eachEval (acc |> List.rev |> Context.extendEnvs envs) cont (SEmpty, pos)
        | (var, expr) :: xs ->
            expr
            |> Eval.eval envs (fun a -> xs |> bindLet envs pos cont body ((var, ref a) :: acc))

    [<TailCall>]
    let rec loopBindingLet pos acc args =
        function
        | SEmpty, _ -> acc
        | SSymbol v, _ -> (v, ref (args |> toSPair)) :: acc
        | SPair p, _ ->
            match args with
            | h :: t ->
                match p.car with
                | SSymbol v, _ -> p.cdr |> loopBindingLet pos ((v, ref h) :: acc) t
                | x -> x |> invalid (snd x) "'%s' is not a symbol."
            | [] -> failwithf "Not enough arguments.%s" (pos |> formatPosition)
        | x -> x |> invalid (snd x) "'%s' is not a symbol."

    let bindingLet envs pos cont bindings body captureEnvs args =
        let boundVars =
            bindings
            |> List.map (fun (v, _) -> SSymbol v, pos)
            |> toSPair
            |> loopBindingLet pos [] args
            |> List.rev

        body
        |> Eval.eachEval
            (Context.mergeEnvs captureEnvs envs
             |> fun ctx -> Context.extendEnvs ctx boundVars)
            cont
            (SEmpty, pos)

    let sLet envs pos cont =
        function
        | bindings :: body ->
            match bindings with
            | SSymbol var, _ ->
                match body with
                | bBindings :: bBody ->
                    let bindings' = bBindings |> toList |> List.map eachBinding
                    let r = ref (SUnspecified, pos)
                    let envs' = [ var, r ] |> Context.extendEnvs envs
                    let proc = SProcedure(fun e p c a -> bindingLet e p c bindings' bBody envs' a), pos
                    r.Value <- proc

                    bindings'
                    |> List.map snd
                    |> Eval.evalArgs envs' cont (fun e c a -> Eval.apply e c a proc) []
                | _ -> body |> invalidParameter pos "'%s' invalid named let."
            | _ -> bindings |> toList |> List.map eachBinding |> bindLet envs pos cont body []
        | x -> x |> invalidParameter pos "'%s' invalid let parameter."

    let eachValuesBinding =
        function
        | SPair { car = formalsExpr
                  cdr = SPair { car = expr; cdr = SEmpty, _ }, _ },
          _ ->
            let vars =
                formalsExpr
                |> toList
                |> List.map (function
                    | SSymbol v, _ -> v
                    | x -> x |> invalid (snd x) "'%s' is not a symbol.")

            vars, expr
        | x -> x |> invalid (snd x) "'%s' invalid values binding."

    [<TailCall>]
    let rec bindLetStar envs pos cont body =
        function
        | [] -> body |> Eval.eachEval envs cont (SEmpty, pos)
        | (var, expr) :: xs ->
            expr
            |> Eval.eval envs (fun v -> xs |> bindLetStar ([ var, ref v ] |> Context.extendEnvs envs) pos cont body)

    let sLetStar envs pos cont =
        function
        | bindings :: body -> bindings |> toList |> List.map eachBinding |> bindLetStar envs pos cont body
        | x -> x |> invalidParameter pos "'%s' invalid let* parameter."

    [<TailCall>]
    let rec bindLetRecExpr envs pos cont body =
        function
        | [] -> body |> Eval.eachEval envs cont (SEmpty, pos)
        | (var, expr) :: xs ->
            expr
            |> Eval.eval envs (fun a ->
                Context.defineEnvVar envs var a
                xs |> bindLetRecExpr envs pos cont body)

    let sLetRec envs pos cont =
        let bindRef bindings =
            bindings
            |> List.map (function
                | var, _ -> var, ref (SEmpty, pos))
            |> Context.extendEnvs envs

        function
        | bindings :: body ->
            let bindings' = bindings |> toList |> List.map eachBinding
            bindings' |> bindLetRecExpr (bindings' |> bindRef) pos cont body
        | x -> x |> invalidParameter pos "'%s' invalid letrec parameter."

    [<TailCall>]
    let rec bindLetRecStarExpr envs pos cont body =
        function
        | [], _
        | _, [] -> body |> Eval.eachEval envs cont (SEmpty, pos)
        | (_, expr) :: xs, r: SExpression ref :: rs ->
            expr
            |> Eval.eval envs (fun v ->
                r.Value <- v
                (xs, rs) |> bindLetRecStarExpr envs pos cont body)

    let sLetRecStar envs pos cont =
        let eachRef (envs', refs) (var, _) =
            let r = ref (SEmpty, pos)
            [ var, r ] |> Context.extendEnvs envs', r :: refs

        let bindRef bindings =
            let envs', refs = bindings |> List.fold eachRef (envs, [])
            envs', List.rev refs

        function
        | bindings :: body ->
            let bindings' = bindings |> toList |> List.map eachBinding
            let envs', refs = bindRef bindings'
            (bindings', refs) |> bindLetRecStarExpr envs' pos cont body
        | x -> x |> invalidParameter pos "'%s' invalid letrec* parameter."

    [<TailCall>]
    let rec bindLetValues envs pos cont body acc =
        function
        | [] ->
            body
            |> Eval.eachEval (acc |> List.rev |> Context.extendEnvs envs) cont (SEmpty, pos)
        | (vars, expr) :: xs ->
            expr
            |> Eval.eval envs (fun v ->
                let vals =
                    match v with
                    | SValues vs, _ -> vs
                    | single -> [ single ]

                if List.length vars <> List.length vals then
                    failwithf "Values count mismatch in let-values.%s" (pos |> formatPosition)

                let bindings = List.zip vars vals |> List.map (fun (vr, vl) -> vr, ref vl)
                xs |> bindLetValues envs pos cont body (bindings @ acc))

    let sLetValues envs pos cont =
        function
        | bindings :: body ->
            bindings
            |> toList
            |> List.map eachValuesBinding
            |> bindLetValues envs pos cont body []
        | x -> x |> invalidParameter pos "'%s' invalid let-values parameter."

    [<TailCall>]
    let rec bindLetStarValues envs pos cont body =
        function
        | [] -> body |> Eval.eachEval envs cont (SEmpty, pos)
        | (vars, expr) :: xs ->
            expr
            |> Eval.eval envs (fun v ->
                let vals =
                    match v with
                    | SValues vs, _ -> vs
                    | single -> [ single ]

                if List.length vars <> List.length vals then
                    failwithf "Values count mismatch in let-star-values.%s" (pos |> formatPosition)

                let nextEnvs =
                    List.zip vars vals
                    |> List.map (fun (vr, vl) -> vr, ref vl)
                    |> Context.extendEnvs envs

                xs |> bindLetStarValues nextEnvs pos cont body)

    let sLetStarValues envs pos cont =
        function
        | bindings :: body ->
            bindings
            |> toList
            |> List.map eachValuesBinding
            |> bindLetStarValues envs pos cont body
        | x -> x |> invalidParameter pos "'%s' invalid let*-values parameter."

    let sBegin envs pos cont = Eval.eachEval envs cont (SEmpty, pos)

    [<TailCall>]
    let rec loopDo envs pos cont test exprs commands bindings loopEnvs =
        test
        |> Eval.eval loopEnvs (function
            | SBool false, _ ->
                commands
                |> Eval.eachEval
                    loopEnvs
                    (fun _ -> bindings |> evalDoStep envs pos cont test exprs commands bindings loopEnvs [])
                    (SEmpty, pos)
            | testResult ->
                match exprs with
                | [] -> (SEmpty, pos) |> cont
                | _ -> exprs |> Eval.eachEval loopEnvs cont testResult)

    and [<TailCall>] evalDoStep envs pos cont test exprs commands bindings loopEnvs acc =
        function
        | [] ->
            acc
            |> List.rev
            |> Context.extendEnvs envs
            |> loopDo envs pos cont test exprs commands bindings
        | (var, _, _, Some step) :: xs ->
            step
            |> Eval.eval loopEnvs (fun v ->
                xs
                |> evalDoStep envs pos cont test exprs commands bindings loopEnvs ((var, ref v) :: acc))
        | (var, varPos, _, None) :: xs ->
            let v = (Context.lookupEnvs loopEnvs varPos var).Value

            xs
            |> evalDoStep envs pos cont test exprs commands bindings loopEnvs ((var, ref v) :: acc)

    [<TailCall>]
    let rec initDoVariables envs pos cont test exprs commands bindings acc =
        function
        | [] ->
            acc
            |> List.rev
            |> Context.extendEnvs envs
            |> loopDo envs pos cont test exprs commands bindings
        | (var, _, init, _) :: xs ->
            init
            |> Eval.eval envs (fun v ->
                xs
                |> initDoVariables envs pos cont test exprs commands bindings ((var, ref v) :: acc))

    let sDo envs pos cont =
        let parseBinding =
            function
            | SPair { car = SSymbol var, varPos
                      cdr = SPair { car = init
                                    cdr = SPair { car = step; cdr = SEmpty, _ }, _ },
                            _ },
              _ -> var, varPos, init, Some step
            | SPair { car = SSymbol var, varPos
                      cdr = SPair { car = init; cdr = SEmpty, _ }, _ },
              _ -> var, varPos, init, None
            | x -> [ x ] |> invalidParameter pos "'%s' invalid do binding parameter."

        function
        | bindings :: testClause :: commands ->
            match testClause with
            | SPair { car = test; cdr = exprs }, _ ->
                let bindings' = bindings |> toList |> List.map parseBinding

                bindings'
                |> initDoVariables envs pos cont test (exprs |> toList) commands bindings' []
            | _ -> [ testClause ] |> invalidParameter pos "'%s' invalid do test clause."
        | x -> x |> invalidParameter pos "'%s' invalid do parameter."

    let sDelay envs pos cont =
        function
        | [ expr ] ->
            let thunk = closure envs (SEmpty, pos) [ expr ]
            (SPromise(ref (false, (SSyntax thunk, pos))), pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid delay parameter."

    let sDelayForce envs pos cont =
        function
        | [ expr ] ->
            let thunk = closure envs (SEmpty, pos) [ expr ]
            (SPromise(ref (false, (SSyntax thunk, pos))), pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid delay-force parameter."

    let sParameterize envs pos cont =
        function
        | bindings :: body ->
            bindings
            |> toList
            |> List.map eachParamBinding
            |> bindParameterize envs pos cont body []
        | x -> x |> invalidParameter pos "'%s' invalid parameterize parameter."

    let sGuard envs pos cont =
        function
        | (SPair { car = SSymbol var, _; cdr = clauses }, _) :: body ->
            let savedWinders = envs.currentWinders.Value

            try
                body |> Eval.eachEval envs cont (SEmpty, pos)
            with SchemeRaise obj ->
                let clausesList = clauses |> toList

                let hasElse =
                    match List.tryLast clausesList with
                    | Some(SPair { car = SSymbol "else", _; cdr = _ }, _) -> true
                    | _ -> false

                let finalClauses =
                    if hasElse then
                        clausesList
                    else
                        clausesList
                        @ [ toSPair [ SSymbol "else", pos; toSPair [ SSymbol "raise", pos; SQuote obj, pos ] ] ]

                doWind
                    envs
                    (fun _ ->
                        let envs' = [ var, ref obj ] |> Context.extendEnvs envs
                        finalClauses |> sCond envs' pos cont)
                    savedWinders
        | x -> x |> invalidParameter pos "'%s' invalid guard parameter."

    [<TailCall>]
    let rec loopReplaceQuasiquote acc =
        function
        | SPair p, _ -> p.cdr |> loopReplaceQuasiquote (p.car :: acc)
        | x -> List.rev acc, x

    [<TailCall>]
    let rec replaceQuasiquote envs pos cont n next =
        function
        | SEmpty, _ -> (SEmpty, pos) |> next
        | SPair _, _ as x ->
            let xs, tail = x |> loopReplaceQuasiquote []
            xs |> replaceQuasiquoteList envs pos cont n next tail
        | SVector xs, _ ->
            xs
            |> Array.toList
            |> replaceQuasiquoteList
                envs
                pos
                cont
                n
                (function
                | SEmpty, _ -> ([||] |> SVector, pos) |> next
                | SPair _, _ as y -> (y |> toList |> List.toArray |> SVector, pos) |> next
                | y -> y |> next)
                (SEmpty, pos)
        | x -> x |> replaceQuasiquoteDatum envs pos cont n next

    and [<TailCall>] replaceQuasiquoteList envs pos cont n next tail xs =
        let cons x b =
            match b with
            | SEmpty, _ -> [ x ] |> toSPair
            | SPair _, p -> SPair { car = x; cdr = b }, p
            | y -> SPair { car = x; cdr = y }, snd y

        let join a b =
            match a with
            | SEmpty, _ -> b
            | SPair _, _ ->
                try
                    List.foldBack (fun h acc -> SPair { car = h; cdr = acc }, snd h) (a |> toList) b
                with _ ->
                    failwithf "unquote-splicing must return a list.%s" (pos |> formatPosition)
            | x -> x |> invalid (snd x) "'%s' invalid unquote-splicing parameter."

        match xs with
        | [] -> tail |> replaceQuasiquoteDatum envs pos cont n next
        | (SUnquote x, _) :: rest
        | (SPair { car = SSymbol "unquote", _
                   cdr = SPair { car = x; cdr = SEmpty, _ }, _ },
           _) :: rest ->
            if n = 0 then
                x
                |> Eval.eval envs (fun a ->
                    rest |> replaceQuasiquoteList envs pos cont n (fun b -> cons a b |> next) tail)
            else
                x
                |> replaceQuasiquote envs pos cont (n - 1) (fun a ->
                    rest
                    |> replaceQuasiquoteList envs pos cont n (fun b -> cons (SUnquote a, pos) b |> next) tail)
        | (SUnquoteSplicing x, _) :: rest
        | (SPair { car = SSymbol "unquote-splicing", _
                   cdr = SPair { car = x; cdr = SEmpty, _ }, _ },
           _) :: rest ->
            if n = 0 then
                x
                |> Eval.eval envs (fun a ->
                    rest |> replaceQuasiquoteList envs pos cont n (fun b -> join a b |> next) tail)
            else
                x
                |> replaceQuasiquote envs pos cont (n - 1) (fun a ->
                    rest
                    |> replaceQuasiquoteList envs pos cont n (fun b -> cons (SUnquoteSplicing a, pos) b |> next) tail)
        | (SQuasiquote x, _) :: rest
        | (SPair { car = SSymbol "quasiquote", _
                   cdr = SPair { car = x; cdr = SEmpty, _ }, _ },
           _) :: rest ->
            x
            |> replaceQuasiquote envs pos cont (n + 1) (fun a ->
                rest
                |> replaceQuasiquoteList envs pos cont n (fun b -> cons (SQuasiquote a, pos) b |> next) tail)
        | (SQuote x, _) :: rest
        | (SPair { car = SSymbol "quote", _
                   cdr = SPair { car = x; cdr = SEmpty, _ }, _ },
           _) :: rest ->
            x
            |> replaceQuasiquote envs pos cont n (fun a ->
                rest
                |> replaceQuasiquoteList envs pos cont n (fun b -> cons (SQuote a, pos) b |> next) tail)
        | x :: rest ->
            x
            |> replaceQuasiquote envs pos cont n (fun a ->
                rest |> replaceQuasiquoteList envs pos cont n (fun b -> cons a b |> next) tail)

    and [<TailCall>] replaceQuasiquoteDatum envs pos cont n next =
        function
        | SUnquote x, _
        | SPair { car = SSymbol "unquote", _
                  cdr = SPair { car = x; cdr = SEmpty, _ }, _ },
          _ ->
            if n = 0 then
                x |> Eval.eval envs next
            else
                x
                |> replaceQuasiquote envs pos cont (n - 1) (fun x -> (SUnquote x, pos) |> next)
        | SUnquoteSplicing x, _
        | SPair { car = SSymbol "unquote-splicing", _
                  cdr = SPair { car = x; cdr = SEmpty, _ }, _ },
          _ ->
            if n = 0 then
                x |> Eval.eval envs next
            else
                x
                |> replaceQuasiquote envs pos cont (n - 1) (fun x -> (SUnquoteSplicing x, pos) |> next)
        | SQuasiquote x, _
        | SPair { car = SSymbol "quasiquote", _
                  cdr = SPair { car = x; cdr = SEmpty, _ }, _ },
          _ ->
            x
            |> replaceQuasiquote envs pos cont (n + 1) (fun x -> (SQuasiquote x, pos) |> next)
        | SQuote x, _
        | SPair { car = SSymbol "quote", _
                  cdr = SPair { car = x; cdr = SEmpty, _ }, _ },
          _ -> x |> replaceQuasiquote envs pos cont n (fun x -> (SQuote x, pos) |> next)
        | x -> x |> next

    let sQuasiquote envs pos cont =
        function
        | [ x ] -> x |> replaceQuasiquote envs pos cont 0 cont
        | x -> x |> invalidParameter pos "'%s' invalid quasiquote parameter."

    let sDefine envs pos cont =
        let define' var =
            Eval.eval envs (fun x ->
                Context.defineEnvVar envs var x
                (var |> SSymbol, pos) |> cont)

        function
        | [ SSymbol var, _; expr ] -> expr |> define' var
        | (SPair { car = SSymbol var, _; cdr = formals }, _) :: body ->
            sLambda envs pos cont (formals :: body) |> define' var
        | x -> x |> invalidParameter pos "'%s' invalid define parameter."

    [<TailCall>]
    let rec bindDefineValues envs cont formals =
        function
        | [] -> formals |> cont
        | (var, expr) :: xs ->
            expr
            |> Eval.eval envs (fun value ->
                Context.defineEnvVar envs var value
                xs |> bindDefineValues envs cont formals)

    let sDefineValues envs pos cont =
        function
        | [ formals; expr ] ->
            expr
            |> Eval.eval envs (fun result ->
                let vals =
                    match result with
                    | SValues vs, _ -> vs
                    | x -> [ x ]

                formals
                |> zipFormalsRef pos vals
                |> List.map (fun (v, r) -> v, r.Value)
                |> bindDefineValues envs cont formals)
        | x -> x |> invalidParameter pos "'%s' invalid define-values parameter."

    let sDefineRecordType envs pos cont =
        function
        | (SSymbol name, _) :: (SPair { car = SSymbol ctorName, _
                                        cdr = ctorFieldsExpr },
                                _) :: (SSymbol predName, _) :: restSpecs ->
            let defineVal var valExpr = Context.defineEnvVar envs var valExpr
            let typeId = Context.getNextRecordTypeId envs
            let ctorFields = ctorFieldsExpr |> toList

            let fieldSpecs =
                restSpecs
                |> List.map (function
                    | SPair { car = SSymbol fName, _
                              cdr = SPair { car = SSymbol aName, _; cdr = rest }, _ },
                      _ ->
                        let mName =
                            match rest with
                            | SPair { car = SSymbol m, _; cdr = SEmpty, _ }, _ -> Some m
                            | _ -> None

                        fName, aName, mName
                    | x -> x |> invalid (snd x) "'%s' invalid record field spec.")

            let fieldNames = fieldSpecs |> List.map (fun (n, _, _) -> n)
            let fieldCount = fieldNames.Length

            let predProc _ pos' cont' =
                function
                | [ SRecord(tid, _, _), _ ] -> (tid = typeId |> toSBool, pos') |> cont'
                | _ -> (SFalse, pos') |> cont'

            defineVal predName (SProcedure predProc, pos)

            let ctorProc _ pos' cont' (args: SExpression list) =
                if args.Length <> ctorFields.Length then
                    failwithf
                        "%s requires %d arguments, but got %d.%s"
                        ctorName
                        ctorFields.Length
                        args.Length
                        (pos' |> formatPosition)

                let recordFields = Array.init fieldCount (fun _ -> ref (SUnspecified, pos'))

                args
                |> List.zip ctorFields
                |> List.iter (fun (fExpr, v) ->
                    let fName =
                        match fExpr with
                        | SSymbol s, _ -> s
                        | x -> x |> invalid (snd x) "'%s' is not a symbol."

                    let idx = fieldNames |> List.findIndex ((=) fName)
                    recordFields.[idx].Value <- v)

                (SRecord(typeId, name, recordFields), pos') |> cont'

            defineVal ctorName (SProcedure ctorProc, pos)

            fieldSpecs
            |> List.iteri (fun idx (_, aName, mNameOpt) ->
                let accessorProc _ pos' cont' =
                    function
                    | [ SRecord(tid, _, fs), _ ] when tid = typeId -> fs.[idx].Value |> cont'
                    | [ x ] ->
                        failwithf
                            "Accessor %s expected %s, but got %s.%s"
                            aName
                            name
                            (x |> Print.print)
                            (x |> snd |> formatPosition)
                    | _ -> failwithf "Accessor %s requires 1 argument.%s" aName (pos' |> formatPosition)

                defineVal aName (SProcedure accessorProc, pos)

                mNameOpt
                |> Option.iter (fun mName ->
                    let modifierProc _ pos' cont' =
                        function
                        | [ SRecord(tid, _, fs), _; v ] when tid = typeId ->
                            fs.[idx].Value <- v
                            (SUnspecified, pos') |> cont'
                        | [ x; _ ] ->
                            failwithf
                                "Modifier %s expected %s, but got %s.%s"
                                mName
                                name
                                (x |> Print.print)
                                (x |> snd |> formatPosition)
                        | _ -> failwithf "Modifier %s requires 2 arguments.%s" mName (pos' |> formatPosition)

                    defineVal mName (SProcedure modifierProc, pos)))

            (name |> SSymbol, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid define-record-type parameter."
