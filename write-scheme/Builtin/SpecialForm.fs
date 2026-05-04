namespace WriteScheme.Builtins

open WriteScheme
open Type

[<AutoOpen>]
module SpecialForm =
    let sQuote envs pos cont =
        function
        | [ x ] -> Ok x |> cont
        | x -> x |> invalidParameter pos "'%s' invalid quote parameter." |> cont

    [<TailCall>]
    let rec loopZipFormals pos acc args =
        function
        | SEmpty, _ ->
            if List.isEmpty args then
                Ok acc
            else
                Error(EvalError("Too many arguments.", pos))
        | SSymbol var, _ -> Ok((var, args |> toSPair) :: acc)
        | SPair p, _ ->
            match args with
            | h :: t ->
                match p.car with
                | SSymbol var, _ -> p.cdr |> loopZipFormals pos ((var, h) :: acc) t
                | x -> x |> invalid (snd x) "'%s' is not a symbol."
            | [] -> Error(EvalError("Not enough arguments.", pos))
        | x -> x |> invalid (snd x) "'%s' is not a symbol."

    let zipFormals pos args =
        function
        | SSymbol var, _ -> Ok [ var, args |> toSPair ]
        | x -> x |> loopZipFormals pos [] args |> Result.map List.rev

    [<TailCall>]
    let rec bindArgs envs pos cont body acc =
        function
        | [] ->
            body
            |> Eval.eachEval (acc |> List.rev |> Context.extendEnvs envs) cont (Ok(SEmpty, pos))
        | (var, v) :: xs -> xs |> bindArgs envs pos cont body ((var, ref v) :: acc)

    let closure captureEnvs formals body envs pos cont args =
        match formals |> zipFormals pos args with
        | Ok bindings -> bindings |> bindArgs (Context.mergeEnvs envs captureEnvs) pos cont body []
        | Error e -> Error e |> cont

    [<TailCall>]
    let rec loopZipFormalsRef pos acc args =
        function
        | SEmpty, _ ->
            if List.isEmpty args then
                Ok acc
            else
                Error(EvalError("Too many arguments.", pos))
        | SSymbol var, _ -> Ok((var, ref (args |> toSPair |> SQuote, pos)) :: acc)
        | SPair pair, _ ->
            match args with
            | h :: t ->
                match pair.car with
                | SSymbol var, _ -> pair.cdr |> loopZipFormalsRef pos ((var, ref h) :: acc) t
                | x -> x |> invalid (snd x) "'%s' is not a symbol."
            | [] -> Error(EvalError("Not enough arguments.", pos))
        | x -> x |> invalid (snd x) "'%s' is not a symbol."

    let zipFormalsRef pos args =
        function
        | SSymbol var, _ -> Ok [ var, ref (args |> toSPair |> SQuote, pos) ]
        | x -> x |> loopZipFormalsRef pos [] args |> Result.map List.rev

    let sLambda envs pos cont =
        function
        | formals :: body -> Ok(SProcedure(closure envs formals body), pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid lambda parameter." |> cont

    let sIf envs pos cont =
        let if' test conseq alter =
            test
            |> Eval.eval envs (function
                | Ok(SBool false, _) -> alter |> Eval.eval envs cont
                | Ok _ -> conseq |> Eval.eval envs cont
                | x -> x |> cont)

        function
        | [ test; conseq; alter ] -> if' test conseq alter
        | [ test; conseq ] -> if' test conseq (SEmpty, pos)
        | x -> x |> invalidParameter pos "'%s' invalid if parameter." |> cont

    let sSet envs pos cont =
        function
        | [ SSymbol var, pos'; expr ] ->
            expr
            |> Eval.eval
                envs
                (Result.bind (fun x ->
                    Context.lookupEnvs envs pos' var
                    |> Result.map (fun v ->
                        v.Value <- x
                        x)))
            |> cont
        | x -> x |> invalidParameter pos "'%s' invalid set! parameter." |> cont

    [<TailCall>]
    let rec sCond envs pos cont =
        function
        | [] -> Ok(SEmpty, pos) |> cont
        | clause :: clauses ->
            match clause with
            | SPair { car = SSymbol "else", _; cdr = exprs }, _ ->
                match exprs |> toList with
                | Ok elist -> elist |> Eval.eachEval envs cont (Ok(SEmpty, pos))
                | Error e -> Error e |> cont
            | SPair { car = test
                      cdr = SPair { car = SSymbol "=>", _
                                    cdr = SPair { car = expr; cdr = SEmpty, _ }, _ },
                            _ },
              _ ->
                test
                |> Eval.eval envs (function
                    | Ok(SBool false, _) -> clauses |> sCond envs pos cont
                    | Ok a -> [ expr; SQuote a, pos ] |> toSPair |> Eval.eval envs cont
                    | x -> x |> cont)
            | SPair { car = test; cdr = exprs }, _ ->
                test
                |> Eval.eval envs (function
                    | Ok(SBool false, _) -> clauses |> sCond envs pos cont
                    | Ok a ->
                        match exprs |> toList with
                        | Ok elist -> elist |> Eval.eachEval envs cont (Ok a)
                        | Error e -> Error e |> cont
                    | x -> x |> cont)
            | x -> x |> invalid pos "'%s' invalid cond clause." |> cont

    [<TailCall>]
    let rec testCase envs pos cont key =
        function
        | [] -> Ok(SEmpty, pos) |> cont
        | clause :: clauses ->
            match clause with
            | SPair { car = SSymbol "else", _
                      cdr = SPair { car = SSymbol "=>", _
                                    cdr = SPair { car = expr; cdr = SEmpty, _ }, _ },
                            _ },
              _ -> [ expr; SQuote key, pos ] |> toSPair |> Eval.eval envs cont
            | SPair { car = SSymbol "else", _; cdr = exprs }, _ ->
                match exprs |> toList with
                | Ok elist -> elist |> Eval.eachEval envs cont (Ok(SEmpty, pos))
                | Error e -> Error e |> cont
            | SPair { car = datums
                      cdr = SPair { car = SSymbol "=>", _
                                    cdr = SPair { car = expr; cdr = SEmpty, _ }, _ },
                            _ },
              _ ->
                match datums |> toList with
                | Ok dlist ->
                    if dlist |> List.exists (fun d -> eqv (key, d)) then
                        [ expr; SQuote key, pos ] |> toSPair |> Eval.eval envs cont
                    else
                        clauses |> testCase envs pos cont key
                | Error e -> Error e |> cont
            | SPair { car = datums; cdr = exprs }, _ ->
                match datums |> toList with
                | Ok dlist ->
                    if dlist |> List.exists (fun d -> eqv (key, d)) then
                        match exprs |> toList with
                        | Ok elist -> elist |> Eval.eachEval envs cont (Ok(SEmpty, pos))
                        | Error e -> Error e |> cont
                    else
                        clauses |> testCase envs pos cont key
                | Error e -> Error e |> cont
            | _, p as x -> x |> invalid p "'%s' invalid case clause." |> cont

    let sCase envs pos cont =
        function
        | key :: clauses ->
            key
            |> Eval.eval envs (function
                | Ok k -> clauses |> testCase envs pos cont k
                | x -> x |> cont)
        | x -> x |> invalidParameter pos "'%s' invalid case parameter." |> cont

    [<TailCall>]
    let rec sAnd envs pos cont =
        function
        | [] -> Ok(STrue, pos) |> cont
        | [ test ] ->
            test
            |> Eval.eval envs (function
                | Ok(SBool false, _) -> Ok(SFalse, pos) |> cont
                | x -> x |> cont)
        | test :: tests ->
            test
            |> Eval.eval envs (function
                | Ok(SBool false, _) -> Ok(SFalse, pos) |> cont
                | Ok _ -> tests |> sAnd envs pos cont
                | x -> x |> cont)

    [<TailCall>]
    let rec sOr envs pos cont =
        function
        | [] -> Ok(SFalse, pos) |> cont
        | test :: tests ->
            test
            |> Eval.eval envs (function
                | Ok(SBool false, _) -> tests |> sOr envs pos cont
                | Ok x -> Ok x |> cont
                | x -> x |> cont)

    let sWhen envs pos cont =
        function
        | test :: exprs ->
            test
            |> Eval.eval envs (function
                | Ok(SBool false, _) -> Ok(SEmpty, pos) |> cont
                | Ok _ -> exprs |> Eval.eachEval envs cont (Ok(SEmpty, pos))
                | x -> x |> cont)
        | x -> x |> invalidParameter pos "'%s' invalid when parameter." |> cont

    let sUnless envs pos cont =
        function
        | test :: exprs ->
            test
            |> Eval.eval envs (function
                | Ok(SBool false, _) -> exprs |> Eval.eachEval envs cont (Ok(SEmpty, pos))
                | Ok _ -> Ok(SEmpty, pos) |> cont
                | x -> x |> cont)
        | x -> x |> invalidParameter pos "'%s' invalid unless parameter." |> cont

    [<TailCall>]
    let rec bindLet envs pos cont body acc =
        function
        | [] ->
            body
            |> Eval.eachEval (acc |> List.rev |> Context.extendEnvs envs) cont (Ok(SEmpty, pos))
        | (var, expr) :: xs ->
            expr
            |> Eval.eval envs (function
                | Ok a -> xs |> bindLet envs pos cont body ((var, ref a) :: acc)
                | x -> x |> cont)

    [<TailCall>]
    let rec loopBindingLet pos acc args =
        function
        | SEmpty, _ -> Ok acc
        | SSymbol v, _ -> Ok((v, ref (args |> toSPair)) :: acc)
        | SPair p, _ ->
            match args with
            | h :: t ->
                match p.car with
                | SSymbol v, _ -> p.cdr |> loopBindingLet pos ((v, ref h) :: acc) t
                | x -> x |> invalid (snd x) "'%s' is not a symbol."
            | [] -> Error(EvalError("Not enough arguments.", pos))
        | x -> x |> invalid (snd x) "'%s' is not a symbol."

    let bindingLet envs pos cont bindings body captureEnvs args =
        match
            bindings
            |> List.map (fun (v, _) -> SSymbol v, pos)
            |> toSPair
            |> loopBindingLet pos [] args
        with
        | Ok boundVars ->
            body
            |> Eval.eachEval
                (Context.mergeEnvs captureEnvs envs
                 |> fun ctx -> Context.extendEnvs ctx (List.rev boundVars))
                cont
                (Ok(SEmpty, pos))
        | Error e -> Error e |> cont

    let sLet envs pos cont =
        function
        | bindings :: body ->
            match bindings with
            | SSymbol var, _ ->
                match body with
                | bBindings :: bBody ->
                    match bBindings |> toList with
                    | Ok blist ->
                        match blist |> mapResult eachBinding with
                        | Ok bindings' ->
                            let r = ref (SUnspecified, pos)
                            let envs' = [ var, r ] |> Context.extendEnvs envs
                            let proc = SProcedure(fun e p c a -> bindingLet e p c bindings' bBody envs' a), pos
                            r.Value <- proc

                            bindings'
                            |> List.map snd
                            |> Eval.evalArgs envs' cont (fun e c a -> Eval.apply e c a proc) []
                        | Error e -> Error e |> cont
                    | Error e -> Error e |> cont
                | _ -> body |> invalidParameter pos "'%s' invalid named let." |> cont
            | _ ->
                match bindings |> toList with
                | Ok blist ->
                    match blist |> mapResult eachBinding with
                    | Ok bindings' -> bindings' |> bindLet envs pos cont body []
                    | Error e -> Error e |> cont
                | Error e -> Error e |> cont
        | x -> x |> invalidParameter pos "'%s' invalid let parameter." |> cont

    let eachValuesBinding =
        function
        | SPair { car = formalsExpr
                  cdr = SPair { car = expr; cdr = SEmpty, _ }, _ },
          _ ->
            match formalsExpr |> toList with
            | Ok flist ->
                flist
                |> mapResult (function
                    | SSymbol v, _ -> Ok v
                    | x -> x |> invalid (snd x) "'%s' is not a symbol.")
                |> Result.map (fun vars -> vars, expr)
            | Error e -> Error e
        | x -> x |> invalid (snd x) "'%s' invalid values binding."

    [<TailCall>]
    let rec bindLetStar envs pos cont body =
        function
        | [] -> body |> Eval.eachEval envs cont (Ok(SEmpty, pos))
        | (var, expr) :: xs ->
            expr
            |> Eval.eval envs (function
                | Ok v -> xs |> bindLetStar ([ var, ref v ] |> Context.extendEnvs envs) pos cont body
                | x -> x |> cont)

    let sLetStar envs pos cont =
        function
        | bindings :: body ->
            match bindings |> toList with
            | Ok blist ->
                match blist |> mapResult eachBinding with
                | Ok bindings' -> bindings' |> bindLetStar envs pos cont body
                | Error e -> Error e |> cont
            | Error e -> Error e |> cont
        | x -> x |> invalidParameter pos "'%s' invalid let* parameter." |> cont

    [<TailCall>]
    let rec bindLetRecExpr envs pos cont body =
        function
        | [] -> body |> Eval.eachEval envs cont (Ok(SEmpty, pos))
        | (var, expr) :: xs ->
            expr
            |> Eval.eval envs (function
                | Ok a ->
                    Context.defineEnvVar envs var a
                    xs |> bindLetRecExpr envs pos cont body
                | x -> x |> cont)

    let sLetRec envs pos cont =
        let bindRef bindings =
            bindings
            |> List.map (function
                | var, _ -> var, ref (SEmpty, pos))
            |> Context.extendEnvs envs

        function
        | bindings :: body ->
            match bindings |> toList with
            | Ok blist ->
                match blist |> mapResult eachBinding with
                | Ok bindings' -> bindings' |> bindLetRecExpr (bindings' |> bindRef) pos cont body
                | Error e -> Error e |> cont
            | Error e -> Error e |> cont
        | x -> x |> invalidParameter pos "'%s' invalid letrec parameter." |> cont

    [<TailCall>]
    let rec bindLetRecStarExpr envs pos cont body =
        function
        | [], _
        | _, [] -> body |> Eval.eachEval envs cont (Ok(SEmpty, pos))
        | (_, expr) :: xs, r: SExpression ref :: rs ->
            expr
            |> Eval.eval envs (function
                | Ok v ->
                    r.Value <- v
                    (xs, rs) |> bindLetRecStarExpr envs pos cont body
                | x -> x |> cont)

    let sLetRecStar envs pos cont =
        let eachRef (envs', refs) (var, _) =
            let r = ref (SEmpty, pos)
            [ var, r ] |> Context.extendEnvs envs', r :: refs

        let bindRef bindings =
            let envs', refs = bindings |> List.fold eachRef (envs, [])
            envs', List.rev refs

        function
        | bindings :: body ->
            match bindings |> toList with
            | Ok blist ->
                match blist |> mapResult eachBinding with
                | Ok bindings' ->
                    let envs', refs = bindRef bindings'
                    (bindings', refs) |> bindLetRecStarExpr envs' pos cont body
                | Error e -> Error e |> cont
            | Error e -> Error e |> cont
        | x -> x |> invalidParameter pos "'%s' invalid letrec* parameter." |> cont

    [<TailCall>]
    let rec bindLetValues envs pos cont body acc =
        function
        | [] ->
            body
            |> Eval.eachEval (acc |> List.rev |> Context.extendEnvs envs) cont (Ok(SEmpty, pos))
        | (vars, expr) :: xs ->
            expr
            |> Eval.eval envs (function
                | Ok v ->
                    let vals =
                        match v with
                        | SValues vs, _ -> vs
                        | single -> [ single ]

                    if List.length vars <> List.length vals then
                        Error(EvalError("Values count mismatch in let-values.", pos)) |> cont
                    else
                        let bindings = List.zip vars vals |> List.map (fun (vr, vl) -> vr, ref vl)
                        xs |> bindLetValues envs pos cont body (bindings @ acc)
                | x -> x |> cont)

    let sLetValues envs pos cont =
        function
        | bindings :: body ->
            match bindings |> toList with
            | Ok blist ->
                match blist |> mapResult eachValuesBinding with
                | Ok bindings' -> bindings' |> bindLetValues envs pos cont body []
                | Error e -> Error e |> cont
            | Error e -> Error e |> cont
        | x -> x |> invalidParameter pos "'%s' invalid let-values parameter." |> cont

    [<TailCall>]
    let rec bindLetStarValues envs pos cont body =
        function
        | [] -> body |> Eval.eachEval envs cont (Ok(SEmpty, pos))
        | (vars, expr) :: xs ->
            expr
            |> Eval.eval envs (function
                | Ok v ->
                    let vals =
                        match v with
                        | SValues vs, _ -> vs
                        | single -> [ single ]

                    if List.length vars <> List.length vals then
                        Error(EvalError("Values count mismatch in let-star-values.", pos)) |> cont
                    else
                        let nextEnvs =
                            List.zip vars vals
                            |> List.map (fun (vr, vl) -> vr, ref vl)
                            |> Context.extendEnvs envs

                        xs |> bindLetStarValues nextEnvs pos cont body
                | x -> x |> cont)

    let sLetStarValues envs pos cont =
        function
        | bindings :: body ->
            match bindings |> toList with
            | Ok blist ->
                match blist |> mapResult eachValuesBinding with
                | Ok bindings' -> bindings' |> bindLetStarValues envs pos cont body
                | Error e -> Error e |> cont
            | Error e -> Error e |> cont
        | x -> x |> invalidParameter pos "'%s' invalid let*-values parameter." |> cont

    let sBegin envs pos cont =
        Eval.eachEval envs cont (Ok(SEmpty, pos))

    [<TailCall>]
    let rec loopDo envs pos cont test exprs commands bindings loopEnvs =
        test
        |> Eval.eval loopEnvs (function
            | Ok(SBool false, _) ->
                commands
                |> Eval.eachEval
                    loopEnvs
                    (function
                    | Ok _ -> bindings |> evalDoStep envs pos cont test exprs commands bindings loopEnvs []
                    | x -> x |> cont)
                    (Ok(SEmpty, pos))
            | Ok testResult ->
                match exprs with
                | [] -> Ok(SEmpty, pos) |> cont
                | _ -> exprs |> Eval.eachEval loopEnvs cont (Ok testResult)
            | x -> x |> cont)

    and [<TailCall>] evalDoStep envs pos cont test exprs commands bindings loopEnvs acc =
        function
        | [] ->
            acc
            |> List.rev
            |> Context.extendEnvs envs
            |> loopDo envs pos cont test exprs commands bindings
        | (var, _, _, Some step) :: xs ->
            step
            |> Eval.eval loopEnvs (function
                | Ok v ->
                    xs
                    |> evalDoStep envs pos cont test exprs commands bindings loopEnvs ((var, ref v) :: acc)
                | x -> x |> cont)
        | (var, varPos, _, None) :: xs ->
            match Context.lookupEnvs loopEnvs varPos var with
            | Ok v ->
                xs
                |> evalDoStep envs pos cont test exprs commands bindings loopEnvs ((var, ref v.Value) :: acc)
            | Error e -> Error e |> cont

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
            |> Eval.eval envs (function
                | Ok v ->
                    xs
                    |> initDoVariables envs pos cont test exprs commands bindings ((var, ref v) :: acc)
                | x -> x |> cont)

    let sDo envs pos cont =
        let parseBinding =
            function
            | SPair { car = SSymbol var, varPos
                      cdr = SPair { car = init
                                    cdr = SPair { car = step; cdr = SEmpty, _ }, _ },
                            _ },
              _ -> Ok(var, varPos, init, Some step)
            | SPair { car = SSymbol var, varPos
                      cdr = SPair { car = init; cdr = SEmpty, _ }, _ },
              _ -> Ok(var, varPos, init, None)
            | x -> x |> invalid pos "'%s' invalid do binding parameter."

        function
        | bindings :: testClause :: commands ->
            match testClause with
            | SPair { car = test; cdr = exprs }, _ ->
                match bindings |> toList with
                | Ok blist ->
                    match blist |> mapResult parseBinding with
                    | Ok bindings' ->
                        match exprs |> toList with
                        | Ok elist -> initDoVariables envs pos cont test elist commands bindings' [] bindings'
                        | Error e -> Error e |> cont
                    | Error e -> Error e |> cont
                | Error e -> Error e |> cont
            | _ -> [ testClause ] |> invalidParameter pos "'%s' invalid do test clause." |> cont
        | x -> x |> invalidParameter pos "'%s' invalid do parameter." |> cont

    let sDelay envs pos cont =
        function
        | [ expr ] ->
            let thunk = closure envs (SEmpty, pos) [ expr ]
            Ok(SPromise(ref (false, (SProcedure thunk, pos))), pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid delay parameter." |> cont

    let sDelayForce envs pos cont =
        function
        | [ expr ] ->
            let thunk = closure envs (SEmpty, pos) [ expr ]
            Ok(SPromise(ref (false, (SProcedure thunk, pos))), pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid delay-force parameter." |> cont

    let eachParamBinding =
        function
        | SPair { car = param
                  cdr = SPair { car = expr; cdr = SEmpty, _ }, _ },
          _ -> Ok(param, expr)
        | x -> x |> invalid (snd x) "'%s' invalid parameterize binding."

    [<TailCall>]
    let rec loopParameterize envs pos cont body triples =
        function
        | [] ->
            let triples = List.rev triples

            let before _ pos cont _ =
                triples
                |> List.iter (fun (r: SExpression ref, nv: SExpression ref, ov: SExpression ref) ->
                    ov.Value <- r.Value
                    r.Value <- nv.Value)

                Ok(SUnspecified, pos) |> cont

            let after _ pos cont _ =
                triples
                |> List.iter (fun (r: SExpression ref, nv: SExpression ref, ov: SExpression ref) ->
                    nv.Value <- r.Value
                    r.Value <- ov.Value)

                Ok(SUnspecified, pos) |> cont

            let thunk envs pos cont _ =
                body |> Eval.eachEval envs cont (Ok(SEmpty, pos))

            sDynamicWind envs pos cont [ SProcedure before, pos; SProcedure thunk, pos; SProcedure after, pos ]
        | (pExpr, vExpr) :: rest ->
            pExpr
            |> Eval.eval envs (function
                | Ok(SParameter(r, convOpt), _) ->
                    vExpr
                    |> Eval.eval envs (function
                        | Ok newVal ->
                            match convOpt with
                            | Some conv ->
                                conv
                                |> Eval.apply
                                    envs
                                    (function
                                    | Ok converted ->
                                        let oldVal = r.Value

                                        rest
                                        |> loopParameterize
                                            envs
                                            pos
                                            cont
                                            body
                                            ((r, ref converted, ref oldVal) :: triples)
                                    | x -> x |> cont)
                                    [ newVal ]
                            | None ->
                                let oldVal = r.Value

                                rest
                                |> loopParameterize envs pos cont body ((r, ref newVal, ref oldVal) :: triples)
                        | x -> x |> cont)
                | Ok x ->
                    Error(EvalError(sprintf "'%s' is not a parameter." (x |> Print.print), snd pExpr))
                    |> cont
                | x -> x |> cont)

    let sParameterize envs pos cont =
        function
        | bindings :: body ->
            match bindings |> toList with
            | Ok blist ->
                match blist |> mapResult eachParamBinding with
                | Ok bindings' -> bindings' |> loopParameterize envs pos cont body []
                | Error e -> Error e |> cont
            | Error e -> Error e |> cont
        | x -> x |> invalidParameter pos "'%s' invalid parameterize parameter." |> cont

    let sGuard envs pos cont =
        function
        | (SPair { car = SSymbol var, _; cdr = clauses }, _) :: body ->
            let savedWinders = envs.currentWinders.Value

            body
            |> Eval.eachEval
                envs
                (function
                | Ok res -> Ok res |> cont
                | Error(SchemeRaise(obj, _)) ->
                    match clauses |> toList with
                    | Ok clist ->
                        let hasElse =
                            match List.tryLast clist with
                            | Some(SPair { car = SSymbol "else", _; cdr = _ }, _) -> true
                            | _ -> false

                        let finalClauses =
                            if hasElse then
                                clist
                            else
                                clist
                                @ [ toSPair [ SSymbol "else", pos; toSPair [ SSymbol "raise", pos; SQuote obj, pos ] ] ]

                        doWind
                            envs
                            (fun _ ->
                                let envs' = [ var, ref obj ] |> Context.extendEnvs envs
                                finalClauses |> sCond envs' pos cont)
                            savedWinders
                            (Ok(SUnspecified, pos))
                    | Error e -> Error e |> cont
                | x -> x |> cont)
                (Ok(SEmpty, pos))
        | x -> x |> invalidParameter pos "'%s' invalid guard parameter." |> cont

    [<TailCall>]
    let rec loopReplaceQuasiquote acc =
        function
        | SPair p, _ -> p.cdr |> loopReplaceQuasiquote (p.car :: acc)
        | x -> List.rev acc, x

    [<TailCall>]
    let rec replaceQuasiquote envs pos cont n next =
        function
        | SEmpty, _ -> Ok(SEmpty, pos) |> next
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
                | Ok(SEmpty, _) -> Ok([||] |> SVector, pos) |> next
                | Ok(SPair _, _ as y) ->
                    match y |> toList with
                    | Ok ylist -> Ok(ylist |> List.toArray |> SVector, pos) |> next
                    | Error e -> Error e |> next
                | x -> x |> next)
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
            | SEmpty, _ -> Ok b
            | SPair _, _ ->
                match a |> toList with
                | Ok alist ->
                    try
                        Ok(List.foldBack (fun h acc -> SPair { car = h; cdr = acc }, snd h) alist b)
                    with _ ->
                        Error(EvalError("unquote-splicing must return a list.", pos))
                | Error e -> Error e
            | x -> x |> invalid (snd x) "'%s' invalid unquote-splicing parameter."

        match xs with
        | [] -> tail |> replaceQuasiquoteDatum envs pos cont n next
        | (SUnquote x, _) :: rest
        | (SPair { car = SSymbol "unquote", _
                   cdr = SPair { car = x; cdr = SEmpty, _ }, _ },
           _) :: rest ->
            if n = 0 then
                x
                |> Eval.eval envs (function
                    | Ok a ->
                        rest
                        |> replaceQuasiquoteList envs pos cont n (Result.map (fun b -> cons a b) >> next) tail
                    | x -> x |> next)
            else
                x
                |> replaceQuasiquote envs pos cont (n - 1) (function
                    | Ok a ->
                        rest
                        |> replaceQuasiquoteList
                            envs
                            pos
                            cont
                            n
                            (Result.map (fun b -> cons (SUnquote a, pos) b) >> next)
                            tail
                    | x -> x |> next)
        | (SUnquoteSplicing x, _) :: rest
        | (SPair { car = SSymbol "unquote-splicing", _
                   cdr = SPair { car = x; cdr = SEmpty, _ }, _ },
           _) :: rest ->
            if n = 0 then
                x
                |> Eval.eval envs (function
                    | Ok a ->
                        rest
                        |> replaceQuasiquoteList envs pos cont n (Result.bind (fun b -> join a b) >> next) tail
                    | x -> x |> next)
            else
                x
                |> replaceQuasiquote envs pos cont (n - 1) (function
                    | Ok a ->
                        rest
                        |> replaceQuasiquoteList
                            envs
                            pos
                            cont
                            n
                            (Result.map (fun b -> cons (SUnquoteSplicing a, pos) b) >> next)
                            tail
                    | x -> x |> next)
        | (SQuasiquote x, _) :: rest
        | (SPair { car = SSymbol "quasiquote", _
                   cdr = SPair { car = x; cdr = SEmpty, _ }, _ },
           _) :: rest ->
            x
            |> replaceQuasiquote envs pos cont (n + 1) (function
                | Ok a ->
                    rest
                    |> replaceQuasiquoteList
                        envs
                        pos
                        cont
                        n
                        (Result.map (fun b -> cons (SQuasiquote a, pos) b) >> next)
                        tail
                | x -> x |> next)
        | (SQuote x, _) :: rest
        | (SPair { car = SSymbol "quote", _
                   cdr = SPair { car = x; cdr = SEmpty, _ }, _ },
           _) :: rest ->
            x
            |> replaceQuasiquote envs pos cont n (function
                | Ok a ->
                    rest
                    |> replaceQuasiquoteList envs pos cont n (Result.map (fun b -> cons (SQuote a, pos) b) >> next) tail
                | x -> x |> next)
        | x :: rest ->
            x
            |> replaceQuasiquote envs pos cont n (function
                | Ok a ->
                    rest
                    |> replaceQuasiquoteList envs pos cont n (Result.map (fun b -> cons a b) >> next) tail
                | x -> x |> next)

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
                |> replaceQuasiquote envs pos cont (n - 1) (Result.map (fun x' -> SUnquote x', pos) >> next)
        | SUnquoteSplicing x, _
        | SPair { car = SSymbol "unquote-splicing", _
                  cdr = SPair { car = x; cdr = SEmpty, _ }, _ },
          _ ->
            if n = 0 then
                x |> Eval.eval envs next
            else
                x
                |> replaceQuasiquote envs pos cont (n - 1) (Result.map (fun x' -> SUnquoteSplicing x', pos) >> next)
        | SQuasiquote x, _
        | SPair { car = SSymbol "quasiquote", _
                  cdr = SPair { car = x; cdr = SEmpty, _ }, _ },
          _ ->
            x
            |> replaceQuasiquote envs pos cont (n + 1) (Result.map (fun x' -> SQuasiquote x', pos) >> next)
        | SQuote x, _
        | SPair { car = SSymbol "quote", _
                  cdr = SPair { car = x; cdr = SEmpty, _ }, _ },
          _ ->
            x
            |> replaceQuasiquote envs pos cont n (Result.map (fun x' -> SQuote x', pos) >> next)
        | x -> x |> Ok |> next

    let sQuasiquote envs pos cont =
        function
        | [ x ] -> x |> replaceQuasiquote envs pos cont 0 cont
        | x -> x |> invalidParameter pos "'%s' invalid quasiquote parameter." |> cont

    let sDefine envs pos cont =
        let define' var =
            Result.map (fun x ->
                Context.defineEnvVar envs var x
                SSymbol var, pos)
            >> cont

        function
        | [ SSymbol var, _; expr ] -> expr |> Eval.eval envs (define' var)
        | (SPair { car = SSymbol var, _; cdr = formals }, _) :: body -> sLambda envs pos (define' var) (formals :: body)
        | x -> x |> invalidParameter pos "'%s' invalid define parameter." |> cont

    [<TailCall>]
    let rec bindDefineValues envs cont formals =
        function
        | [] -> formals |> Ok |> cont
        | (var, expr) :: xs ->
            expr
            |> Eval.eval envs (function
                | Ok value ->
                    Context.defineEnvVar envs var value
                    xs |> bindDefineValues envs cont formals
                | x -> x |> cont)

    let sDefineValues envs pos cont =
        function
        | [ formals; expr ] ->
            expr
            |> Eval.eval envs (function
                | Ok result ->
                    let vals =
                        match result with
                        | SValues vs, _ -> vs
                        | x -> [ x ]

                    match formals |> zipFormalsRef pos vals with
                    | Ok bindings ->
                        bindings
                        |> List.map (fun (v, r) -> v, r.Value)
                        |> bindDefineValues envs cont formals
                    | Error e -> Error e |> cont
                | x -> x |> cont)
        | x -> x |> invalidParameter pos "'%s' invalid define-values parameter." |> cont

    let sDefineRecordType envs pos cont =
        function
        | (SSymbol name, _) :: (SPair { car = SSymbol ctorName, _
                                        cdr = ctorFieldsExpr },
                                _) :: (SSymbol predName, _) :: restSpecs ->
            let defineVal var valExpr = Context.defineEnvVar envs var valExpr
            let typeId = Context.getNextRecordTypeId envs

            match ctorFieldsExpr |> toList with
            | Ok ctorFields ->
                match
                    restSpecs
                    |> mapResult (function
                        | SPair { car = SSymbol fName, _
                                  cdr = SPair { car = SSymbol aName, _; cdr = rest }, _ },
                          _ ->
                            let mName =
                                match rest with
                                | SPair { car = SSymbol m, _; cdr = SEmpty, _ }, _ -> Some m
                                | _ -> None

                            Ok(fName, aName, mName)
                        | x -> x |> invalid (snd x) "'%s' invalid record field spec.")
                with
                | Ok fieldSpecs ->
                    let fieldNames = fieldSpecs |> List.map (fun (n, _, _) -> n)
                    let fieldCount = fieldNames.Length

                    let predProc _ pos' cont' =
                        function
                        | [ SRecord(tid, _, _), _ ] -> Ok(tid = typeId |> toSBool, pos') |> cont'
                        | _ -> Ok(SFalse, pos') |> cont'

                    defineVal predName (SProcedure predProc, pos)

                    let ctorProc _ pos' cont' (args: SExpression list) =
                        if args.Length <> ctorFields.Length then
                            Error(
                                EvalError(
                                    sprintf
                                        "%s requires %d arguments, but got %d."
                                        ctorName
                                        ctorFields.Length
                                        args.Length,
                                    pos'
                                )
                            )
                            |> cont'
                        else
                            let recordFields = Array.init fieldCount (fun _ -> ref (SUnspecified, pos'))

                            let mutable error = None

                            args
                            |> List.zip ctorFields
                            |> List.iter (fun (fExpr, v) ->
                                if error.IsNone then
                                    match fExpr with
                                    | SSymbol s, _ ->
                                        let idx = fieldNames |> List.findIndex ((=) s)
                                        recordFields.[idx].Value <- v
                                    | _ ->
                                        error <-
                                            Some(
                                                Error(
                                                    EvalError("Constructor field mapping failed: not a symbol", pos')
                                                )
                                            ))

                            error
                            |> Option.defaultWith (fun () -> Ok(SRecord(typeId, name, recordFields), pos'))
                            |> cont'

                    defineVal ctorName (SProcedure ctorProc, pos)

                    fieldSpecs
                    |> List.iteri (fun idx (_, aName, mNameOpt) ->
                        let accessorProc _ pos' cont' =
                            function
                            | [ SRecord(tid, _, fs), _ ] when tid = typeId -> Ok fs.[idx].Value |> cont'
                            | [ x ] ->
                                Error(
                                    EvalError(
                                        sprintf "Accessor %s expected %s, but got %s." aName name (x |> Print.print),
                                        x |> snd
                                    )
                                )
                                |> cont'
                            | _ ->
                                Error(EvalError(sprintf "Accessor %s requires 1 argument." aName, pos'))
                                |> cont'

                        defineVal aName (SProcedure accessorProc, pos)

                        mNameOpt
                        |> Option.iter (fun mName ->
                            let modifierProc _ pos' cont' =
                                function
                                | [ SRecord(tid, _, fs), _; v ] when tid = typeId ->
                                    fs.[idx].Value <- v
                                    Ok(SUnspecified, pos') |> cont'
                                | [ x; _ ] ->
                                    Error(
                                        EvalError(
                                            sprintf
                                                "Modifier %s expected %s, but got %s."
                                                mName
                                                name
                                                (x |> Print.print),
                                            x |> snd
                                        )
                                    )
                                    |> cont'
                                | _ ->
                                    Error(EvalError(sprintf "Modifier %s requires 2 arguments." mName, pos'))
                                    |> cont'

                            defineVal mName (SProcedure modifierProc, pos)))

                    Ok(SSymbol name, pos) |> cont
                | Error e -> Error e |> cont
            | Error e -> Error e |> cont
        | x -> x |> invalidParameter pos "'%s' invalid define-record-type parameter." |> cont
