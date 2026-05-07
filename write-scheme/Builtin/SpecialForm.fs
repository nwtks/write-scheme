namespace WriteScheme.Builtins

open WriteScheme
open Type

[<AutoOpen>]
module SpecialForm =
    let sQuote envs pos cont =
        function
        | [ datum ] -> Ok datum |> cont
        | x -> x |> invalidParameter pos "'%s' invalid quote parameter." |> cont

    [<TailCall>]
    let rec loopZipFormals pos args acc =
        function
        | SEmpty, _ ->
            if List.isEmpty args then
                Ok acc
            else
                EvalError("Too many arguments.", pos) |> Error
        | SSymbol var, _ -> Ok((var, args |> toSPair) :: acc)
        | SPair p, _ ->
            match args with
            | h :: t ->
                match p.car with
                | SSymbol var, _ -> p.cdr |> loopZipFormals pos t ((var, h) :: acc)
                | x -> x |> invalid (snd x) "'%s' is not a symbol."
            | [] -> EvalError("Not enough arguments.", pos) |> Error
        | x -> x |> invalid (snd x) "'%s' is not a symbol."

    let zipFormals pos args =
        function
        | SSymbol var, _ -> Ok [ var, args |> toSPair ]
        | x -> x |> loopZipFormals pos args [] |> Result.map List.rev

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

    let sLambda envs pos cont =
        function
        | formals :: body -> Ok(SProcedure(closure envs formals body), pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid lambda parameter." |> cont

    let sIf envs pos cont =
        let if' test consequent alternate =
            test
            |> Eval.eval envs (function
                | Ok(SBool false, _) -> alternate |> Eval.eval envs cont
                | Ok _ -> consequent |> Eval.eval envs cont
                | x -> x |> cont)

        function
        | [ test; consequent; alternate ] -> if' test consequent alternate
        | [ test; consequent ] -> if' test consequent (SEmpty, pos)
        | x -> x |> invalidParameter pos "'%s' invalid if parameter." |> cont

    let sSetBang envs pos cont =
        function
        | [ SSymbol variable, pos'; expression ] ->
            expression
            |> Eval.eval
                envs
                (Result.bind (fun exprVal ->
                    variable
                    |> Context.lookupEnvs envs pos'
                    |> Result.map (fun v ->
                        v.Value <- exprVal
                        exprVal)))
            |> cont
        | x -> x |> invalidParameter pos "'%s' invalid set! parameter." |> cont

    [<TailCall>]
    let rec sCond envs pos cont =
        function
        | [] -> Ok(SEmpty, pos) |> cont
        | clause :: clauses ->
            match clause with
            | SPair { car = SSymbol "else", _
                      cdr = expressions },
              _ ->
                match expressions |> toList with
                | Ok elist -> elist |> Eval.eachEval envs cont (Ok(SEmpty, pos))
                | Error e -> Error e |> cont
            | SPair { car = test
                      cdr = SPair { car = SSymbol "=>", _
                                    cdr = SPair { car = expression; cdr = SEmpty, _ }, _ },
                            _ },
              _ ->
                test
                |> Eval.eval envs (function
                    | Ok(SBool false, _) -> clauses |> sCond envs pos cont
                    | Ok a -> [ expression; SQuote a, pos ] |> toSPair |> Eval.eval envs cont
                    | x -> x |> cont)
            | SPair { car = test; cdr = expressions }, _ ->
                test
                |> Eval.eval envs (function
                    | Ok(SBool false, _) -> clauses |> sCond envs pos cont
                    | Ok a ->
                        match expressions |> toList with
                        | Ok elist -> elist |> Eval.eachEval envs cont (Ok a)
                        | Error e -> Error e |> cont
                    | x -> x |> cont)
            | x -> x |> invalid (snd x) "'%s' invalid cond clause." |> cont

    [<TailCall>]
    let rec testCase envs pos cont key =
        function
        | [] -> Ok(SEmpty, pos) |> cont
        | clause :: clauses ->
            match clause with
            | SPair { car = SSymbol "else", _
                      cdr = SPair { car = SSymbol "=>", _
                                    cdr = SPair { car = expression; cdr = SEmpty, _ }, _ },
                            _ },
              _ -> [ expression; SQuote key, pos ] |> toSPair |> Eval.eval envs cont
            | SPair { car = SSymbol "else", _
                      cdr = expressions },
              _ ->
                match expressions |> toList with
                | Ok elist -> elist |> Eval.eachEval envs cont (Ok(SEmpty, pos))
                | Error e -> Error e |> cont
            | SPair { car = datums
                      cdr = SPair { car = SSymbol "=>", _
                                    cdr = SPair { car = expression; cdr = SEmpty, _ }, _ },
                            _ },
              _ ->
                match datums |> toList with
                | Ok dlist ->
                    if dlist |> List.exists (fun datum -> eqv (key, datum)) then
                        [ expression; SQuote key, pos ] |> toSPair |> Eval.eval envs cont
                    else
                        clauses |> testCase envs pos cont key
                | Error e -> Error e |> cont
            | SPair { car = datums; cdr = expressions }, _ ->
                match datums |> toList with
                | Ok dlist ->
                    if dlist |> List.exists (fun datum -> eqv (key, datum)) then
                        match expressions |> toList with
                        | Ok elist -> elist |> Eval.eachEval envs cont (Ok(SEmpty, pos))
                        | Error e -> Error e |> cont
                    else
                        clauses |> testCase envs pos cont key
                | Error e -> Error e |> cont
            | x -> x |> invalid (snd x) "'%s' invalid case clause." |> cont

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
        | test :: expressions ->
            test
            |> Eval.eval envs (function
                | Ok(SBool false, _) -> Ok(SEmpty, pos) |> cont
                | Ok _ -> expressions |> Eval.eachEval envs cont (Ok(SEmpty, pos))
                | x -> x |> cont)
        | x -> x |> invalidParameter pos "'%s' invalid when parameter." |> cont

    let sUnless envs pos cont =
        function
        | test :: expressions ->
            test
            |> Eval.eval envs (function
                | Ok(SBool false, _) -> expressions |> Eval.eachEval envs cont (Ok(SEmpty, pos))
                | Ok _ -> Ok(SEmpty, pos) |> cont
                | x -> x |> cont)
        | x -> x |> invalidParameter pos "'%s' invalid unless parameter." |> cont

    [<TailCall>]
    let rec bindLet envs pos cont body acc =
        function
        | [] ->
            body
            |> Eval.eachEval (acc |> List.rev |> Context.extendEnvs envs) cont (Ok(SEmpty, pos))
        | (variable, init) :: bindings ->
            init
            |> Eval.eval envs (function
                | Ok i -> bindings |> bindLet envs pos cont body ((variable, ref i) :: acc)
                | x -> x |> cont)

    let sLet envs pos cont =
        function
        | (SSymbol variable, _) :: bindings :: body ->
            match bindings |> toList with
            | Ok blist ->
                match blist |> mapResult eachBinding with
                | Ok bindings' ->
                    let r = ref (SUnspecified, pos)
                    let envs' = [ variable, r ] |> Context.extendEnvs envs
                    let formals = bindings' |> List.map (fun (v, _) -> SSymbol v, pos) |> toSPair
                    let proc = SProcedure(closure envs' formals body), pos
                    r.Value <- proc

                    bindings'
                    |> List.map snd
                    |> Eval.evalArgs envs' cont (fun e c a -> proc |> Eval.apply e c a) []
                | Error e -> Error e |> cont
            | Error e -> Error e |> cont
        | bindings :: body ->
            match bindings |> toList with
            | Ok blist ->
                match blist |> mapResult eachBinding with
                | Ok bindings' -> bindings' |> bindLet envs pos cont body []
                | Error e -> Error e |> cont
            | Error e -> Error e |> cont
        | x -> x |> invalidParameter pos "'%s' invalid let parameter." |> cont

    [<TailCall>]
    let rec bindLetStar envs pos cont body =
        function
        | [] -> body |> Eval.eachEval envs cont (Ok(SEmpty, pos))
        | (variable, init) :: bindings ->
            init
            |> Eval.eval envs (function
                | Ok i ->
                    bindings
                    |> bindLetStar ([ variable, ref i ] |> Context.extendEnvs envs) pos cont body
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
    let rec bindLetRec envs pos cont body =
        function
        | [] -> body |> Eval.eachEval envs cont (Ok(SEmpty, pos))
        | (variable, init) :: bindings ->
            init
            |> Eval.eval envs (function
                | Ok i ->
                    Context.defineEnvVar envs variable i
                    bindings |> bindLetRec envs pos cont body
                | x -> x |> cont)

    let sLetRec envs pos cont =
        let bindRef bindings =
            bindings
            |> List.map (function
                | variable, _ -> variable, ref (SEmpty, pos))
            |> Context.extendEnvs envs

        function
        | bindings :: body ->
            match bindings |> toList with
            | Ok blist ->
                match blist |> mapResult eachBinding with
                | Ok bindings' -> bindings' |> bindLetRec (bindings' |> bindRef) pos cont body
                | Error e -> Error e |> cont
            | Error e -> Error e |> cont
        | x -> x |> invalidParameter pos "'%s' invalid letrec parameter." |> cont

    [<TailCall>]
    let rec bindLetRecStar envs pos cont body =
        function
        | [], _
        | _, [] -> body |> Eval.eachEval envs cont (Ok(SEmpty, pos))
        | (_, init) :: bindings, refs: SExpression ref :: rs ->
            init
            |> Eval.eval envs (function
                | Ok i ->
                    refs.Value <- i
                    (bindings, rs) |> bindLetRecStar envs pos cont body
                | x -> x |> cont)

    let sLetRecStar envs pos cont =
        let eachRef (envs', refs) (variable, _) =
            let r = ref (SEmpty, pos)
            [ variable, r ] |> Context.extendEnvs envs', r :: refs

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
                    (bindings', refs) |> bindLetRecStar envs' pos cont body
                | Error e -> Error e |> cont
            | Error e -> Error e |> cont
        | x -> x |> invalidParameter pos "'%s' invalid letrec* parameter." |> cont

    let eachValuesBinding =
        function
        | SPair { car = formals
                  cdr = SPair { car = init; cdr = SEmpty, _ }, _ },
          _ ->
            match formals |> toList with
            | Ok flist ->
                flist
                |> mapResult (function
                    | SSymbol v, _ -> Ok v
                    | x -> x |> invalid (snd x) "'%s' is not a symbol.")
                |> Result.map (fun vars -> vars, init)
            | Error e -> Error e
        | x -> x |> invalid (snd x) "'%s' invalid values binding."

    [<TailCall>]
    let rec bindLetValues envs pos cont body acc =
        function
        | [] ->
            body
            |> Eval.eachEval (acc |> List.rev |> Context.extendEnvs envs) cont (Ok(SEmpty, pos))
        | (vars, init) :: bindings ->
            init
            |> Eval.eval envs (function
                | Ok i ->
                    let values =
                        match i with
                        | SValues vs, _ -> vs
                        | value -> [ value ]

                    if List.length vars <> List.length values then
                        EvalError("Values count mismatch in let-values.", pos) |> Error |> cont
                    else
                        let bindings' =
                            List.zip vars values |> List.map (fun (variable, value) -> variable, ref value)

                        bindings |> bindLetValues envs pos cont body (bindings' @ acc)
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
        | (vars, init) :: xs ->
            init
            |> Eval.eval envs (function
                | Ok i ->
                    let values =
                        match i with
                        | SValues vs, _ -> vs
                        | value -> [ value ]

                    if List.length vars <> List.length values then
                        EvalError("Values count mismatch in let-star-values.", pos) |> Error |> cont
                    else
                        let nextEnvs =
                            List.zip vars values
                            |> List.map (fun (variable, value) -> variable, ref value)
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
    let rec loopDo envs pos cont bindings test expressions commands loopEnvs =
        test
        |> Eval.eval loopEnvs (function
            | Ok(SBool false, _) ->
                commands
                |> Eval.eachEval
                    loopEnvs
                    (function
                    | Ok _ ->
                        bindings
                        |> evalDoStep envs pos cont bindings test expressions commands loopEnvs []
                    | x -> x |> cont)
                    (Ok(SEmpty, pos))
            | Ok testResult ->
                match expressions with
                | [] -> Ok(SEmpty, pos) |> cont
                | _ -> expressions |> Eval.eachEval loopEnvs cont (Ok testResult)
            | x -> x |> cont)

    and [<TailCall>] evalDoStep envs pos cont bindings test expressions commands loopEnvs acc =
        function
        | [] ->
            acc
            |> List.rev
            |> Context.extendEnvs envs
            |> loopDo envs pos cont bindings test expressions commands
        | (variable, _, _, Some step) :: bindings' ->
            step
            |> Eval.eval loopEnvs (function
                | Ok s ->
                    bindings'
                    |> evalDoStep envs pos cont bindings test expressions commands loopEnvs ((variable, ref s) :: acc)
                | x -> x |> cont)
        | (variable, varPos, _, None) :: bindings' ->
            match variable |> Context.lookupEnvs loopEnvs varPos with
            | Ok v ->
                bindings'
                |> evalDoStep envs pos cont bindings test expressions commands loopEnvs ((variable, ref v.Value) :: acc)
            | Error e -> Error e |> cont

    [<TailCall>]
    let rec initDoVariables envs pos cont bindings test expressions commands acc =
        function
        | [] ->
            acc
            |> List.rev
            |> Context.extendEnvs envs
            |> loopDo envs pos cont bindings test expressions commands
        | (variable, _, init, _) :: bindings' ->
            init
            |> Eval.eval envs (function
                | Ok i ->
                    bindings'
                    |> initDoVariables envs pos cont bindings test expressions commands ((variable, ref i) :: acc)
                | x -> x |> cont)

    let parseDoBinding =
        function
        | SPair { car = SSymbol variable, varPos
                  cdr = SPair { car = init
                                cdr = SPair { car = step; cdr = SEmpty, _ }, _ },
                        _ },
          _ -> Ok(variable, varPos, init, Some step)
        | SPair { car = SSymbol variable, varPos
                  cdr = SPair { car = init; cdr = SEmpty, _ }, _ },
          _ -> Ok(variable, varPos, init, None)
        | x -> x |> invalid (snd x) "'%s' invalid do binding parameter."

    let sDo envs pos cont =
        function
        | bindings :: testClause :: commands ->
            match testClause with
            | SPair { car = test; cdr = expressions }, _ ->
                match bindings |> toList with
                | Ok blist ->
                    match blist |> mapResult parseDoBinding with
                    | Ok bindings' ->
                        match expressions |> toList with
                        | Ok elist -> bindings' |> initDoVariables envs pos cont bindings' test elist commands []
                        | Error e -> Error e |> cont
                    | Error e -> Error e |> cont
                | Error e -> Error e |> cont
            | _ -> [ testClause ] |> invalidParameter pos "'%s' invalid do test clause." |> cont
        | x -> x |> invalidParameter pos "'%s' invalid do parameter." |> cont

    let sDelay envs pos cont =
        function
        | [ expression ] ->
            let thunk = closure envs (SEmpty, pos) [ expression ]
            Ok(SPromise(ref (false, (SProcedure thunk, pos))), pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid delay parameter." |> cont

    let sDelayForce envs pos cont =
        function
        | [ expression ] ->
            let thunk = closure envs (SEmpty, pos) [ expression ]
            Ok(SPromise(ref (false, (SProcedure thunk, pos))), pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid delay-force parameter." |> cont

    let sParameterize envs pos cont =
        function
        | parameters :: body ->
            match parameters |> toList with
            | Ok plist ->
                match plist |> mapResult eachParamBinding with
                | Ok parameters' -> parameters' |> loopParameterize envs pos cont body []
                | Error e -> Error e |> cont
            | Error e -> Error e |> cont
        | x -> x |> invalidParameter pos "'%s' invalid parameterize parameter." |> cont

    let sGuard envs pos cont =
        function
        | (SPair { car = SSymbol variable, _
                   cdr = clauses },
           _) :: body ->
            let savedWinders = envs.winders.Value

            body
            |> Eval.eachEval
                envs
                (function
                | Ok res -> Ok res |> cont
                | Error(SchemeRaise(raised, _)) ->
                    match clauses |> toList with
                    | Ok clist ->
                        let hasElse =
                            match List.tryLast clist with
                            | Some(SPair { car = SSymbol "else", _; cdr = _ }, _) -> true
                            | _ -> false

                        let clauses' =
                            if hasElse then
                                clist
                            else
                                clist
                                @ [ toSPair
                                        [ SSymbol "else", pos; toSPair [ SSymbol "raise", pos; SQuote raised, pos ] ] ]

                        doWind
                            envs
                            (fun _ ->
                                let envs' = [ variable, ref raised ] |> Context.extendEnvs envs
                                clauses' |> sCond envs' pos cont)
                            savedWinders
                            (Ok(SUnspecified, pos))
                    | Error e -> Error e |> cont
                | x -> x |> cont)
                (Ok(SEmpty, pos))
        | x -> x |> invalidParameter pos "'%s' invalid guard parameter." |> cont

    [<TailCall>]
    let rec loopReplaceQuasiquote acc =
        function
        | SPair template, _ -> template.cdr |> loopReplaceQuasiquote (template.car :: acc)
        | x -> List.rev acc, x

    [<TailCall>]
    let rec replaceQuasiquote envs pos cont n next =
        function
        | SEmpty, _ -> Ok(SEmpty, pos) |> next
        | SPair _, _ as template ->
            let templates, tail = template |> loopReplaceQuasiquote []
            templates |> replaceQuasiquoteList envs pos cont n next tail
        | SVector templates, _ ->
            templates
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

    and [<TailCall>] replaceQuasiquoteList envs pos cont n next tail templates =
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
                        Ok(b |> List.foldBack (fun h acc -> SPair { car = h; cdr = acc }, snd h) alist)
                    with _ ->
                        EvalError("unquote-splicing must return a list.", pos) |> Error
                | Error e -> Error e
            | x -> x |> invalid (snd x) "'%s' invalid unquote-splicing parameter."

        match templates with
        | [] -> tail |> replaceQuasiquoteDatum envs pos cont n next
        | (SUnquote template, _) :: rest
        | (SPair { car = SSymbol "unquote", _
                   cdr = SPair { car = template; cdr = SEmpty, _ }, _ },
           _) :: rest ->
            if n = 0 then
                template
                |> Eval.eval envs (function
                    | Ok a ->
                        rest
                        |> replaceQuasiquoteList envs pos cont n (Result.map (fun b -> cons a b) >> next) tail
                    | x -> x |> next)
            else
                template
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
        | (SUnquoteSplicing template, _) :: rest
        | (SPair { car = SSymbol "unquote-splicing", _
                   cdr = SPair { car = template; cdr = SEmpty, _ }, _ },
           _) :: rest ->
            if n = 0 then
                template
                |> Eval.eval envs (function
                    | Ok a ->
                        rest
                        |> replaceQuasiquoteList envs pos cont n (Result.bind (fun b -> join a b) >> next) tail
                    | x -> x |> next)
            else
                template
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
        | (SQuasiquote template, _) :: rest
        | (SPair { car = SSymbol "quasiquote", _
                   cdr = SPair { car = template; cdr = SEmpty, _ }, _ },
           _) :: rest ->
            template
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
        | (SQuote template, _) :: rest
        | (SPair { car = SSymbol "quote", _
                   cdr = SPair { car = template; cdr = SEmpty, _ }, _ },
           _) :: rest ->
            template
            |> replaceQuasiquote envs pos cont n (function
                | Ok a ->
                    rest
                    |> replaceQuasiquoteList envs pos cont n (Result.map (fun b -> cons (SQuote a, pos) b) >> next) tail
                | x -> x |> next)
        | template :: rest ->
            template
            |> replaceQuasiquote envs pos cont n (function
                | Ok a ->
                    rest
                    |> replaceQuasiquoteList envs pos cont n (Result.map (fun b -> cons a b) >> next) tail
                | x -> x |> next)

    and [<TailCall>] replaceQuasiquoteDatum envs pos cont n next =
        function
        | SUnquote template, _
        | SPair { car = SSymbol "unquote", _
                  cdr = SPair { car = template; cdr = SEmpty, _ }, _ },
          _ ->
            if n = 0 then
                template |> Eval.eval envs next
            else
                template
                |> replaceQuasiquote envs pos cont (n - 1) (Result.map (fun x' -> SUnquote x', pos) >> next)
        | SUnquoteSplicing template, _
        | SPair { car = SSymbol "unquote-splicing", _
                  cdr = SPair { car = template; cdr = SEmpty, _ }, _ },
          _ ->
            if n = 0 then
                template |> Eval.eval envs next
            else
                template
                |> replaceQuasiquote envs pos cont (n - 1) (Result.map (fun x' -> SUnquoteSplicing x', pos) >> next)
        | SQuasiquote template, _
        | SPair { car = SSymbol "quasiquote", _
                  cdr = SPair { car = template; cdr = SEmpty, _ }, _ },
          _ ->
            template
            |> replaceQuasiquote envs pos cont (n + 1) (Result.map (fun x' -> SQuasiquote x', pos) >> next)
        | SQuote template, _
        | SPair { car = SSymbol "quote", _
                  cdr = SPair { car = template; cdr = SEmpty, _ }, _ },
          _ ->
            template
            |> replaceQuasiquote envs pos cont n (Result.map (fun x' -> SQuote x', pos) >> next)
        | x -> x |> Ok |> next

    let sQuasiquote envs pos cont =
        function
        | [ template ] -> template |> replaceQuasiquote envs pos cont 0 cont
        | x -> x |> invalidParameter pos "'%s' invalid quasiquote parameter." |> cont

    let sDefine envs pos cont =
        let define' variable =
            Result.map (fun value ->
                Context.defineEnvVar envs variable value
                SSymbol variable, pos)
            >> cont

        function
        | [ SSymbol variable, _; expression ] -> expression |> Eval.eval envs (define' variable)
        | (SPair { car = SSymbol variable, _
                   cdr = formals },
           _) :: body -> sLambda envs pos (define' variable) (formals :: body)
        | x -> x |> invalidParameter pos "'%s' invalid define parameter." |> cont

    [<TailCall>]
    let rec bindDefineValues envs cont formals =
        function
        | [] -> formals |> Ok |> cont
        | (variable, expression) :: bindings ->
            expression
            |> Eval.eval envs (function
                | Ok value ->
                    Context.defineEnvVar envs variable value
                    bindings |> bindDefineValues envs cont formals
                | x -> x |> cont)

    [<TailCall>]
    let rec loopZipFormalsRef pos values acc =
        function
        | SEmpty, _ ->
            if List.isEmpty values then
                Ok acc
            else
                EvalError("Too many arguments.", pos) |> Error
        | SSymbol variable, _ -> Ok((variable, ref (values |> toSPair |> SQuote, pos)) :: acc)
        | SPair formals, _ ->
            match values with
            | h :: t ->
                match formals.car with
                | SSymbol variable, _ -> formals.cdr |> loopZipFormalsRef pos t ((variable, ref h) :: acc)
                | x -> x |> invalid (snd x) "'%s' is not a symbol."
            | [] -> EvalError("Not enough arguments.", pos) |> Error
        | x -> x |> invalid (snd x) "'%s' is not a symbol."

    let zipFormalsRef pos values =
        function
        | SSymbol variable, _ -> Ok [ variable, ref (values |> toSPair |> SQuote, pos) ]
        | formals -> formals |> loopZipFormalsRef pos values [] |> Result.map List.rev

    let sDefineValues envs pos cont =
        function
        | [ formals; expression ] ->
            expression
            |> Eval.eval envs (function
                | Ok result ->
                    let values =
                        match result with
                        | SValues vs, _ -> vs
                        | value -> [ value ]

                    match formals |> zipFormalsRef pos values with
                    | Ok bindings ->
                        bindings
                        |> List.map (fun (v, r) -> v, r.Value)
                        |> bindDefineValues envs cont formals
                    | Error e -> Error e |> cont
                | x -> x |> cont)
        | x -> x |> invalidParameter pos "'%s' invalid define-values parameter." |> cont

    let parseRecordFields specs =
        specs
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

    let recordConstructorProc
        typeId
        name
        constructorName
        (constructorFields: SExpression list)
        (fieldNames: string list)
        envs
        pos
        cont
        (args: SExpression list)
        =
        if args.Length <> constructorFields.Length then
            EvalError(
                sprintf "%s requires %d arguments, but got %d." constructorName constructorFields.Length args.Length,
                pos
            )
            |> Error
            |> cont
        else
            let recordFields = Array.init fieldNames.Length (fun _ -> ref (SUnspecified, pos))
            let mutable error = None

            args
            |> List.zip constructorFields
            |> List.iter (fun (field, value) ->
                if error.IsNone then
                    match field with
                    | SSymbol fieldName, _ ->
                        let idx = fieldNames |> List.findIndex ((=) fieldName)
                        recordFields.[idx].Value <- value
                    | _ ->
                        error <-
                            EvalError("Constructor field mapping failed: not a symbol", pos)
                            |> Error
                            |> Some)

            error
            |> Option.defaultWith (fun () -> Ok(SRecord(typeId, name, recordFields), pos))
            |> cont

    let recordPredProc typeId envs pos cont =
        function
        | [ SRecord(tid, _, _), _ ] -> Ok(tid = typeId |> toSBool, pos) |> cont
        | _ -> Ok(SFalse, pos) |> cont

    let recordFieldAccessorProc typeId name idx accessorName envs pos cont =
        function
        | [ SRecord(tid, _, fs), _ ] when tid = typeId -> Ok fs.[idx].Value |> cont
        | [ x ] ->
            EvalError(sprintf "Accessor %s expected %s, but got %s." accessorName name (x |> Print.print), x |> snd)
            |> Error
            |> cont
        | _ ->
            EvalError(sprintf "Accessor %s requires 1 argument." accessorName, pos)
            |> Error
            |> cont

    let recordFieldModifierProc typeId name idx modifierName envs pos cont =
        function
        | [ SRecord(tid, _, fs), _; v ] when tid = typeId ->
            fs.[idx].Value <- v
            Ok(SUnspecified, pos) |> cont
        | [ x; _ ] ->
            EvalError(sprintf "Modifier %s expected %s, but got %s." modifierName name (x |> Print.print), x |> snd)
            |> Error
            |> cont
        | _ ->
            EvalError(sprintf "Modifier %s requires 2 arguments." modifierName, pos)
            |> Error
            |> cont

    let sDefineRecordType envs pos cont =
        function
        | (SSymbol name, _) :: (SPair { car = SSymbol constructorName, _
                                        cdr = constructorFields },
                                _) :: (SSymbol pred, _) :: fields ->
            let defineProc name proc =
                Context.defineEnvVar envs name (proc |> SProcedure, pos)

            let typeId = Context.getNextRecordTypeId envs

            match constructorFields |> toList with
            | Ok ctorFields ->
                match parseRecordFields fields with
                | Ok fieldSpecs ->
                    let fieldNames = fieldSpecs |> List.map (fun (n, _, _) -> n)
                    defineProc constructorName (recordConstructorProc typeId name constructorName ctorFields fieldNames)
                    defineProc pred (recordPredProc typeId)

                    fieldSpecs
                    |> List.iteri (fun idx (_, accessorName, modifierNameOpt) ->
                        defineProc accessorName (recordFieldAccessorProc typeId name idx accessorName)

                        modifierNameOpt
                        |> Option.iter (fun modifierName ->
                            defineProc modifierName (recordFieldModifierProc typeId name idx modifierName)))

                    Ok(SSymbol name, pos) |> cont
                | Error e -> Error e |> cont
            | Error e -> Error e |> cont
        | x -> x |> invalidParameter pos "'%s' invalid define-record-type parameter." |> cont
