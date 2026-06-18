namespace WriteScheme.Builtins

open WriteScheme
open Type

[<AutoOpen>]
module SpecialForm =
    let sQuote context pos cont =
        function
        | [ datum ] -> Ok datum |> cont
        | x -> x |> invalidParameter pos "'%s' invalid quote parameter." |> cont

    [<TailCall>]
    let rec loopZipFormals pos args acc =
        function
        | SEmpty, _ ->
            if args |> List.isEmpty then
                Ok acc
            else
                EvalError("Too many arguments.", pos) |> Error
        | SSymbol variable, _ -> (variable, args |> toSPair) :: acc |> Ok
        | SPair pair, _ ->
            match args with
            | h :: t ->
                match pair.car with
                | SSymbol variable, _ -> pair.cdr |> loopZipFormals pos t ((variable, h) :: acc)
                | x -> x |> invalid (snd x) "'%s' is not a symbol."
            | [] -> EvalError("Not enough arguments.", pos) |> Error
        | x -> x |> invalid (snd x) "'%s' is not a symbol."

    let zipFormals pos args =
        function
        | SSymbol variable, _ -> [ variable, args |> toSPair ] |> Ok
        | x -> x |> loopZipFormals pos args [] |> Result.map List.rev

    [<TailCall>]
    let rec bindArgs context pos cont body acc =
        function
        | [] ->
            body
            |> Eval.evalBody (acc |> List.rev |> Context.extendEnvironments context) cont (Ok(SEmpty, pos))
        | (variable, value) :: rest -> rest |> bindArgs context pos cont body ((variable, ref value) :: acc)

    let closure captureContext formals body context pos cont args =
        match formals |> zipFormals pos args with
        | Ok bindings ->
            bindings
            |> bindArgs (Context.mergeEnvironments context captureContext) pos cont body []
        | Error e -> Error e |> cont

    let sLambda context pos cont =
        function
        | formals :: body -> Ok(SProcedure(closure context formals body), pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid lambda parameter." |> cont

    let sIf context pos cont =
        let if' test consequent alternate =
            test
            |> Eval.eval context (function
                | Ok(SBool false, _) -> alternate |> Eval.eval context cont
                | Ok _ -> consequent |> Eval.eval context cont
                | x -> x |> cont)

        function
        | [ test; consequent; alternate ] -> if' test consequent alternate
        | [ test; consequent ] -> if' test consequent (SUnspecified, pos)
        | x -> x |> invalidParameter pos "'%s' invalid if parameter." |> cont

    let sSetBang context pos cont =
        function
        | [ SSymbol variable, pos'; expression ] ->
            expression
            |> Eval.eval
                context
                (Result.bind (fun exprVal ->
                    variable
                    |> Context.lookupEnvironments context pos'
                    |> Result.map (fun v ->
                        v.Value <- exprVal
                        exprVal)))
            |> cont
        | x -> x |> invalidParameter pos "'%s' invalid set! parameter." |> cont

    [<TailCall>]
    let rec sIncludeFiles foldCase context pos cont acc =
        function
        | [] ->
            match acc |> List.rev with
            | [] -> Ok(SUnspecified, pos) |> cont
            | expressions -> expressions |> Eval.eachEval context cont (Ok(SUnspecified, pos))
        | (SString f, p) :: rest ->
            match tryReadAll foldCase f p with
            | Ok expressions ->
                match expressions |> mapResult DatumLabel.resolveLabels with
                | Ok resolvedExpressions ->
                    rest
                    |> sIncludeFiles foldCase context pos cont (List.rev resolvedExpressions @ acc)
                | Error e -> Error e |> cont
            | Error e -> Error e |> cont
        | x :: _ -> [ x ] |> invalidParameter pos "'%s' invalid include parameter." |> cont

    let sInclude context pos cont files =
        files |> sIncludeFiles false context pos cont []

    let sIncludeCi context pos cont files =
        files |> sIncludeFiles true context pos cont []

    let isElseClause datums =
        match fst datums with
        | SSymbol "else" -> true
        | _ -> false

    let normalizeCaseClause =
        function
        | SPair { car = datums
                  cdr = SPair { car = SSymbol "=>", _
                                cdr = SPair { car = expr; cdr = SEmpty, _ }, _ },
                        _ },
          _ -> Some(datums, Choice1Of2 expr)
        | SPair { car = datums; cdr = exprs }, _ -> Some(datums, Choice2Of2 exprs)
        | _ -> None

    [<TailCall>]
    let rec evalCondTest context pos cont clauses next test =
        test
        |> Eval.eval context (function
            | Ok(SBool false, _) -> clauses |> sCond context pos cont
            | Ok a -> next a
            | x -> x |> cont)

    and [<TailCall>] sCond context pos cont =
        function
        | [] -> Ok(SUnspecified, pos) |> cont
        | clause :: clauses ->
            match clause with
            | SPair { car = SSymbol "else", _
                      cdr = expressions },
              _ ->
                match expressions |> toList with
                | Ok elist -> elist |> Eval.eachEval context cont (Ok(SUnspecified, pos))
                | Error e -> Error e |> cont
            | SPair { car = test
                      cdr = SPair { car = SSymbol "=>", _
                                    cdr = SPair { car = expression; cdr = SEmpty, _ }, _ },
                            _ },
              _ ->
                test
                |> evalCondTest context pos cont clauses (fun a ->
                    [ expression; SQuote a, pos ] |> toSPair |> Eval.eval context cont)
            | SPair { car = test; cdr = expressions }, _ ->
                test
                |> evalCondTest context pos cont clauses (fun a ->
                    match expressions |> toList with
                    | Ok elist -> elist |> Eval.eachEval context cont (Ok a)
                    | Error e -> Error e |> cont)
            | x -> x |> invalid (snd x) "'%s' invalid cond clause." |> cont

    [<TailCall>]
    let rec evalCaseDatums context pos cont clauses key next =
        function
        | Ok dlist ->
            if dlist |> List.exists (fun datum -> eqv (key, datum)) then
                next ()
            else
                clauses |> testCase context pos cont key
        | Error e -> Error e |> cont

    and [<TailCall>] testCase context pos cont key =
        function
        | [] -> Ok(SUnspecified, pos) |> cont
        | clause :: clauses ->
            match clause |> normalizeCaseClause with
            | Some(datums, Choice1Of2 expr) ->
                if isElseClause datums then
                    [ expr; SQuote key, pos ] |> toSPair |> Eval.eval context cont
                else
                    datums
                    |> toList
                    |> evalCaseDatums context pos cont clauses key (fun () ->
                        [ expr; SQuote key, pos ] |> toSPair |> Eval.eval context cont)
            | Some(datums, Choice2Of2 exprs) ->
                if isElseClause datums then
                    match exprs |> toList with
                    | Ok elist -> elist |> Eval.eachEval context cont (Ok(SUnspecified, pos))
                    | Error e -> Error e |> cont
                else
                    datums
                    |> toList
                    |> evalCaseDatums context pos cont clauses key (fun () ->
                        match exprs |> toList with
                        | Ok elist -> elist |> Eval.eachEval context cont (Ok(SUnspecified, pos))
                        | Error e -> Error e |> cont)
            | None -> clause |> invalid (snd clause) "'%s' invalid case clause." |> cont

    let sCase context pos cont =
        function
        | key :: clauses ->
            key
            |> Eval.eval context (function
                | Ok k -> clauses |> testCase context pos cont k
                | x -> x |> cont)
        | x -> x |> invalidParameter pos "'%s' invalid case parameter." |> cont

    [<TailCall>]
    let rec sAnd context pos cont =
        function
        | [] -> Ok(STrue, pos) |> cont
        | [ test ] ->
            test
            |> Eval.eval context (function
                | Ok(SBool false, _) -> Ok(SFalse, pos) |> cont
                | x -> x |> cont)
        | test :: tests ->
            test
            |> Eval.eval context (function
                | Ok(SBool false, _) -> Ok(SFalse, pos) |> cont
                | Ok _ -> tests |> sAnd context pos cont
                | x -> x |> cont)

    [<TailCall>]
    let rec sOr context pos cont =
        function
        | [] -> Ok(SFalse, pos) |> cont
        | test :: tests ->
            test
            |> Eval.eval context (function
                | Ok(SBool false, _) -> tests |> sOr context pos cont
                | Ok x -> Ok x |> cont
                | x -> x |> cont)

    let sWhen context pos cont =
        function
        | test :: expressions ->
            test
            |> Eval.eval context (function
                | Ok(SBool false, _) -> Ok(SUnspecified, pos) |> cont
                | Ok _ -> expressions |> Eval.eachEval context cont (Ok(SUnspecified, pos))
                | x -> x |> cont)
        | x -> x |> invalidParameter pos "'%s' invalid when parameter." |> cont

    let sUnless context pos cont =
        function
        | test :: expressions ->
            test
            |> Eval.eval context (function
                | Ok(SBool false, _) -> expressions |> Eval.eachEval context cont (Ok(SUnspecified, pos))
                | Ok _ -> Ok(SUnspecified, pos) |> cont
                | x -> x |> cont)
        | x -> x |> invalidParameter pos "'%s' invalid unless parameter." |> cont

    let supportedFeatures =
        Set.ofList
            [ "r7rs"
              "exact-closed"
              "exact-rational"
              "ieee-float"
              "full-unicode"
              "ratios" ]

    [<TailCall>]
    let rec checkFeatureRequirement context pos negated =
        function
        | SSymbol feature, _ -> supportedFeatures |> Set.contains feature <> negated
        | SPair { car = SSymbol("and" | "or" as kind), _
                  cdr = args },
          _ ->
            match args |> toList with
            | Ok reqs ->
                if kind = "and" <> negated then
                    reqs |> List.forall (checkFeatureRequirement context pos negated)
                else
                    reqs |> List.exists (checkFeatureRequirement context pos negated)
            | Error _ -> negated
        | SPair { car = SSymbol "not", _
                  cdr = SPair { car = inner; cdr = SEmpty, _ }, _ },
          _ -> inner |> checkFeatureRequirement context pos (not negated)
        | SPair { car = SSymbol "library", _
                  cdr = SPair { car = libName; cdr = SEmpty, _ }, _ },
          _ ->
            match libName |> Context.lookupLibrary context pos with
            | Ok _ -> not negated
            | Error _ -> negated
        | _ -> negated

    [<TailCall>]
    let rec sCondExpand context pos cont =
        function
        | [] -> EvalError("No matching clause in cond-expand.", pos) |> Error |> cont
        | clause :: rest ->
            match clause with
            | SPair { car = SSymbol "else", _; cdr = body }, _ ->
                match body |> toList with
                | Ok exprs -> exprs |> Eval.eachEval context cont (Ok(SUnspecified, pos))
                | Error e -> Error e |> cont
            | SPair { car = req; cdr = body }, _ ->
                if checkFeatureRequirement context pos false req then
                    match body |> toList with
                    | Ok exprs -> exprs |> Eval.eachEval context cont (Ok(SUnspecified, pos))
                    | Error e -> Error e |> cont
                else
                    rest |> sCondExpand context pos cont
            | x -> x |> invalid (snd x) "'%s' invalid cond-expand clause." |> cont

    [<TailCall>]
    let rec bindLet context pos cont body acc =
        function
        | [] ->
            body
            |> Eval.evalBody (acc |> List.rev |> Context.extendEnvironments context) cont (Ok(SUnspecified, pos))
        | (variable, init) :: bindings ->
            init
            |> Eval.eval context (function
                | Ok i -> bindings |> bindLet context pos cont body ((variable, ref i) :: acc)
                | x -> x |> cont)

    let evalNamedLet context pos cont variable body =
        function
        | Ok bindings ->
            match bindings |> mapResult eachBinding with
            | Ok bindings' ->
                let r = ref (SUnspecified, pos)
                let context' = [ variable, r ] |> Context.extendEnvironments context
                let formals = bindings' |> List.map (fun (v, _) -> SSymbol v, pos) |> toSPair
                let proc = SProcedure(closure context' formals body), pos
                r.Value <- proc

                bindings'
                |> List.map snd
                |> Eval.evalArgs context' cont (fun e c a -> proc |> Eval.apply e c a) []
            | Error e -> Error e |> cont
        | Error e -> Error e |> cont

    let sLet context pos cont =
        function
        | (SSymbol variable, _) :: bindings :: body -> evalNamedLet context pos cont variable body (bindings |> toList)
        | bindings :: body ->
            match bindings |> toList with
            | Ok blist ->
                match blist |> mapResult eachBinding with
                | Ok bindings' -> bindings' |> bindLet context pos cont body []
                | Error e -> Error e |> cont
            | Error e -> Error e |> cont
        | x -> x |> invalidParameter pos "'%s' invalid let parameter." |> cont

    [<TailCall>]
    let rec bindLetStar context pos cont body =
        function
        | [] -> body |> Eval.evalBody context cont (Ok(SUnspecified, pos))
        | (variable, init) :: bindings ->
            init
            |> Eval.eval context (function
                | Ok i ->
                    bindings
                    |> bindLetStar ([ variable, ref i ] |> Context.extendEnvironments context) pos cont body
                | x -> x |> cont)

    let sLetStar context pos cont =
        function
        | bindings :: body ->
            match bindings |> toList with
            | Ok blist ->
                match blist |> mapResult eachBinding with
                | Ok bindings' -> bindings' |> bindLetStar context pos cont body
                | Error e -> Error e |> cont
            | Error e -> Error e |> cont
        | x -> x |> invalidParameter pos "'%s' invalid let* parameter." |> cont

    [<TailCall>]
    let rec bindLetRec context pos cont body =
        function
        | [] -> body |> Eval.evalBody context cont (Ok(SUnspecified, pos))
        | (variable, init) :: bindings ->
            init
            |> Eval.eval context (function
                | Ok i ->
                    Context.defineEnvironmentVariable context variable i
                    bindings |> bindLetRec context pos cont body
                | x -> x |> cont)

    let sLetRec context pos cont =
        let bindRef bindings =
            bindings
            |> List.map (function
                | variable, _ -> variable, ref (SEmpty, pos))
            |> Context.extendEnvironments context

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
    let rec bindLetRecStar context pos cont body =
        function
        | [], _
        | _, [] -> body |> Eval.evalBody context cont (Ok(SUnspecified, pos))
        | (_, init) :: bindings, refs: SExpression ref :: rs ->
            init
            |> Eval.eval context (function
                | Ok i ->
                    refs.Value <- i
                    (bindings, rs) |> bindLetRecStar context pos cont body
                | x -> x |> cont)

    let sLetRecStar context pos cont =
        let eachRef (context', refs) (variable, _) =
            let r = ref (SEmpty, pos)
            [ variable, r ] |> Context.extendEnvironments context', r :: refs

        let bindRef bindings =
            let context', refs = bindings |> List.fold eachRef (context, [])
            context', refs |> List.rev

        function
        | bindings :: body ->
            match bindings |> toList with
            | Ok blist ->
                match blist |> mapResult eachBinding with
                | Ok bindings' ->
                    let context', refs = bindRef bindings'
                    (bindings', refs) |> bindLetRecStar context' pos cont body
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

    let matchValuesBinding pos cont name variables init next =
        let values =
            match init with
            | SValues vs, _ -> vs
            | value -> [ value ]

        if List.length variables <> List.length values then
            EvalError(sprintf "Values count mismatch in %s." name, pos) |> Error |> cont
        else
            List.zip variables values
            |> List.map (fun (variable, value) -> variable, ref value)
            |> next

    [<TailCall>]
    let rec bindLetValues context pos cont body acc =
        function
        | [] ->
            body
            |> Eval.evalBody (acc |> List.rev |> Context.extendEnvironments context) cont (Ok(SUnspecified, pos))
        | (variables, init) :: bindings ->
            init
            |> Eval.eval context (function
                | Ok i ->
                    matchValuesBinding pos cont "let-values" variables i (fun bindings' ->
                        bindings |> bindLetValues context pos cont body (bindings' @ acc))
                | x -> x |> cont)

    let sLetValues context pos cont =
        function
        | bindings :: body ->
            match bindings |> toList with
            | Ok blist ->
                match blist |> mapResult eachValuesBinding with
                | Ok bindings' -> bindings' |> bindLetValues context pos cont body []
                | Error e -> Error e |> cont
            | Error e -> Error e |> cont
        | x -> x |> invalidParameter pos "'%s' invalid let-values parameter." |> cont

    [<TailCall>]
    let rec bindLetStarValues context pos cont body =
        function
        | [] -> body |> Eval.evalBody context cont (Ok(SUnspecified, pos))
        | (variables, init) :: bindings ->
            init
            |> Eval.eval context (function
                | Ok i ->
                    matchValuesBinding pos cont "let-star-values" variables i (fun bindings' ->
                        let nextContext = bindings' |> Context.extendEnvironments context
                        bindings |> bindLetStarValues nextContext pos cont body)
                | x -> x |> cont)

    let sLetStarValues context pos cont =
        function
        | bindings :: body ->
            match bindings |> toList with
            | Ok blist ->
                match blist |> mapResult eachValuesBinding with
                | Ok bindings' -> bindings' |> bindLetStarValues context pos cont body
                | Error e -> Error e |> cont
            | Error e -> Error e |> cont
        | x -> x |> invalidParameter pos "'%s' invalid let*-values parameter." |> cont

    let sBegin context pos cont =
        Eval.eachEval context cont (Ok(SUnspecified, pos))

    [<TailCall>]
    let rec loopDo context pos cont bindings test expressions commands loopContext =
        test
        |> Eval.eval loopContext (function
            | Ok(SBool false, _) ->
                commands
                |> Eval.eachEval
                    loopContext
                    (function
                    | Ok _ ->
                        bindings
                        |> evalDoStep context pos cont bindings test expressions commands loopContext []
                    | x -> x |> cont)
                    (Ok(SEmpty, pos))
            | Ok testResult ->
                match expressions with
                | [] -> Ok(SUnspecified, pos) |> cont
                | _ -> expressions |> Eval.eachEval loopContext cont (Ok testResult)
            | x -> x |> cont)

    and [<TailCall>] evalDoStep context pos cont bindings test expressions commands loopContext acc =
        function
        | [] ->
            acc
            |> List.rev
            |> Context.extendEnvironments context
            |> loopDo context pos cont bindings test expressions commands
        | (variable, _, _, Some step) :: bindings' ->
            step
            |> Eval.eval loopContext (function
                | Ok s ->
                    bindings'
                    |> evalDoStep
                        context
                        pos
                        cont
                        bindings
                        test
                        expressions
                        commands
                        loopContext
                        ((variable, ref s) :: acc)
                | x -> x |> cont)
        | (variable, varPos, _, None) :: bindings' ->
            match variable |> Context.lookupEnvironments loopContext varPos with
            | Ok v ->
                bindings'
                |> evalDoStep
                    context
                    pos
                    cont
                    bindings
                    test
                    expressions
                    commands
                    loopContext
                    ((variable, ref v.Value) :: acc)
            | Error e -> Error e |> cont

    [<TailCall>]
    let rec initDoVariables context pos cont bindings test expressions commands acc =
        function
        | [] ->
            acc
            |> List.rev
            |> Context.extendEnvironments context
            |> loopDo context pos cont bindings test expressions commands
        | (variable, _, init, _) :: bindings' ->
            init
            |> Eval.eval context (function
                | Ok i ->
                    bindings'
                    |> initDoVariables context pos cont bindings test expressions commands ((variable, ref i) :: acc)
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

    let sDo context pos cont =
        function
        | bindings :: testClause :: commands ->
            match testClause with
            | SPair { car = test; cdr = expressions }, _ ->
                match bindings |> toList with
                | Ok blist ->
                    match blist |> mapResult parseDoBinding with
                    | Ok bindings' ->
                        match expressions |> toList with
                        | Ok elist -> bindings' |> initDoVariables context pos cont bindings' test elist commands []
                        | Error e -> Error e |> cont
                    | Error e -> Error e |> cont
                | Error e -> Error e |> cont
            | _ -> [ testClause ] |> invalidParameter pos "'%s' invalid do test clause." |> cont
        | x -> x |> invalidParameter pos "'%s' invalid do parameter." |> cont

    let sDelay context pos cont =
        function
        | [ expression ] ->
            let thunk = closure context (SEmpty, pos) [ expression ]
            Ok(SPromise(ref (false, (SProcedure thunk, pos))), pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid delay parameter." |> cont

    let sDelayForce context pos cont =
        function
        | [ expression ] ->
            let thunk = closure context (SEmpty, pos) [ expression ]
            Ok(SPromise(ref (false, (SProcedure thunk, pos))), pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid delay-force parameter." |> cont

    let sParameterize context pos cont =
        function
        | parameters :: body ->
            match parameters |> toList with
            | Ok plist ->
                match plist |> mapResult eachParamBinding with
                | Ok parameters' -> parameters' |> loopParameterize context pos cont body []
                | Error e -> Error e |> cont
            | Error e -> Error e |> cont
        | x -> x |> invalidParameter pos "'%s' invalid parameterize parameter." |> cont

    let evalGuardClauses context pos cont variable savedWinders raised =
        function
        | Ok clauses ->
            let hasElse =
                match clauses |> List.tryLast with
                | Some(SPair { car = SSymbol "else", _; cdr = _ }, _) -> true
                | _ -> false

            let clauses' =
                if hasElse then
                    clauses
                else
                    clauses
                    @ [ [ SSymbol "else", pos; [ SSymbol "raise", pos; SQuote raised, pos ] |> toSPair ]
                        |> toSPair ]

            doWind
                context
                (fun _ ->
                    let context' = [ variable, ref raised ] |> Context.extendEnvironments context
                    clauses' |> sCond context' pos cont)
                savedWinders
                (Ok(SUnspecified, pos))
        | Error e -> Error e |> cont

    let sGuard context pos cont =
        function
        | (SPair { car = SSymbol variable, _
                   cdr = clauses },
           _) :: body ->
            let savedWinders = context.winders.Value

            body
            |> Eval.evalBody
                context
                (function
                | Ok res -> Ok res |> cont
                | Error(SchemeRaise(raised, _)) ->
                    clauses
                    |> toList
                    |> evalGuardClauses context pos cont variable savedWinders raised
                | x -> x |> cont)
                (Ok(SEmpty, pos))
        | x -> x |> invalidParameter pos "'%s' invalid guard parameter." |> cont

    type QqKeyword =
        | QqUnquote of SExpression
        | QqUnquoteSplicing of SExpression
        | QqQuasiquote of SExpression
        | QqQuote of SExpression

    let isQqKeyword (s: string) =
        s = "unquote" || s = "unquote-splicing" || s = "quasiquote" || s = "quote"

    let keywordToQq s t =
        match s with
        | "unquote" -> Some(QqUnquote t)
        | "unquote-splicing" -> Some(QqUnquoteSplicing t)
        | "quasiquote" -> Some(QqQuasiquote t)
        | _ -> Some(QqQuote t)

    let normalizeQqKeyword =
        function
        | SUnquote t, _ -> Some(QqUnquote t)
        | SUnquoteSplicing t, _ -> Some(QqUnquoteSplicing t)
        | SQuasiquote t, _ -> Some(QqQuasiquote t)
        | SQuote t, _ -> Some(QqQuote t)
        | SPair { car = SSymbol s, _
                  cdr = SPair { car = t; cdr = SEmpty, _ }, _ },
          _ when isQqKeyword s -> keywordToQq s t
        | _ -> None

    let consQq x b =
        match b with
        | SEmpty, _ -> [ x ] |> toSPair
        | SPair _, p -> SPair { car = x; cdr = b }, p
        | y -> SPair { car = x; cdr = y }, snd y

    let joinQq a b pos =
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

    [<TailCall>]
    let rec loopReplaceQuasiquote acc =
        function
        | SPair template, _ -> template.cdr |> loopReplaceQuasiquote (template.car :: acc)
        | x -> acc |> List.rev, x

    [<TailCall>]
    let rec replaceQuasiquote context pos cont n next =
        function
        | SEmpty, _ -> Ok(SEmpty, pos) |> next
        | SPair _, _ as template ->
            let templates, templateTail = template |> loopReplaceQuasiquote []
            templates |> replaceQuasiquoteList context pos cont n next templateTail
        | SVector templates, _ ->
            templates
            |> Array.toList
            |> replaceQuasiquoteList
                context
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
        | x -> x |> replaceQuasiquoteDatum context pos cont n next

    and [<TailCall>] chainExpand context pos cont n deltaN rest templateTail wrap next template =
        template
        |> replaceQuasiquote context pos cont (n + deltaN) (function
            | Ok a ->
                rest
                |> replaceQuasiquoteList
                    context
                    pos
                    cont
                    n
                    (Result.map (fun b -> consQq (wrap a) b) >> next)
                    templateTail
            | x -> x |> next)

    and [<TailCall>] replaceQuasiquoteList context pos cont n next templateTail templates =
        match templates with
        | [] -> templateTail |> replaceQuasiquoteDatum context pos cont n next
        | template :: rest ->
            match template |> normalizeQqKeyword with
            | Some(QqUnquote template) ->
                if n = 0 then
                    template
                    |> Eval.eval context (function
                        | Ok(SValues _, p) -> EvalError("Multiple values in single value context.", p) |> Error |> next
                        | Ok a ->
                            rest
                            |> replaceQuasiquoteList
                                context
                                pos
                                cont
                                n
                                (Result.map (fun b -> consQq a b) >> next)
                                templateTail
                        | x -> x |> next)
                else
                    template
                    |> chainExpand context pos cont n -1 rest templateTail (fun a -> (SUnquote a, pos)) next
            | Some(QqUnquoteSplicing template) ->
                if n = 0 then
                    template
                    |> Eval.eval context (function
                        | Ok a ->
                            rest
                            |> replaceQuasiquoteList
                                context
                                pos
                                cont
                                n
                                (Result.bind (fun b -> joinQq a b pos) >> next)
                                templateTail
                        | x -> x |> next)
                else
                    template
                    |> chainExpand context pos cont n -1 rest templateTail (fun a -> (SUnquoteSplicing a, pos)) next
            | Some(QqQuasiquote template) ->
                template
                |> chainExpand context pos cont n 1 rest templateTail (fun a -> (SQuasiquote a, pos)) next
            | Some(QqQuote template) ->
                template
                |> chainExpand context pos cont n 0 rest templateTail (fun a -> (SQuote a, pos)) next
            | None -> template |> chainExpand context pos cont n 0 rest templateTail id next

    and [<TailCall>] replaceQuasiquoteDatum context pos cont n next =
        function
        | arg ->
            match arg |> normalizeQqKeyword with
            | Some(QqUnquote template) ->
                if n = 0 then
                    template
                    |> Eval.eval context (function
                        | Ok(SValues _, p) -> EvalError("Multiple values in single value context.", p) |> Error |> next
                        | x -> x |> next)
                else
                    template
                    |> replaceQuasiquote context pos cont (n - 1) (Result.map (fun x' -> SUnquote x', pos) >> next)
            | Some(QqUnquoteSplicing template) ->
                if n = 0 then
                    EvalError("unquote-splicing must be in a list or vector context.", pos)
                    |> Error
                    |> next
                else
                    template
                    |> replaceQuasiquote
                        context
                        pos
                        cont
                        (n - 1)
                        (Result.map (fun x' -> SUnquoteSplicing x', pos) >> next)
            | Some(QqQuasiquote template) ->
                template
                |> replaceQuasiquote context pos cont (n + 1) (Result.map (fun x' -> SQuasiquote x', pos) >> next)
            | Some(QqQuote template) ->
                template
                |> replaceQuasiquote context pos cont n (Result.map (fun x' -> SQuote x', pos) >> next)
            | None -> arg |> Ok |> next

    let sQuasiquote context pos cont =
        function
        | [ template ] -> template |> replaceQuasiquote context pos cont 0 cont
        | x -> x |> invalidParameter pos "'%s' invalid quasiquote parameter." |> cont

    [<TailCall>]
    let rec arityMatches args =
        function
        | SEmpty, _ -> args |> List.isEmpty
        | SPair p, _ ->
            match args with
            | _ :: rest -> p.cdr |> arityMatches rest
            | [] -> false
        | _ -> true

    [<TailCall>]
    let rec caseClosure captureContext clauses context pos cont args =
        match clauses with
        | [] -> EvalError("No matching clause in case-lambda.", pos) |> Error |> cont
        | (formals, body) :: rest ->
            if formals |> arityMatches args then
                match formals |> zipFormals pos args with
                | Ok bindings ->
                    bindings
                    |> bindArgs (Context.mergeEnvironments context captureContext) pos cont body []
                | Error e -> Error e |> cont
            else
                caseClosure captureContext rest context pos cont args

    let sCaseLambda context pos cont clauses =
        let parseClause =
            function
            | SPair { car = formals; cdr = body }, _ ->
                match body |> toList with
                | Ok b -> Ok(formals, b)
                | Error e -> Error e
            | x -> x |> invalid (snd x) "'%s' invalid case-lambda clause."

        match clauses |> mapResult parseClause with
        | Ok parsedClauses -> Ok(SProcedure(caseClosure context parsedClauses), pos) |> cont
        | Error e -> Error e |> cont

    let processImportSetOnly cont ids =
        function
        | Ok bindings ->
            match ids |> toList with
            | Ok idList ->
                let mutable result = Map.empty
                let mutable err = None

                idList
                |> List.iter (function
                    | SSymbol id, pos ->
                        match bindings |> Map.tryFind id with
                        | Some r -> result <- result |> Map.add id r
                        | None -> err <- Some(EvalError(sprintf "only: identifier '%s' not exported." id, pos))
                    | x -> err <- Some(EvalError("only: identifier expected.", snd x)))

                match err with
                | Some e -> Error e |> cont
                | None -> Ok result |> cont
            | Error e -> Error e |> cont
        | x -> x |> cont

    let processImportSetExcept cont ids =
        function
        | Ok bindings ->
            match ids |> toList with
            | Ok idList ->
                let mutable result = bindings
                let mutable err = None

                idList
                |> List.iter (function
                    | SSymbol id, pos ->
                        if bindings |> Map.containsKey id then
                            result <- result |> Map.remove id
                        else
                            err <- Some(EvalError(sprintf "except: identifier '%s' not exported." id, pos))
                    | x -> err <- Some(EvalError("except: identifier expected.", snd x)))

                match err with
                | Some e -> Error e |> cont
                | None -> Ok result |> cont
            | Error e -> Error e |> cont
        | x -> x |> cont

    let processImportSetPrefix cont prefix =
        function
        | Ok bindings ->
            bindings
            |> Map.toSeq
            |> Seq.map (fun (name, r) -> prefix + name, r)
            |> Map.ofSeq
            |> Ok
            |> cont
        | x -> x |> cont

    let processImportSetRename cont renames =
        function
        | Ok bindings ->
            match renames |> toList with
            | Ok renameList ->
                let mutable result = bindings
                let mutable err = None

                renameList
                |> List.iter (function
                    | SPair { car = SSymbol fromId, _
                              cdr = SPair { car = SSymbol toId, _
                                            cdr = SEmpty, _ },
                                    _ },
                      pos ->
                        match bindings |> Map.tryFind fromId with
                        | Some r -> result <- result |> Map.remove fromId |> Map.add toId r
                        | None -> err <- Some(EvalError(sprintf "rename: identifier '%s' not exported." fromId, pos))
                    | x -> err <- Some(EvalError("rename: invalid rename clause.", snd x)))

                match err with
                | Some e -> Error e |> cont
                | None -> Ok result |> cont
            | Error e -> Error e |> cont
        | x -> x |> cont

    [<TailCall>]
    let rec processImportSet context pos cont =
        function
        | SPair { car = SSymbol "only", _
                  cdr = SPair { car = inner; cdr = ids }, _ },
          _ -> inner |> processImportSet context pos (processImportSetOnly cont ids)
        | SPair { car = SSymbol "except", _
                  cdr = SPair { car = inner; cdr = ids }, _ },
          _ -> inner |> processImportSet context pos (processImportSetExcept cont ids)
        | SPair { car = SSymbol "prefix", _
                  cdr = SPair { car = inner
                                cdr = SPair { car = SSymbol prefix, _
                                              cdr = SEmpty, _ },
                                      _ },
                        _ },
          _ -> inner |> processImportSet context pos (processImportSetPrefix cont prefix)
        | SPair { car = SSymbol "rename", _
                  cdr = SPair { car = inner; cdr = renames }, _ },
          _ -> inner |> processImportSet context pos (processImportSetRename cont renames)
        | imports ->
            match imports |> Context.lookupLibrary context pos with
            | Ok lib ->
                lib.exports
                |> Map.fold
                    (fun acc externalName internalName ->
                        match internalName |> Context.tryLookupEnvironment lib.environment with
                        | Some r -> acc |> Map.add externalName r
                        | None -> acc)
                    Map.empty
                |> Ok
                |> cont
            | Error e -> Error e |> cont

    [<TailCall>]
    let rec sImport context pos cont =
        function
        | [] -> Ok(SUnspecified, pos) |> cont
        | importSet :: rest ->
            importSet
            |> processImportSet context pos (function
                | Ok bindings ->
                    let currentEnv = context.environments.Head

                    bindings
                    |> Map.iter (fun name refVal -> currentEnv.Value <- currentEnv.Value |> Map.add name refVal)

                    rest |> sImport context pos cont
                | Error e -> Error e |> cont)

    let sDefine context pos cont =
        let define' variable =
            Result.map (fun value ->
                Context.defineEnvironmentVariable context variable value
                SUnspecified, pos)
            >> cont

        function
        | [ SSymbol variable, _; expression ] -> expression |> Eval.eval context (define' variable)
        | (SPair { car = SSymbol variable, _
                   cdr = formals },
           _) :: body -> sLambda context pos (define' variable) (formals :: body)
        | x -> x |> invalidParameter pos "'%s' invalid define parameter." |> cont

    [<TailCall>]
    let rec bindDefineValues pos context cont =
        function
        | [] -> Ok(SUnspecified, pos) |> cont
        | (variable, expression) :: bindings ->
            expression
            |> Eval.eval context (function
                | Ok value ->
                    Context.defineEnvironmentVariable context variable value
                    bindings |> bindDefineValues pos context cont
                | x -> x |> cont)

    [<TailCall>]
    let rec loopZipFormalsRef pos values acc =
        function
        | SEmpty, _ ->
            if values |> List.isEmpty then
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

    let evalDefineValues context pos cont formals result =
        let values =
            match result with
            | SValues vs, _ -> vs
            | value -> [ value ]

        match formals |> zipFormalsRef pos values with
        | Ok bindings ->
            bindings
            |> List.map (fun (v, r) -> v, r.Value)
            |> bindDefineValues pos context cont
        | Error e -> Error e |> cont

    let sDefineValues context pos cont =
        function
        | [ formals; expression ] ->
            expression
            |> Eval.eval context (function
                | Ok result -> result |> evalDefineValues context pos cont formals
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
        context
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

    let recordPredProc typeId context pos cont =
        function
        | [ SRecord(tid, _, _), _ ] -> Ok(tid = typeId |> toSBool, pos) |> cont
        | _ -> Ok(SFalse, pos) |> cont

    let recordFieldAccessorProc typeId name idx accessorName context pos cont =
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

    let recordFieldModifierProc typeId name idx modifierName context pos cont =
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

    let sDefineRecordType context pos cont =
        function
        | (SSymbol name, _) :: (SPair { car = SSymbol constructorName, _
                                        cdr = constructorFields },
                                _) :: (SSymbol pred, _) :: fields ->
            let defineProc name proc =
                Context.defineEnvironmentVariable context name (proc |> SProcedure, pos)

            let typeId = Context.getNextRecordTypeId context

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

                    Ok(SUnspecified, pos) |> cont
                | Error e -> Error e |> cont
            | Error e -> Error e |> cont
        | x -> x |> invalidParameter pos "'%s' invalid define-record-type parameter." |> cont

    [<TailCall>]
    let rec loopLibraryExport pos acc =
        function
        | [] -> Ok acc
        | (SSymbol name, _) :: rest -> rest |> loopLibraryExport pos (acc |> Map.add name name)
        | (SPair { car = SSymbol "rename", _
                   cdr = SPair { car = SSymbol oldName, _
                                 cdr = SPair { car = SSymbol newName, _
                                               cdr = SEmpty, _ },
                                       _ },
                         _ },
           _) :: rest -> rest |> loopLibraryExport pos (acc |> Map.add newName oldName)
        | x :: _ -> [ x ] |> invalidParameter pos "'%s' invalid export parameter."

    let processLibraryExport exports pos cont declaration =
        match declaration |> toList with
        | Ok dlist -> dlist |> loopLibraryExport pos exports |> cont
        | Error e -> Error e |> cont

    [<TailCall>]
    let rec readLibraryDeclarations pos foldCase acc =
        function
        | [] -> acc |> List.rev |> Ok
        | (SString f, fp) :: rest ->
            match tryReadAll foldCase f fp with
            | Ok expressions -> rest |> readLibraryDeclarations pos foldCase (List.rev expressions @ acc)
            | Error e -> Error e
        | x :: _ ->
            EvalError(sprintf "'%s' invalid include-library-declarations parameter." (x |> Print.print), pos)
            |> Error

    type LibDecl =
        | ImportDecl of importSets: SExpression
        | ExportDecl of exportSpecs: SExpression
        | BeginDecl of exprs: SExpression
        | IncludeDecl of files: SExpression * pos: Position option
        | IncludeCiDecl of files: SExpression * pos: Position option
        | IncludeLibDecl of files: SExpression * pos: Position option
        | CondExpandDecl of clauses: SExpression * expandPos: Position option

    let parseLibraryDeclaration =
        function
        | SPair { car = SSymbol "import", _
                  cdr = importSets },
          _ -> Ok(ImportDecl importSets)
        | SPair { car = SSymbol "export", _
                  cdr = exportSpecs },
          _ -> Ok(ExportDecl exportSpecs)
        | SPair { car = SSymbol "begin", _
                  cdr = exprs },
          _ -> Ok(BeginDecl exprs)
        | SPair { car = SSymbol "include", p
                  cdr = files },
          _ -> Ok(IncludeDecl(files, p))
        | SPair { car = SSymbol "include-ci", p
                  cdr = files },
          _ -> Ok(IncludeCiDecl(files, p))
        | SPair { car = SSymbol "include-library-declarations", p
                  cdr = files },
          _ -> Ok(IncludeLibDecl(files, p))
        | SPair { car = SSymbol "cond-expand", expandPos
                  cdr = clauses },
          _ -> Ok(CondExpandDecl(clauses, expandPos))
        | x -> x |> invalid (snd x) "'%s' invalid library declaration."

    [<TailCall>]
    let rec processLibraryDeclaration pos cont foldCase libContext exports =
        function
        | [] -> Ok exports |> cont
        | declaration :: declarations ->
            match parseLibraryDeclaration declaration with
            | Ok(ImportDecl importSets) ->
                processImportDecl pos cont foldCase libContext exports importSets declarations
            | Ok(ExportDecl exportSpecs) ->
                processExportDecl pos cont foldCase libContext exports exportSpecs declarations
            | Ok(BeginDecl exprs) -> processBeginDecl pos cont foldCase libContext exports exprs declarations
            | Ok(IncludeDecl(files, p)) -> processIncludeDecl pos cont foldCase libContext exports files p declarations
            | Ok(IncludeCiDecl(files, p)) ->
                processIncludeCiDecl pos cont foldCase libContext exports files p declarations
            | Ok(IncludeLibDecl(files, p)) ->
                processIncludeLibDecl pos cont foldCase libContext exports files p declarations
            | Ok(CondExpandDecl(clauses, expandPos)) ->
                processCondExpandDecl pos cont foldCase libContext exports clauses expandPos declarations
            | Error e -> Error e |> cont

    and processImportDecl pos cont foldCase libContext exports importSets declarations =
        match importSets |> toList with
        | Ok isets ->
            isets
            |> sImport libContext pos (function
                | Ok _ -> declarations |> processLibraryDeclaration pos cont foldCase libContext exports
                | Error e -> Error e |> cont)
        | Error e -> Error e |> cont

    and processExportDecl pos cont foldCase libContext exports exportSpecs declarations =
        exportSpecs
        |> processLibraryExport exports pos (function
            | Ok newExports ->
                declarations
                |> processLibraryDeclaration pos cont foldCase libContext newExports
            | Error e -> Error e |> cont)

    and processBeginDecl pos cont foldCase libContext exports exprs declarations =
        match exprs |> toList with
        | Ok elist ->
            elist
            |> Eval.eachEval
                libContext
                (function
                | Ok _ -> declarations |> processLibraryDeclaration pos cont foldCase libContext exports
                | Error e -> Error e |> cont)
                (Ok(SUnspecified, pos))
        | Error e -> Error e |> cont

    and processIncludeDecl pos cont foldCase libContext exports files filePos declarations =
        match files |> toList with
        | Ok flist ->
            flist
            |> sIncludeFiles
                false
                libContext
                filePos
                (function
                | Ok _ -> declarations |> processLibraryDeclaration pos cont foldCase libContext exports
                | Error e -> Error e |> cont)
                []
        | Error e -> Error e |> cont

    and processIncludeCiDecl pos cont foldCase libContext exports files filePos declarations =
        match files |> toList with
        | Ok flist ->
            flist
            |> sIncludeFiles
                true
                libContext
                filePos
                (function
                | Ok _ -> declarations |> processLibraryDeclaration pos cont foldCase libContext exports
                | Error e -> Error e |> cont)
                []
        | Error e -> Error e |> cont

    and processIncludeLibDecl pos cont foldCase libContext exports files filePos declarations =
        match files |> toList with
        | Ok flist ->
            match flist |> readLibraryDeclarations filePos foldCase [] with
            | Ok decls ->
                decls @ declarations
                |> processLibraryDeclaration pos cont foldCase libContext exports
            | Error e -> Error e |> cont
        | Error e -> Error e |> cont

    and processCondExpandDecl pos cont foldCase libContext exports clauses expandPos declarations =
        match clauses |> toList with
        | Ok clist ->
            clist
            |> evalLibraryCondExpand pos cont foldCase libContext exports expandPos declarations
        | Error e -> Error e |> cont

    and [<TailCall>] evalLibraryCondExpand pos cont foldCase libContext exports expandPos declarations =
        function
        | [] -> EvalError("No matching clause in cond-expand.", expandPos) |> Error |> cont
        | clause :: cRest ->
            match clause with
            | SPair { car = SSymbol "else", _; cdr = body }, _ ->
                match body |> toList with
                | Ok expressions ->
                    expressions @ declarations
                    |> processLibraryDeclaration pos cont foldCase libContext exports
                | Error e -> Error e |> cont
            | SPair { car = requirement; cdr = body }, _ ->
                if checkFeatureRequirement libContext pos false requirement then
                    match body |> toList with
                    | Ok expressions ->
                        expressions @ declarations
                        |> processLibraryDeclaration pos cont foldCase libContext exports
                    | Error e -> Error e |> cont
                else
                    cRest
                    |> evalLibraryCondExpand pos cont foldCase libContext exports expandPos declarations
            | x -> x |> invalid (snd x) "'%s' invalid cond-expand clause." |> cont

    let sDefineLibrary context pos cont =
        function
        | name :: declarations ->
            let libContext = [] |> Context.extendEnvironments { context with environments = [] }

            declarations
            |> processLibraryDeclaration
                pos
                (function
                | Ok exports ->
                    Context.registerLibrary context name libContext.environments.Head exports
                    Ok(SUnspecified, pos) |> cont
                | Error e -> Error e |> cont)
                false
                libContext
                Map.empty
        | x -> x |> invalidParameter pos "'%s' invalid define-library parameter." |> cont
