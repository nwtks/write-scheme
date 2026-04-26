namespace WriteScheme.Builtins

open WriteScheme
open Type

[<AutoOpen>]
module SpecialForm =
    let sQuote envs cont =
        function
        | [ x ] -> x |> cont
        | x -> x |> invalidParameter "'%s' invalid quote parameter."

    [<TailCall>]
    let rec loopZipFormals acc args =
        function
        | SEmpty ->
            if List.isEmpty args then
                acc
            else
                failwith "too many arguments"
        | SSymbol var -> (var, args |> toSPair |> SQuote) :: acc
        | SPair p ->
            match args with
            | h :: t ->
                match p.car with
                | SSymbol var -> p.cdr |> loopZipFormals ((var, h) :: acc) t
                | x -> x |> invalid "'%s' not symbol."
            | [] -> failwith "not enough arguments"
        | x -> x |> invalid "'%s' not symbol."

    let zipFormals args =
        function
        | SSymbol var -> [ var, args |> toSPair |> SQuote ]
        | x -> x |> loopZipFormals [] args |> List.rev

    [<TailCall>]
    let rec bindArgs envs cont body acc =
        function
        | [] -> body |> Eval.eachEval (acc |> List.rev |> Context.extendEnvs envs) cont SEmpty
        | (var, expr) :: xs ->
            expr
            |> Eval.eval envs (fun a -> xs |> bindArgs envs cont body ((var, ref a) :: acc))

    let closure captureEnvs formals body envs cont args =
        formals
        |> zipFormals args
        |> bindArgs (Context.mergeEnvs envs captureEnvs) cont body []

    [<TailCall>]
    let rec loopZipFormalsRef acc args =
        function
        | SEmpty ->
            if List.isEmpty args then
                acc
            else
                failwith "too many arguments"
        | SSymbol var -> (var, ref (args |> toSPair |> SQuote)) :: acc
        | SPair p ->
            match args with
            | h :: t ->
                match p.car with
                | SSymbol var -> p.cdr |> loopZipFormalsRef ((var, ref h) :: acc) t
                | x -> x |> invalid "'%s' not symbol."
            | [] -> failwith "not enough arguments"
        | x -> x |> invalid "'%s' not symbol."

    let zipFormalsRef args =
        function
        | SSymbol var -> [ var, ref (args |> toSPair |> SQuote) ]
        | x -> x |> loopZipFormalsRef [] args |> List.rev

    let sLambda envs cont =
        function
        | formals :: body -> closure envs formals body |> SSyntax |> cont
        | x -> x |> invalidParameter "'%s' invalid lambda parameter."

    let sIf envs cont =
        let if' test conseq alter =
            test
            |> Eval.eval envs (function
                | SBool false -> alter |> Eval.eval envs cont
                | _ -> conseq |> Eval.eval envs cont)

        function
        | [ test; conseq; alter ] -> if' test conseq alter
        | [ test; conseq ] -> if' test conseq SEmpty
        | x -> x |> invalidParameter "'%s' invalid if parameter."

    let sSet envs cont =
        function
        | [ SSymbol var; expr ] ->
            expr
            |> Eval.eval envs (fun x ->
                (Context.lookupEnvs envs var).Value <- x
                x |> cont)
        | x -> x |> invalidParameter "'%s' invalid set! parameter."

    [<TailCall>]
    let rec sCond envs cont =
        function
        | [] -> SEmpty |> cont
        | clause :: clauses ->
            match clause with
            | SPair { car = SSymbol "else"; cdr = exprs } -> exprs |> toList |> Eval.eachEval envs cont SEmpty
            | SPair { car = test
                      cdr = SPair { car = SSymbol "=>"
                                    cdr = SPair { car = expr; cdr = SEmpty } } } ->
                test
                |> Eval.eval envs (function
                    | SBool false -> clauses |> sCond envs cont
                    | a -> [ expr; SQuote a ] |> toSPair |> Eval.eval envs cont)
            | SPair { car = test; cdr = exprs } ->
                test
                |> Eval.eval envs (function
                    | SBool false -> clauses |> sCond envs cont
                    | a -> exprs |> toList |> Eval.eachEval envs cont a)
            | x -> x |> invalid "'%s' invalid cond clause."

    [<TailCall>]
    let rec testCase envs cont key =
        function
        | [] -> SEmpty |> cont
        | clause :: clauses ->
            match clause with
            | SPair { car = SSymbol "else"
                      cdr = SPair { car = SSymbol "=>"
                                    cdr = SPair { car = expr; cdr = SEmpty } } } ->
                [ expr; SQuote key ] |> toSPair |> Eval.eval envs cont
            | SPair { car = SSymbol "else"; cdr = exprs } -> exprs |> toList |> Eval.eachEval envs cont SEmpty
            | SPair { car = datums
                      cdr = SPair { car = SSymbol "=>"
                                    cdr = SPair { car = expr; cdr = SEmpty } } } ->
                if datums |> toList |> List.exists (fun d -> eqv (key, d)) then
                    [ expr; SQuote key ] |> toSPair |> Eval.eval envs cont
                else
                    clauses |> testCase envs cont key
            | SPair { car = datums; cdr = exprs } ->
                if datums |> toList |> List.exists (fun d -> eqv (key, d)) then
                    exprs |> toList |> Eval.eachEval envs cont SEmpty
                else
                    clauses |> testCase envs cont key
            | x -> x |> invalid "'%s' invalid case clause."

    let sCase envs cont =
        function
        | key :: clauses -> key |> Eval.eval envs (fun k -> testCase envs cont k clauses)
        | x -> x |> invalidParameter "'%s' invalid case parameter."

    [<TailCall>]
    let rec sAnd envs cont =
        function
        | [] -> STrue |> cont
        | [ test ] ->
            test
            |> Eval.eval envs (function
                | SBool false -> SFalse |> cont
                | x -> x |> cont)
        | test :: tests ->
            test
            |> Eval.eval envs (function
                | SBool false -> SFalse |> cont
                | _ -> tests |> sAnd envs cont)

    [<TailCall>]
    let rec sOr envs cont =
        function
        | [] -> SFalse |> cont
        | test :: tests ->
            test
            |> Eval.eval envs (function
                | SBool false -> tests |> sOr envs cont
                | x -> x |> cont)

    let sWhen envs cont =
        function
        | test :: exprs ->
            test
            |> Eval.eval envs (function
                | SBool false -> SEmpty |> cont
                | _ -> exprs |> Eval.eachEval envs cont SEmpty)
        | x -> x |> invalidParameter "'%s' invalid when parameter."

    let sUnless envs cont =
        function
        | test :: exprs ->
            test
            |> Eval.eval envs (function
                | SBool false -> exprs |> Eval.eachEval envs cont SEmpty
                | _ -> SEmpty |> cont)
        | x -> x |> invalidParameter "'%s' invalid unless parameter."

    [<TailCall>]
    let rec bindLet envs cont body acc =
        function
        | [] -> body |> Eval.eachEval (acc |> List.rev |> Context.extendEnvs envs) cont SEmpty
        | (var, expr) :: xs ->
            expr
            |> Eval.eval envs (fun a -> xs |> bindLet envs cont body ((var, ref a) :: acc))

    [<TailCall>]
    let rec loopBindingLet acc args =
        function
        | SEmpty -> acc
        | SSymbol v -> (v, ref (args |> toSPair)) :: acc
        | SPair p ->
            match args with
            | h :: t ->
                match p.car with
                | SSymbol v -> p.cdr |> loopBindingLet ((v, ref h) :: acc) t
                | _ -> failwith "not symbol"
            | [] -> failwith "not enough args"
        | _ -> failwith "not symbol"

    let bindingLet envs cont bindings body captureEnvs args =
        let boundVars =
            bindings
            |> List.map (fun (v, _) -> SSymbol v)
            |> toSPair
            |> loopBindingLet [] args
            |> List.rev

        body
        |> Eval.eachEval
            (Context.mergeEnvs captureEnvs envs
             |> fun ctx -> Context.extendEnvs ctx boundVars)
            cont
            SEmpty

    let sLet envs cont =
        function
        | bindings :: body ->
            match bindings with
            | SSymbol var ->
                match body with
                | bBindings :: bBody ->
                    let bindings' = bBindings |> toList |> List.map eachBinding
                    let r = ref SUnspecified
                    let envs' = [ var, r ] |> Context.extendEnvs envs
                    let proc = SProcedure(fun _ c a -> bindingLet envs c bindings' bBody envs' a)
                    r.Value <- proc

                    bindings'
                    |> List.map snd
                    |> Eval.evalArgs envs' cont (fun e c a -> Eval.apply e c a proc) []
                | _ -> body |> invalidParameter "'%s' invalid named let."
            | _ -> bindings |> toList |> List.map eachBinding |> bindLet envs cont body []
        | x -> x |> invalidParameter "'%s' invalid let parameter."

    let eachValuesBinding =
        function
        | SPair { car = formalsExpr
                  cdr = SPair { car = expr; cdr = SEmpty } } ->
            let vars =
                formalsExpr
                |> toList
                |> List.map (function
                    | SSymbol v -> v
                    | x -> x |> invalid "'%s' not symbol.")

            vars, expr
        | x -> x |> invalid "'%s' invalid let-values binding."

    [<TailCall>]
    let rec bindLetStar envs cont body =
        function
        | [] -> body |> Eval.eachEval envs cont SEmpty
        | (var, expr) :: xs ->
            expr
            |> Eval.eval envs (fun v -> xs |> bindLetStar ([ var, ref v ] |> Context.extendEnvs envs) cont body)

    let sLetStar envs cont =
        function
        | bindings :: body -> bindings |> toList |> List.map eachBinding |> bindLetStar envs cont body
        | x -> x |> invalidParameter "'%s' invalid let* parameter."

    [<TailCall>]
    let rec bindLetRecExpr envs cont body =
        function
        | [] -> body |> Eval.eachEval envs cont SEmpty
        | (var, expr) :: xs ->
            expr
            |> Eval.eval envs (fun a ->
                Context.defineEnvVar envs var a
                xs |> bindLetRecExpr envs cont body)

    let sLetRec envs cont =
        let bindRef bindings =
            bindings
            |> List.map (function
                | var, _ -> var, ref SEmpty)
            |> Context.extendEnvs envs

        function
        | bindings :: body ->
            let bindings' = bindings |> toList |> List.map eachBinding
            bindings' |> bindLetRecExpr (bindings' |> bindRef) cont body
        | x -> x |> invalidParameter "'%s' invalid letrec parameter."

    [<TailCall>]
    let rec bindLetRecStarExpr envs cont body =
        function
        | [], _
        | _, [] -> body |> Eval.eachEval envs cont SEmpty
        | (_, expr) :: xs, r: SExpression ref :: rs ->
            expr
            |> Eval.eval envs (fun v ->
                r.Value <- v
                (xs, rs) |> bindLetRecStarExpr envs cont body)

    let sLetRecStar envs cont =
        let eachRef (envs', refs) (var, _) =
            let r = ref SEmpty
            [ var, r ] |> Context.extendEnvs envs', r :: refs

        let bindRef bindings =
            let envs', refs = bindings |> List.fold eachRef (envs, [])
            envs', List.rev refs

        function
        | bindings :: body ->
            let bindings' = bindings |> toList |> List.map eachBinding
            let envs', refs = bindRef bindings'
            (bindings', refs) |> bindLetRecStarExpr envs' cont body
        | x -> x |> invalidParameter "'%s' invalid letrec* parameter."

    [<TailCall>]
    let rec bindLetValues envs cont body acc =
        function
        | [] -> body |> Eval.eachEval (acc |> List.rev |> Context.extendEnvs envs) cont SEmpty
        | (vars, expr) :: xs ->
            expr
            |> Eval.eval envs (fun v ->
                let vals =
                    match v with
                    | SValues vs -> vs
                    | single -> [ single ]

                if List.length vars <> List.length vals then
                    failwith "values count mismatch"

                let bindings = List.zip vars vals |> List.map (fun (vr, vl) -> vr, ref vl)
                xs |> bindLetValues envs cont body (bindings @ acc))

    let sLetValues envs cont =
        function
        | bindings :: body ->
            bindings
            |> toList
            |> List.map eachValuesBinding
            |> bindLetValues envs cont body []
        | x -> x |> invalidParameter "'%s' invalid let-values parameter."

    [<TailCall>]
    let rec bindLetStarValues envs cont body =
        function
        | [] -> body |> Eval.eachEval envs cont SEmpty
        | (vars, expr) :: xs ->
            expr
            |> Eval.eval envs (fun v ->
                let vals =
                    match v with
                    | SValues vs -> vs
                    | single -> [ single ]

                if List.length vars <> List.length vals then
                    failwith "values count mismatch"

                let nextEnvs =
                    List.zip vars vals
                    |> List.map (fun (vr, vl) -> vr, ref vl)
                    |> Context.extendEnvs envs

                xs |> bindLetStarValues nextEnvs cont body)

    let sLetStarValues envs cont =
        function
        | bindings :: body ->
            bindings
            |> toList
            |> List.map eachValuesBinding
            |> bindLetStarValues envs cont body
        | x -> x |> invalidParameter "'%s' invalid let*-values parameter."

    let sBegin envs cont = Eval.eachEval envs cont SEmpty

    [<TailCall>]
    let rec loopDo envs cont test exprs commands bindings loopEnvs =
        test
        |> Eval.eval loopEnvs (function
            | SBool false ->
                commands
                |> Eval.eachEval
                    loopEnvs
                    (fun _ -> bindings |> evalDoStep envs cont test exprs commands bindings loopEnvs [])
                    SEmpty
            | testResult ->
                match exprs with
                | [] -> SEmpty |> cont
                | _ -> exprs |> Eval.eachEval loopEnvs cont testResult)

    and [<TailCall>] evalDoStep envs cont test exprs commands bindings loopEnvs acc =
        function
        | [] ->
            acc
            |> List.rev
            |> Context.extendEnvs envs
            |> loopDo envs cont test exprs commands bindings
        | (var, _, Some step) :: xs ->
            step
            |> Eval.eval loopEnvs (fun v ->
                xs
                |> evalDoStep envs cont test exprs commands bindings loopEnvs ((var, ref v) :: acc))
        | (var, _, None) :: xs ->
            let v = (Context.lookupEnvs loopEnvs var).Value

            xs
            |> evalDoStep envs cont test exprs commands bindings loopEnvs ((var, ref v) :: acc)

    [<TailCall>]
    let rec initDoVariables envs cont test exprs commands bindings acc =
        function
        | [] ->
            acc
            |> List.rev
            |> Context.extendEnvs envs
            |> loopDo envs cont test exprs commands bindings
        | (var, init, _) :: xs ->
            init
            |> Eval.eval envs (fun v ->
                xs
                |> initDoVariables envs cont test exprs commands bindings ((var, ref v) :: acc))

    let sDo envs cont =
        let parseBinding =
            function
            | SPair { car = SSymbol var
                      cdr = SPair { car = init
                                    cdr = SPair { car = step; cdr = SEmpty } } } -> var, init, Some step
            | SPair { car = SSymbol var
                      cdr = SPair { car = init; cdr = SEmpty } } -> var, init, None
            | x -> [ x ] |> invalidParameter "'%s' invalid do binding parameter."

        function
        | bindings :: testClause :: commands ->
            match testClause with
            | SPair { car = test; cdr = exprs } ->
                let bindings' = bindings |> toList |> List.map parseBinding

                bindings'
                |> initDoVariables envs cont test (exprs |> toList) commands bindings' []
            | _ -> [ testClause ] |> invalidParameter "'%s' invalid do test clause."
        | x -> x |> invalidParameter "'%s' invalid do parameter."

    let sDelay envs cont =
        function
        | [ expr ] ->
            let thunk = closure envs SEmpty [ expr ]
            SPromise(ref (false, SSyntax thunk)) |> cont
        | x -> x |> invalidParameter "'%s' invalid delay parameter."

    let sDelayForce envs cont =
        function
        | [ expr ] ->
            let thunk = closure envs SEmpty [ expr ]
            SPromise(ref (false, SSyntax thunk)) |> cont
        | x -> x |> invalidParameter "'%s' invalid delay-force parameter."

    let sParameterize envs cont =
        function
        | bindings :: body ->
            bindings
            |> toList
            |> List.map eachParamBinding
            |> bindParameterize envs cont body []
        | x -> x |> invalidParameter "'%s' invalid parameterize parameter."

    let sGuard envs cont =
        function
        | SPair { car = SSymbol var; cdr = clauses } :: body ->
            let savedWinders = envs.currentWinders.Value

            try
                body |> Eval.eachEval envs cont SEmpty
            with SchemeRaise obj ->
                let clausesList = clauses |> toList

                let hasElse =
                    match List.tryLast clausesList with
                    | Some(SPair { car = SSymbol "else"; cdr = _ }) -> true
                    | _ -> false

                let finalClauses =
                    if hasElse then
                        clausesList
                    else
                        clausesList
                        @ [ toSPair [ SSymbol "else"; toSPair [ SSymbol "raise"; SQuote obj ] ] ]

                doWind
                    envs
                    (fun _ ->
                        let envs' = [ var, ref obj ] |> Context.extendEnvs envs
                        finalClauses |> sCond envs' cont)
                    savedWinders
        | x -> x |> invalidParameter "'%s' invalid guard parameter."

    [<TailCall>]
    let rec loopReplaceQuasiquote acc =
        function
        | SPair p -> p.cdr |> loopReplaceQuasiquote (p.car :: acc)
        | x -> List.rev acc, x

    [<TailCall>]
    let rec replaceQuasiquote envs cont n next =
        function
        | SEmpty -> SEmpty |> next
        | SPair _ as x ->
            let xs, tail = x |> loopReplaceQuasiquote []
            xs |> replaceQuasiquoteList envs cont n next tail
        | SVector xs ->
            xs
            |> Array.toList
            |> replaceQuasiquoteList
                envs
                cont
                n
                (function
                | SEmpty -> [||] |> SVector |> next
                | SPair _ as y -> y |> toList |> List.toArray |> SVector |> next
                | y -> y |> next)
                SEmpty
        | x -> x |> replaceQuasiquoteDatum envs cont n next

    and [<TailCall>] replaceQuasiquoteList envs cont n next tail xs =
        let cons x b =
            match b with
            | SEmpty -> [ x ] |> toSPair
            | SPair _ -> SPair { car = x; cdr = b }
            | y -> SPair { car = x; cdr = y }

        let join a b =
            match a with
            | SEmpty -> b
            | SPair _ ->
                try
                    List.foldBack (fun h acc -> SPair { car = h; cdr = acc }) (a |> toList) b
                with _ ->
                    failwith "unquote-splicing must return a list"
            | _ -> [ a ] |> invalidParameter "'%s' invalid unquote-splicing parameter."

        match xs with
        | [] -> tail |> replaceQuasiquoteDatum envs cont n next
        | (SUnquote x | SPair { car = SSymbol "unquote"
                                cdr = SPair { car = x; cdr = SEmpty } }) :: rest ->
            if n = 0 then
                x
                |> Eval.eval envs (fun a -> rest |> replaceQuasiquoteList envs cont n (fun b -> cons a b |> next) tail)
            else
                x
                |> replaceQuasiquote envs cont (n - 1) (fun a ->
                    rest
                    |> replaceQuasiquoteList envs cont n (fun b -> cons (SUnquote a) b |> next) tail)
        | (SUnquoteSplicing x | SPair { car = SSymbol "unquote-splicing"
                                        cdr = SPair { car = x; cdr = SEmpty } }) :: rest ->
            if n = 0 then
                x
                |> Eval.eval envs (fun a -> rest |> replaceQuasiquoteList envs cont n (fun b -> join a b |> next) tail)
            else
                x
                |> replaceQuasiquote envs cont (n - 1) (fun a ->
                    rest
                    |> replaceQuasiquoteList envs cont n (fun b -> cons (SUnquoteSplicing a) b |> next) tail)
        | SPair { car = SSymbol "quasiquote"
                  cdr = SPair { car = x; cdr = SEmpty } } :: rest ->
            x
            |> replaceQuasiquote envs cont (n + 1) (fun a ->
                rest
                |> replaceQuasiquoteList envs cont n (fun b -> cons (SQuasiquote a) b |> next) tail)
        | SPair { car = SSymbol "quote"
                  cdr = SPair { car = x; cdr = SEmpty } } :: rest ->
            x
            |> replaceQuasiquote envs cont n (fun a ->
                rest
                |> replaceQuasiquoteList envs cont n (fun b -> cons (SQuote a) b |> next) tail)
        | x :: rest ->
            x
            |> replaceQuasiquote envs cont n (fun a ->
                rest |> replaceQuasiquoteList envs cont n (fun b -> cons a b |> next) tail)

    and [<TailCall>] replaceQuasiquoteDatum envs cont n next =
        function
        | SUnquote x
        | SPair { car = SSymbol "unquote"
                  cdr = SPair { car = x; cdr = SEmpty } } ->
            if n = 0 then
                x |> Eval.eval envs next
            else
                x |> replaceQuasiquote envs cont (n - 1) (SUnquote >> next)
        | SUnquoteSplicing x
        | SPair { car = SSymbol "unquote-splicing"
                  cdr = SPair { car = x; cdr = SEmpty } } ->
            if n = 0 then
                x |> Eval.eval envs next
            else
                x |> replaceQuasiquote envs cont (n - 1) (SUnquoteSplicing >> next)
        | SQuasiquote x
        | SPair { car = SSymbol "quasiquote"
                  cdr = SPair { car = x; cdr = SEmpty } } ->
            x |> replaceQuasiquote envs cont (n + 1) (SQuasiquote >> next)
        | SQuote x
        | SPair { car = SSymbol "quote"
                  cdr = SPair { car = x; cdr = SEmpty } } -> x |> replaceQuasiquote envs cont n (SQuote >> next)
        | x -> x |> next

    let sQuasiquote envs cont =
        function
        | [ x ] -> x |> replaceQuasiquote envs cont 0 cont
        | x -> x |> invalidParameter "'%s' invalid quasiquote parameter."

    let sDefine envs cont =
        let define' var =
            Eval.eval envs (fun x ->
                Context.defineEnvVar envs var x
                var |> SSymbol |> cont)

        function
        | [ SSymbol var; expr ] -> expr |> define' var
        | SPair { car = SSymbol var; cdr = formals } :: body -> sLambda envs cont (formals :: body) |> define' var
        | x -> x |> invalidParameter "'%s' invalid define parameter."

    [<TailCall>]
    let rec bindDefineValues envs cont formals =
        function
        | [] -> formals |> cont
        | (var, expr) :: xs ->
            expr
            |> Eval.eval envs (fun value ->
                Context.defineEnvVar envs var value
                xs |> bindDefineValues envs cont formals)

    let sDefineValues envs cont =
        function
        | [ formals; expr ] ->
            expr
            |> Eval.eval envs (fun result ->
                let vals =
                    match result with
                    | SValues vs -> vs
                    | x -> [ x ]

                formals
                |> zipFormalsRef vals
                |> List.map (fun (v, r) -> v, r.Value)
                |> bindDefineValues envs cont formals)
        | x -> x |> invalidParameter "'%s' invalid define-values parameter."

    let sDefineRecordType envs cont =
        function
        | SSymbol name :: SPair { car = SSymbol ctorName
                                  cdr = ctorFieldsExpr } :: SSymbol predName :: restSpecs ->
            let defineVal var valExpr = Context.defineEnvVar envs var valExpr
            let typeId = Context.getNextRecordTypeId envs
            let ctorFields = ctorFieldsExpr |> toList

            let fieldSpecs =
                restSpecs
                |> List.map (function
                    | SPair { car = SSymbol fName
                              cdr = SPair { car = SSymbol aName; cdr = rest } } ->
                        let mName =
                            match rest with
                            | SPair { car = SSymbol m; cdr = SEmpty } -> Some m
                            | _ -> None

                        fName, aName, mName
                    | x -> x |> invalid "Invalid record field spec: %s")

            let fieldNames = fieldSpecs |> List.map (fun (n, _, _) -> n)
            let fieldCount = fieldNames.Length

            let predProc _ cont' =
                function
                | [ SRecord(tid, _, _) ] -> tid = typeId |> toSBool |> cont'
                | _ -> SFalse |> cont'

            defineVal predName (SProcedure predProc)

            let ctorProc _ cont' (args: SExpression list) =
                if args.Length <> ctorFields.Length then
                    failwithf "%s requires %d arguments, but got %d" ctorName ctorFields.Length args.Length

                let recordFields = Array.init fieldCount (fun _ -> ref SUnspecified)

                List.zip ctorFields args
                |> List.iter (fun (fExpr, v) ->
                    let fName =
                        match fExpr with
                        | SSymbol s -> s
                        | _ -> failwith "not symbol"

                    let idx = fieldNames |> List.findIndex ((=) fName)
                    recordFields.[idx].Value <- v)

                SRecord(typeId, name, recordFields) |> cont'

            defineVal ctorName (SProcedure ctorProc)

            fieldSpecs
            |> List.iteri (fun idx (_, aName, mNameOpt) ->
                let accessorProc _ cont' =
                    function
                    | [ SRecord(tid, _, fs) ] when tid = typeId -> fs.[idx].Value |> cont'
                    | [ x ] -> x |> Print.print |> failwithf "Accessor %s expected %s, but got %s" aName name
                    | _ -> failwithf "Accessor %s requires 1 argument" aName

                defineVal aName (SProcedure accessorProc)

                mNameOpt
                |> Option.iter (fun mName ->
                    let modifierProc _ cont' =
                        function
                        | [ SRecord(tid, _, fs); v ] when tid = typeId ->
                            fs.[idx].Value <- v
                            SUnspecified |> cont'
                        | [ x; _ ] -> x |> Print.print |> failwithf "Modifier %s expected %s, but got %s" mName name
                        | _ -> failwithf "Modifier %s requires 2 arguments" mName

                    defineVal mName (SProcedure modifierProc)))

            name |> SSymbol |> cont
        | x -> x |> invalidParameter "'%s' invalid define-record-type parameter."
