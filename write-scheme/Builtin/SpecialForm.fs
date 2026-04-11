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
    let rec bindArgs envs cont body acc =
        function
        | [] -> body |> eachEval (List.rev acc |> Eval.extendEnvs envs) cont SEmpty
        | (var, expr) :: xs ->
            expr
            |> Eval.eval envs (fun a -> xs |> bindArgs envs cont body ((var, ref a) :: acc))

    and closure captureEnvs formals body envs cont args =
        formals |> zipFormals args |> bindArgs (envs @ captureEnvs) cont body []

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
                (Eval.lookupEnvs envs var).Value <- x
                x |> cont)
        | x -> x |> invalidParameter "'%s' invalid set! parameter."

    [<TailCall>]
    let rec sCond envs cont =
        function
        | [] -> SEmpty |> cont
        | [ SList(SSymbol "else" :: exprs) ] -> exprs |> eachEval envs cont SEmpty
        | SList [ test; SSymbol "=>"; expr ] :: clauses ->
            test
            |> testCond envs cont (fun a -> [ expr; SQuote a ] |> toSList |> Eval.eval envs cont) clauses
        | SList(test :: exprs) :: clauses -> test |> testCond envs cont (fun a -> exprs |> eachEval envs cont a) clauses
        | x -> x |> invalidParameter "'%s' invalid cond parameter."

    and testCond envs cont conseq clauses =
        Eval.eval envs (function
            | SBool false -> clauses |> sCond envs cont
            | x -> conseq x)

    [<TailCall>]
    let rec testCase envs cont key =
        function
        | [] -> SEmpty |> cont
        | [ SList [ SSymbol "else"; SSymbol "=>"; expr ] ] -> [ expr; SQuote key ] |> toSList |> Eval.eval envs cont
        | [ SList(SSymbol "else" :: exprs) ] -> exprs |> eachEval envs cont SEmpty
        | SList [ SList datums; SSymbol "=>"; expr ] :: clauses ->
            if datums |> List.exists (fun d -> eqv (key, d)) then
                [ expr; SQuote key ] |> toSList |> Eval.eval envs cont
            else
                clauses |> testCase envs cont key
        | SList(SList datums :: exprs) :: clauses ->
            if datums |> List.exists (fun d -> eqv (key, d)) then
                exprs |> eachEval envs cont SEmpty
            else
                clauses |> testCase envs cont key
        | x -> x |> invalidParameter "'%s' invalid case parameter."

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
                | _ -> exprs |> eachEval envs cont SEmpty)
        | x -> x |> invalidParameter "'%s' invalid when parameter."

    let sUnless envs cont =
        function
        | test :: exprs ->
            test
            |> Eval.eval envs (function
                | SBool false -> exprs |> eachEval envs cont SEmpty
                | _ -> SEmpty |> cont)
        | x -> x |> invalidParameter "'%s' invalid unless parameter."

    [<TailCall>]
    let rec bindLet envs cont body acc =
        function
        | [] -> body |> eachEval (List.rev acc |> Eval.extendEnvs envs) cont SEmpty
        | (var, expr) :: xs ->
            expr
            |> Eval.eval envs (fun a -> xs |> bindLet envs cont body ((var, ref a) :: acc))

    let sLet envs cont =
        function
        | SList bindings :: body -> bindings |> List.map eachBinding |> bindLet envs cont body []
        | x -> x |> invalidParameter "'%s' invalid let parameter."

    let eachValuesBinding =
        function
        | SList [ SList formals; expr ] ->
            let vars =
                formals
                |> List.map (function
                    | SSymbol v -> v
                    | x -> Print.print x |> sprintf "'%s' not symbol." |> failwith)

            vars, expr
        | x -> Print.print x |> sprintf "'%s' invalid let-values binding." |> failwith

    [<TailCall>]
    let rec bindLetStar envs cont body =
        function
        | [] -> body |> eachEval envs cont SEmpty
        | (var, expr) :: xs ->
            expr
            |> Eval.eval envs (fun a -> xs |> bindLetStar ([ var, ref a ] |> Eval.extendEnvs envs) cont body)

    let sLetStar envs cont =
        function
        | SList bindings :: body -> bindings |> List.map eachBinding |> bindLetStar envs cont body
        | x -> x |> invalidParameter "'%s' invalid let* parameter."

    [<TailCall>]
    let rec bindLetRecExpr envs cont body =
        function
        | [] -> body |> eachEval envs cont SEmpty
        | (var, expr) :: xs ->
            expr
            |> Eval.eval envs (fun a ->
                envs.Head.[var].Value <- a
                xs |> bindLetRecExpr envs cont body)

    let sLetRec envs cont =
        let bindRef bindings =
            bindings
            |> List.map (function
                | var, _ -> var, ref SEmpty)
            |> Eval.extendEnvs envs

        function
        | SList bindings :: body ->
            let bindings' = bindings |> List.map eachBinding
            bindings' |> bindLetRecExpr (bindings' |> bindRef) cont body
        | x -> x |> invalidParameter "'%s' invalid letrec parameter."

    [<TailCall>]
    let rec bindLetRecStarExpr envs cont body =
        function
        | [], _
        | _, [] -> body |> eachEval envs cont SEmpty
        | (_, expr) :: xs, r: SExpression ref :: rs ->
            expr
            |> Eval.eval envs (fun v ->
                r.Value <- v
                (xs, rs) |> bindLetRecStarExpr envs cont body)

    let sLetRecStar envs cont =
        let eachRef (envs', refs) =
            function
            | var, _ ->
                let r = ref SEmpty
                [ var, r ] |> Eval.extendEnvs envs', r :: refs

        let bindRef bindings =
            let envs', refs = bindings |> List.fold eachRef (envs, [])
            envs', List.rev refs

        function
        | SList bindings :: body ->
            let bindings' = bindings |> List.map eachBinding
            let envs', refs = bindRef bindings'
            (bindings', refs) |> bindLetRecStarExpr envs' cont body
        | x -> x |> invalidParameter "'%s' invalid letrec* parameter."

    [<TailCall>]
    let rec bindLetValues envs cont body acc =
        function
        | [] -> body |> eachEval (List.rev acc |> Eval.extendEnvs envs) cont SEmpty
        | (vars: string list, expr) :: xs ->
            expr
            |> Eval.eval envs (fun v ->
                let vals =
                    match v with
                    | SValues vs -> vs
                    | single -> [ single ]

                let bindings = List.zip vars vals |> List.map (fun (vr, vl) -> vr, ref vl)
                xs |> bindLetValues envs cont body (bindings @ acc))

    let sLetValues envs cont =
        function
        | SList bindings :: body -> bindings |> List.map eachValuesBinding |> bindLetValues envs cont body []
        | x -> x |> invalidParameter "'%s' invalid let-values parameter."

    [<TailCall>]
    let rec bindLetStarValues envs cont body =
        function
        | [] -> body |> eachEval envs cont SEmpty
        | (vars: string list, expr) :: xs ->
            expr
            |> Eval.eval envs (fun v ->
                let vals =
                    match v with
                    | SValues vs -> vs
                    | single -> [ single ]

                let bindings = List.zip vars vals |> List.map (fun (vr, vl) -> vr, ref vl)
                let nextEnvs = bindings |> Eval.extendEnvs envs
                xs |> bindLetStarValues nextEnvs cont body)

    let sLetStarValues envs cont =
        function
        | SList bindings :: body -> bindings |> List.map eachValuesBinding |> bindLetStarValues envs cont body
        | x -> x |> invalidParameter "'%s' invalid let*-values parameter."

    let sBegin envs cont = eachEval envs cont SEmpty

    [<TailCall>]
    let rec loopDo envs cont test exprs commands bindings loopEnvs =
        test
        |> Eval.eval loopEnvs (function
            | SBool false ->
                commands
                |> eachEval
                    loopEnvs
                    (fun _ -> bindings |> evalDoStep envs cont test exprs commands bindings loopEnvs [])
                    SEmpty
            | testResult ->
                match exprs with
                | [] -> SEmpty |> cont
                | _ -> exprs |> eachEval loopEnvs cont testResult)

    and evalDoStep envs cont test exprs commands bindings loopEnvs acc =
        function
        | [] ->
            let nextEnvs = Eval.extendEnvs envs (List.rev acc)
            loopDo envs cont test exprs commands bindings nextEnvs
        | (var, _, Some step) :: xs ->
            step
            |> Eval.eval loopEnvs (fun v ->
                xs
                |> evalDoStep envs cont test exprs commands bindings loopEnvs ((var, ref v) :: acc))
        | (var, _, None) :: xs ->
            let v = (Eval.lookupEnvs loopEnvs var).Value

            xs
            |> evalDoStep envs cont test exprs commands bindings loopEnvs ((var, ref v) :: acc)

    [<TailCall>]
    let rec initDoVariables envs cont test exprs commands bindings acc =
        function
        | [] ->
            let initialEnvs = Eval.extendEnvs envs (List.rev acc)
            loopDo envs cont test exprs commands bindings initialEnvs
        | (var, init, _) :: xs ->
            init
            |> Eval.eval envs (fun v ->
                xs
                |> initDoVariables envs cont test exprs commands bindings ((var, ref v) :: acc))

    let sDo envs cont =
        let parseBinding =
            function
            | SList [ SSymbol var; init; step ] -> var, init, Some step
            | SList [ SSymbol var; init ] -> var, init, None
            | x -> [ x ] |> invalidParameter "'%s' invalid do binding parameter."

        function
        | SList bindings :: SList(test :: exprs) :: commands ->
            let bindings' = bindings |> List.map parseBinding
            bindings' |> initDoVariables envs cont test exprs commands bindings' []
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

    let eachParamBinding =
        function
        | SList [ param; expr ] -> param, expr
        | x -> Print.print x |> sprintf "'%s' invalid parameterize binding." |> failwith

    [<TailCall>]
    let rec bindParameterize envs cont body saved acc =
        function
        | [] ->
            let savedValues = List.rev saved

            try
                body |> eachEval envs cont SEmpty
            finally
                savedValues |> List.iter (fun (r: SExpression ref, old) -> r.Value <- old)
        | (param, expr) :: xs ->
            param
            |> Eval.eval envs (fun p ->
                expr
                |> Eval.eval envs (fun v ->
                    match p with
                    | SParameter(r, converterOpt) ->
                        match converterOpt with
                        | Some converter ->
                            converter
                            |> Eval.apply
                                envs
                                (fun converted -> setAndContinue envs cont body saved acc xs r converted)
                                [ v ]
                        | None -> setAndContinue envs cont body saved acc xs r v
                    | x -> Print.print x |> sprintf "'%s' is not a parameter object." |> failwith))

    and setAndContinue envs cont body saved acc xs (r: SExpression ref) newVal =
        let old = r.Value
        r.Value <- newVal
        xs |> bindParameterize envs cont body ((r, old) :: saved) acc

    let sParameterize envs cont =
        function
        | SList bindings :: body -> bindings |> List.map eachParamBinding |> bindParameterize envs cont body [] []
        | x -> x |> invalidParameter "'%s' invalid parameterize parameter."

    let sGuard envs cont =
        function
        | SList(SSymbol var :: clauses) :: body ->
            try
                body |> eachEval envs cont SEmpty
            with SchemeRaise obj ->
                let envs' = Eval.extendEnvs envs [ var, ref obj ]

                let hasElse =
                    match List.tryLast clauses with
                    | Some(SList(SSymbol "else" :: _)) -> true
                    | _ -> false

                let finalClauses =
                    if hasElse then
                        clauses
                    else
                        clauses @ [ SList [ SSymbol "else"; SList [ SSymbol "raise"; SQuote obj ] ] ]

                finalClauses |> sCond envs' cont
        | x -> x |> invalidParameter "'%s' invalid guard parameter."

    [<TailCall>]
    let rec replaceQuasiquote envs cont n next =
        function
        | SEmpty -> SEmpty |> next
        | SList xs -> xs |> replaceQuasiquoteList envs cont n |> next
        | SPair(x1, x2) ->
            match x1 |> replaceQuasiquoteList envs cont n with
            | SList ys -> SPair(ys, x2 |> replaceQuasiquoteDatum envs cont n) |> next
            | _ -> x2 |> replaceQuasiquoteDatum envs cont n |> next
        | x -> x |> replaceQuasiquoteDatum envs cont n |> next

    and replaceQuasiquoteList envs cont n =
        let cons x =
            function
            | SEmpty -> [ x ] |> toSList
            | SList ys -> x :: ys |> toSList
            | y -> [ x; y ] |> toSList

        let join =
            function
            | SEmpty, SEmpty -> SEmpty
            | SList xs, SEmpty -> xs |> toSList
            | SEmpty, SList ys -> ys |> toSList
            | SList xs, SList ys -> xs @ ys |> toSList
            | x, y -> [ x; y ] |> toSList

        function
        | [] -> SEmpty
        | SUnquote x :: xs
        | SList [ SSymbol "unquote"; x ] :: xs ->
            if n = 0 then
                x
                |> Eval.eval envs (fun a -> xs |> toSList |> replaceQuasiquote envs cont n (fun b -> cons a b |> cont))
            else
                x
                |> replaceQuasiquote envs cont (n - 1) (fun a ->
                    xs |> toSList |> replaceQuasiquote envs cont n (fun b -> cons (SUnquote a) b))
        | SUnquoteSplicing x :: xs
        | SList [ SSymbol "unquote-splicing"; x ] :: xs ->
            if n = 0 then
                x
                |> Eval.eval envs (fun a ->
                    xs |> toSList |> replaceQuasiquote envs cont n (fun b -> join (a, b) |> cont))
            else
                x
                |> replaceQuasiquote envs cont (n - 1) (fun a ->
                    xs
                    |> toSList
                    |> replaceQuasiquote envs cont n (fun b -> cons (SUnquoteSplicing a) b))
        | SQuasiquote x :: xs
        | SList [ SSymbol "quasiquote"; x ] :: xs ->
            x
            |> replaceQuasiquote envs cont (n + 1) (fun a ->
                xs |> toSList |> replaceQuasiquote envs cont n (fun b -> cons (SQuasiquote a) b))
        | SQuote x :: xs
        | SList [ SSymbol "quote"; x ] :: xs ->
            x
            |> replaceQuasiquote envs cont n (fun a ->
                xs |> toSList |> replaceQuasiquote envs cont n (fun b -> cons (SQuote a) b))
        | x :: xs ->
            x
            |> replaceQuasiquote envs cont n (fun a ->
                xs |> toSList |> replaceQuasiquote envs cont n (fun b -> cons a b))

    and replaceQuasiquoteDatum envs cont n =
        function
        | SUnquote x
        | SList [ SSymbol "unquote"; x ] ->
            if n = 0 then
                x |> Eval.eval envs cont
            else
                x |> replaceQuasiquote envs cont (n - 1) SUnquote
        | SUnquoteSplicing x
        | SList [ SSymbol "unquote-splicing"; x ] ->
            if n = 0 then
                x |> Eval.eval envs cont
            else
                x |> replaceQuasiquote envs cont (n - 1) SUnquoteSplicing
        | SQuasiquote x
        | SList [ SSymbol "quasiquote"; x ] -> x |> replaceQuasiquote envs cont (n + 1) SQuasiquote
        | SQuote x
        | SList [ SSymbol "quote"; x ] -> x |> replaceQuasiquote envs cont n SQuote
        | x -> x

    let sQuasiquote envs cont =
        function
        | [ x ] -> x |> replaceQuasiquote envs cont 0 id
        | x -> x |> invalidParameter "'%s' invalid quasiquote parameter."

    let sDefine (envs: SEnv list) cont =
        let define' var =
            envs.Head.TryAdd(var, ref SEmpty) |> ignore

            Eval.eval envs (fun x ->
                envs.Head.[var].Value <- x
                var |> SSymbol |> cont)

        function
        | [ SSymbol var; expr ] -> expr |> define' var
        | SList(SSymbol var :: formals) :: body -> sLambda envs cont (SList formals :: body) |> define' var
        | SPair([ SSymbol var ], formal) :: body -> sLambda envs cont (formal :: body) |> define' var
        | x -> x |> invalidParameter "'%s' invalid define parameter."

    [<TailCall>]
    let rec bindDefineValues envs cont formals =
        function
        | [] -> formals |> cont
        | (var, expr) :: xs ->
            expr
            |> Eval.eval envs (fun value ->
                if envs.Head.ContainsKey var then
                    envs.Head.[var].Value <- value
                else
                    envs.Head.Add(var, ref value)

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

                formals |> zipFormals vals |> bindDefineValues envs cont formals)
        | x -> x |> invalidParameter "'%s' invalid define-values parameter."

    let sDefineRecordType (envs: SEnv list) cont =
        let defineVal var valExpr =
            if envs.Head.ContainsKey var then
                envs.Head.[var].Value <- valExpr
            else
                envs.Head.Add(var, ref valExpr)

        function
        | SSymbol name :: SList(SSymbol ctorName :: ctorFields) :: SSymbol predName :: restSpecs ->
            let typeId = Type.getNextRecordTypeId ()

            let fieldSpecs =
                restSpecs
                |> List.map (function
                    | SList(SSymbol fName :: SSymbol aName :: rest) ->
                        let mName =
                            match rest with
                            | [ SSymbol m ] -> Some m
                            | _ -> None

                        fName, aName, mName
                    | x -> failwithf "Invalid record field spec: %s" (Print.print x))

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
                    | [ SRecord(tid, _, fs) ] when tid = typeId -> (fs: SExpression ref array).[idx].Value |> cont'
                    | [ x ] -> failwithf "Accessor %s expected %s, but got %s" aName name (Print.print x)
                    | _ -> failwithf "Accessor %s requires 1 argument" aName

                defineVal aName (SProcedure accessorProc)

                mNameOpt
                |> Option.iter (fun mName ->
                    let modifierProc _ cont' =
                        function
                        | [ SRecord(tid, _, fs); v ] when tid = typeId ->
                            (fs: SExpression ref array).[idx].Value <- v
                            SUnspecified |> cont'
                        | [ x; _ ] -> failwithf "Modifier %s expected %s, but got %s" mName name (Print.print x)
                        | _ -> failwithf "Modifier %s requires 2 arguments" mName

                    defineVal mName (SProcedure modifierProc)))

            name |> SSymbol |> cont
        | x -> x |> invalidParameter "'%s' invalid define-record-type parameter."
