namespace WriteScheme.Builtins

open WriteScheme
open Type

[<AutoOpen>]
module SpecialForm =
    let sQuote envs cont =
        function
        | [ x ] -> x |> cont
        | x -> x |> invalidParameter "'%s' invalid quote parameter."

    let zipFormals args =
        let zipVarArg vars args' =
            let varsLen = List.length vars
            let argsLen = List.length args'

            if argsLen < varsLen then
                sprintf "%d parameters requires, but %d." varsLen argsLen |> failwith

            List.zip vars (args' |> List.take varsLen)
            |> List.map (function
                | SSymbol var, expr -> var, expr
                | x, _ -> Print.print x |> sprintf "'%s' not symbol." |> failwith)

        let argsExpr =
            function
            | [] -> SEmpty
            | [ x ] -> x
            | xs -> xs |> toSList |> SQuote

        function
        | SSymbol var -> [ var, args |> argsExpr ]
        | SEmpty -> []
        | SList vars -> zipVarArg vars args
        | SPair(vars, SSymbol var) ->
            let varsLen = List.length vars

            zipVarArg vars (args |> List.take varsLen)
            @ [ var, args |> List.skip varsLen |> argsExpr ]
        | x -> Print.print x |> sprintf "'%s' not symbol." |> failwith

    [<TailCall>]
    let rec bindArgs envs cont body acc =
        function
        | [] -> body |> Eval.eachEval (acc |> List.rev |> Eval.extendEnvs envs) cont SEmpty
        | (var, expr) :: xs ->
            expr
            |> Eval.eval envs (fun a -> xs |> bindArgs envs cont body ((var, ref a) :: acc))

    and [<TailCall>] closure captureEnvs formals body envs cont args =
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
        | [ SList(SSymbol "else" :: exprs) ] -> exprs |> Eval.eachEval envs cont SEmpty
        | SList [ test; SSymbol "=>"; expr ] :: clauses ->
            testCond envs cont (fun a -> [ expr; SQuote a ] |> toSList |> Eval.eval envs cont) clauses test
        | SList(test :: exprs) :: clauses ->
            testCond envs cont (fun a -> exprs |> Eval.eachEval envs cont a) clauses test
        | x -> x |> invalidParameter "'%s' invalid cond parameter."

    and [<TailCall>] testCond envs cont conseq clauses test =
        test
        |> Eval.matchEval envs (function
            | SBool false -> sCond envs cont clauses
            | x -> conseq x)

    [<TailCall>]
    let rec testCase envs cont key =
        function
        | [] -> SEmpty |> cont
        | [ SList [ SSymbol "else"; SSymbol "=>"; expr ] ] -> [ expr; SQuote key ] |> toSList |> Eval.eval envs cont
        | [ SList(SSymbol "else" :: exprs) ] -> exprs |> Eval.eachEval envs cont SEmpty
        | SList [ SList datums; SSymbol "=>"; expr ] :: clauses ->
            if datums |> List.exists (fun d -> eqv (key, d)) then
                [ expr; SQuote key ] |> toSList |> Eval.eval envs cont
            else
                clauses |> testCase envs cont key
        | SList(SList datums :: exprs) :: clauses ->
            if datums |> List.exists (fun d -> eqv (key, d)) then
                exprs |> Eval.eachEval envs cont SEmpty
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
        | [] -> body |> Eval.eachEval (acc |> List.rev |> Eval.extendEnvs envs) cont SEmpty
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
        | [] -> body |> Eval.eachEval envs cont SEmpty
        | (var, expr) :: xs ->
            expr
            |> Eval.eval envs (fun v -> xs |> bindLetStar ([ var, ref v ] |> Eval.extendEnvs envs) cont body)

    let sLetStar envs cont =
        function
        | SList bindings :: body -> bindings |> List.map eachBinding |> bindLetStar envs cont body
        | x -> x |> invalidParameter "'%s' invalid let* parameter."

    [<TailCall>]
    let rec bindLetRecExpr envs cont body =
        function
        | [] -> body |> Eval.eachEval envs cont SEmpty
        | (var, expr) :: xs ->
            expr
            |> Eval.eval envs (fun a ->
                Eval.defineEnvVar envs var a
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
        | _, [] -> body |> Eval.eachEval envs cont SEmpty
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
        | [] -> body |> Eval.eachEval (acc |> List.rev |> Eval.extendEnvs envs) cont SEmpty
        | (vars, expr) :: xs ->
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
        | [] -> body |> Eval.eachEval envs cont SEmpty
        | (vars, expr) :: xs ->
            expr
            |> Eval.eval envs (fun v ->
                let vals =
                    match v with
                    | SValues vs -> vs
                    | single -> [ single ]

                let nextEnvs =
                    List.zip vars vals
                    |> List.map (fun (vr, vl) -> vr, ref vl)
                    |> Eval.extendEnvs envs

                xs |> bindLetStarValues nextEnvs cont body)

    let sLetStarValues envs cont =
        function
        | SList bindings :: body -> bindings |> List.map eachValuesBinding |> bindLetStarValues envs cont body
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
            |> Eval.extendEnvs envs
            |> loopDo envs cont test exprs commands bindings
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
            acc
            |> List.rev
            |> Eval.extendEnvs envs
            |> loopDo envs cont test exprs commands bindings
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

    type SavedParameter =
        { Ref: SExpression ref
          SavedValue: SExpression ref }

    let eachParamBinding =
        function
        | SList [ param; expr ] -> param, expr
        | x -> Print.print x |> sprintf "'%s' invalid parameterize binding." |> failwith

    [<TailCall>]
    let rec bindParameterize envs cont body saved acc =
        function
        | [] ->
            let id = nextWinderId.Value
            nextWinderId.Value <- id + 1
            let savedRev = List.rev saved

            let swapThunk =
                SProcedure(fun _ cont' _ ->
                    savedRev
                    |> List.iter (fun s ->
                        let tmp = s.Ref.Value
                        s.Ref.Value <- s.SavedValue.Value
                        s.SavedValue.Value <- tmp)

                    SUnspecified |> cont')

            let winder =
                { Id = id
                  Before = swapThunk
                  After = swapThunk }

            currentWinders.Value <- winder :: currentWinders.Value

            body
            |> Eval.eachEval
                envs
                (fun res ->
                    let nextCur =
                        match currentWinders.Value with
                        | h :: t when h.Id = id -> t
                        | xs -> xs

                    currentWinders.Value <- nextCur
                    swapThunk |> Eval.apply envs (fun _ -> cont res) [])
                SEmpty
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

    and [<TailCall>] setAndContinue envs cont body saved acc xs r newVal =
        let old = r.Value
        r.Value <- newVal
        let s = { Ref = r; SavedValue = ref old }
        xs |> bindParameterize envs cont body (s :: saved) acc

    let sParameterize envs cont =
        function
        | SList bindings :: body -> bindings |> List.map eachParamBinding |> bindParameterize envs cont body [] []
        | x -> x |> invalidParameter "'%s' invalid parameterize parameter."

    let sGuard envs cont =
        function
        | SList(SSymbol var :: clauses) :: body ->
            let savedWinders = currentWinders.Value

            try
                body |> Eval.eachEval envs cont SEmpty
            with SchemeRaise obj ->
                let hasElse =
                    match List.tryLast clauses with
                    | Some(SList(SSymbol "else" :: _)) -> true
                    | _ -> false

                let finalClauses =
                    if hasElse then
                        clauses
                    else
                        clauses
                        @ [ toSList [ SSymbol "else"; toSList [ SSymbol "raise"; SQuote obj ] ] ]

                doWind envs currentWinders.Value savedWinders (fun _ ->
                    let envs' = [ var, ref obj ] |> Eval.extendEnvs envs
                    finalClauses |> sCond envs' cont)
        | x -> x |> invalidParameter "'%s' invalid guard parameter."

    [<TailCall>]
    let rec replaceQuasiquote envs cont n next =
        function
        | SEmpty -> SEmpty |> next
        | SList xs -> xs |> replaceQuasiquoteList envs cont n next
        | SPair(x1, x2) ->
            x1
            |> replaceQuasiquoteList envs cont n (function
                | SList ys -> x2 |> replaceQuasiquoteDatum envs cont n (fun y2 -> SPair(ys, y2) |> next)
                | _ -> x2 |> replaceQuasiquoteDatum envs cont n next)
        | x -> x |> replaceQuasiquoteDatum envs cont n next

    and [<TailCall>] replaceQuasiquoteList envs cont n next xs =
        let cons x b =
            match b with
            | SEmpty -> [ x ] |> toSList
            | SList ys -> x :: ys |> toSList
            | y -> SPair([ x ], y)

        let join a b =
            match a, b with
            | SEmpty, SEmpty -> SEmpty
            | SList xs, SEmpty -> toSList xs
            | SEmpty, SList ys -> toSList ys
            | SList xs, SList ys -> xs @ ys |> toSList
            | x, y -> SPair([ x ], y)

        match xs with
        | [] -> next SEmpty
        | SUnquote x :: rest
        | SList [ SSymbol "unquote"; x ] :: rest ->
            if n = 0 then
                x
                |> Eval.matchEval envs (fun a -> rest |> replaceQuasiquoteList envs cont n (fun b -> cons a b |> next))
            else
                x
                |> replaceQuasiquote envs cont (n - 1) (fun a ->
                    rest |> replaceQuasiquoteList envs cont n (fun b -> cons (SUnquote a) b |> next))
        | SUnquoteSplicing x :: rest
        | SList [ SSymbol "unquote-splicing"; x ] :: rest ->
            if n = 0 then
                x
                |> Eval.matchEval envs (fun a -> rest |> replaceQuasiquoteList envs cont n (fun b -> join a b |> next))
            else
                x
                |> replaceQuasiquote envs cont (n - 1) (fun a ->
                    rest
                    |> replaceQuasiquoteList envs cont n (fun b -> cons (SUnquoteSplicing a) b |> next))
        | SQuasiquote x :: rest
        | SList [ SSymbol "quasiquote"; x ] :: rest ->
            x
            |> replaceQuasiquote envs cont (n + 1) (fun a ->
                rest
                |> replaceQuasiquoteList envs cont n (fun b -> cons (SQuasiquote a) b |> next))
        | SQuote x :: rest
        | SList [ SSymbol "quote"; x ] :: rest ->
            x
            |> replaceQuasiquote envs cont n (fun a ->
                rest |> replaceQuasiquoteList envs cont n (fun b -> cons (SQuote a) b |> next))
        | x :: rest ->
            x
            |> replaceQuasiquote envs cont n (fun a ->
                rest |> replaceQuasiquoteList envs cont n (fun b -> cons a b |> next))

    and [<TailCall>] replaceQuasiquoteDatum envs cont n next =
        function
        | SUnquote x
        | SList [ SSymbol "unquote"; x ] ->
            if n = 0 then
                x |> Eval.matchEval envs next
            else
                x |> replaceQuasiquote envs cont (n - 1) (SUnquote >> next)
        | SUnquoteSplicing x
        | SList [ SSymbol "unquote-splicing"; x ] ->
            if n = 0 then
                x |> Eval.matchEval envs next
            else
                x |> replaceQuasiquote envs cont (n - 1) (SUnquoteSplicing >> next)
        | SQuasiquote x
        | SList [ SSymbol "quasiquote"; x ] -> x |> replaceQuasiquote envs cont (n + 1) (SQuasiquote >> next)
        | SQuote x
        | SList [ SSymbol "quote"; x ] -> x |> replaceQuasiquote envs cont n (SQuote >> next)
        | x -> x |> next

    let sQuasiquote envs cont =
        function
        | [ x ] -> x |> replaceQuasiquote envs cont 0 cont
        | x -> x |> invalidParameter "'%s' invalid quasiquote parameter."

    let sDefine (envs: SEnv list) cont =
        let define' var =
            Eval.eval envs (fun x ->
                Eval.defineEnvVar envs var x
                var |> SSymbol |> cont)

        function
        | [ SSymbol var; expr ] -> expr |> define' var
        | SList(SSymbol var :: formals) :: body -> sLambda envs cont (toSList formals :: body) |> define' var
        | SPair([ SSymbol var ], formal) :: body -> sLambda envs cont (formal :: body) |> define' var
        | x -> x |> invalidParameter "'%s' invalid define parameter."

    [<TailCall>]
    let rec bindDefineValues envs cont formals =
        function
        | [] -> formals |> cont
        | (var, expr) :: xs ->
            expr
            |> Eval.eval envs (fun value ->
                Eval.defineEnvVar envs var value
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

    let private nextRecordTypeId = new System.Threading.ThreadLocal<int>(fun () -> 0)

    let getNextRecordTypeId () =
        let id = nextRecordTypeId.Value
        nextRecordTypeId.Value <- id + 1
        id

    let sDefineRecordType (envs: SEnv list) cont =
        function
        | SSymbol name :: SList(SSymbol ctorName :: ctorFields) :: SSymbol predName :: restSpecs ->
            let defineVal var valExpr = Eval.defineEnvVar envs var valExpr
            let typeId = getNextRecordTypeId ()

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
                    | [ SRecord(tid, _, fs) ] when tid = typeId -> fs.[idx].Value |> cont'
                    | [ x ] -> failwithf "Accessor %s expected %s, but got %s" aName name (Print.print x)
                    | _ -> failwithf "Accessor %s requires 1 argument" aName

                defineVal aName (SProcedure accessorProc)

                mNameOpt
                |> Option.iter (fun mName ->
                    let modifierProc _ cont' =
                        function
                        | [ SRecord(tid, _, fs); v ] when tid = typeId ->
                            fs.[idx].Value <- v
                            SUnspecified |> cont'
                        | [ x; _ ] -> failwithf "Modifier %s expected %s, but got %s" mName name (Print.print x)
                        | _ -> failwithf "Modifier %s requires 2 arguments" mName

                    defineVal mName (SProcedure modifierProc)))

            name |> SSymbol |> cont
        | x -> x |> invalidParameter "'%s' invalid define-record-type parameter."
