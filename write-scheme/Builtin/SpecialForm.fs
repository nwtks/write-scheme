namespace WriteScheme.Builtins

open WriteScheme
open Type

[<AutoOpen>]
module SpecialForm =
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

    let sQuote context pos cont =
        function
        | [ datum ] -> Ok datum |> cont
        | x -> x |> invalidParameter pos "'%s' invalid quote parameter." |> cont

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
            let setVariable value =
                variable
                |> Context.lookupEnvironments context pos'
                |> Result.map (fun v ->
                    v.Value <- value
                    value)

            expression |> Eval.eval context (Result.bind setVariable) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid set! parameter." |> cont

    [<TailCall>]
    let rec sIncludeFiles foldCase context pos cont acc =
        function
        | [] ->
            match acc |> List.rev with
            | [] -> Ok(SUnspecified, pos) |> cont
            | expressions -> expressions |> Eval.eachEval context cont (Ok(SUnspecified, pos))
        | (SString f, p) :: rest ->
            match readAndResolveInclude foldCase f p with
            | Ok resolvedExpressions ->
                rest
                |> sIncludeFiles foldCase context pos cont (List.rev resolvedExpressions @ acc)
            | Error e -> Error e |> cont
        | x :: _ -> [ x ] |> invalidParameter pos "'%s' invalid include parameter." |> cont

    let sInclude context pos cont files =
        files |> sIncludeFiles false context pos cont []

    let sIncludeCi context pos cont files =
        files |> sIncludeFiles true context pos cont []

    let sBegin context pos cont =
        Eval.eachEval context cont (Ok(SUnspecified, pos))

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
