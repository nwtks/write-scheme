namespace WriteScheme

open Type

module Eval =
    [<TailCall>]
    let rec apply envs cont args =
        function
        | SParameter(param, converterOpt), pos ->
            match args with
            | [] -> param.Value |> Ok |> cont
            | [ v ] ->
                match converterOpt with
                | Some converter ->
                    converter
                    |> apply
                        envs
                        (Result.map (fun converted ->
                            param.Value <- converted
                            converted)
                         >> cont)
                        [ v ]
                | None ->
                    param.Value <- v
                    v |> Ok |> cont
            | _ ->
                EvalError(sprintf "'%s' invalid parameter object call." (args |> toSPair |> Print.print), pos)
                |> Error
        | SSyntax fn, pos
        | SProcedure fn, pos -> args |> fn envs pos cont
        | SContinuation fn, pos ->
            match args with
            | [ arg ] -> Ok arg |> fn
            | _ ->
                EvalError(sprintf "'%s' invalid continuation parameter." (args |> toSPair |> Print.print), pos)
                |> Error
        | x -> EvalError(sprintf "'%s' not operator." (x |> Print.print), snd x) |> Error

    [<TailCall>]
    let rec eval envs cont =
        function
        | SEmpty, pos -> EvalError("() is not a valid expression.", pos) |> Error
        | SUnspecified, _
        | SBool _, _
        | SRational _, _
        | SReal _, _
        | SComplex _, _
        | SString _, _
        | SChar _, _
        | SVector _, _
        | SByteVector _, _
        | SValues _, _
        | SRecord _, _
        | SError _, _
        | SUnquote _, _
        | SUnquoteSplicing _, _
        | SDatumLabel _, _
        | SDatumRef _, _
        | SPromise _, _
        | SParameter _, _
        | SSyntax _, _
        | SProcedure _, _
        | SContinuation _, _ as expr -> expr |> Ok |> cont
        | SSymbol x, pos -> x |> Context.lookupEnvs envs pos |> Result.map (fun v -> v.Value) |> cont
        | SPair p, _ ->
            p.car
            |> eval envs (function
                | Ok(SSyntax fn, pos') ->
                    p.cdr
                    |> toList
                    |> function
                        | Ok args -> args |> fn envs pos' cont
                        | Error e -> Error e |> cont
                | Ok op ->
                    p.cdr
                    |> toList
                    |> function
                        | Ok args -> args |> evalArgs envs cont (fun e c a -> op |> apply e c a) []
                        | Error e -> Error e |> cont
                | x -> x |> cont)
        | SQuote x, pos -> [ SSymbol "quote", pos; x ] |> toSPair |> eval envs cont
        | SQuasiquote x, pos -> [ SSymbol "quasiquote", pos; x ] |> toSPair |> eval envs cont

    and [<TailCall>] evalArgs envs cont fn acc =
        function
        | [] -> acc |> List.rev |> fn envs cont
        | x :: xs ->
            x
            |> eval envs (function
                | Ok(SValues _, p) -> EvalError("Multiple values in single value context.", p) |> Error |> cont
                | Ok a -> xs |> evalArgs envs cont fn (a :: acc)
                | x -> x |> cont)

    [<TailCall>]
    let rec eachEval envs cont acc =
        function
        | [] -> acc |> cont
        | x :: xs ->
            x
            |> eval envs (function
                | Ok a -> xs |> eachEval envs cont (Ok a)
                | x -> x |> cont)

    [<TailCall>]
    let rec getVariablesFromFormals acc =
        function
        | SEmpty, _ -> acc |> List.rev
        | SSymbol v, _ -> v :: acc |> List.rev
        | SPair { car = SSymbol v, _; cdr = rest }, _ -> rest |> getVariablesFromFormals (v :: acc)
        | _ -> acc |> List.rev

    let getDefinedVars =
        function
        | SPair { car = SSymbol "define", _
                  cdr = SPair { car = body; cdr = _ }, _ },
          _ ->
            match body with
            | SSymbol var, _ -> [ var ]
            | SPair { car = SSymbol var, _; cdr = _ }, _ -> [ var ]
            | _ -> []
        | SPair { car = SSymbol "define-values", _
                  cdr = SPair { car = formals; cdr = _ }, _ },
          _ -> formals |> getVariablesFromFormals []
        | _ -> []

    [<TailCall>]
    let rec collectInternalDefinitions acc =
        function
        | [] -> List.rev acc, []
        | [] :: stack -> stack |> collectInternalDefinitions acc
        | (head :: rest) :: stack ->
            match head with
            | SPair { car = SSymbol "define", _; cdr = _ }, _
            | SPair { car = SSymbol "define-values", _
                      cdr = _ },
              _ -> rest :: stack |> collectInternalDefinitions (head :: acc)
            | SPair { car = SSymbol "begin", _
                      cdr = inner },
              _ ->
                match inner |> toList with
                | Ok ilist -> ilist :: rest :: stack |> collectInternalDefinitions acc
                | Error _ -> List.rev acc, head :: rest @ List.concat stack
            | _ -> List.rev acc, head :: rest @ List.concat stack

    let isDefinition =
        function
        | SPair { car = SSymbol "define", _; cdr = _ }, _ -> true
        | SPair { car = SSymbol "define-values", _
                  cdr = _ },
          _ -> true
        | _ -> false

    let evalBody envs cont acc body =
        let defs, exprs = [ body ] |> collectInternalDefinitions []

        match exprs |> List.tryFind isDefinition with
        | Some(_, pos) ->
            EvalError("Definitions must appear at the beginning of a body.", pos)
            |> Error
            |> cont
        | None ->
            if List.isEmpty defs then
                body |> eachEval envs cont acc
            else if List.isEmpty exprs then
                EvalError("Internal definitions must be followed by at least one expression.", snd (List.last defs))
                |> Error
                |> cont
            else
                let vars = defs |> List.collect getDefinedVars |> List.distinct

                let envs' =
                    vars
                    |> List.map (fun v -> v, ref (SUnspecified, None))
                    |> Context.extendEnvs envs

                defs
                |> eachEval
                    envs'
                    (function
                    | Ok _ -> exprs |> eachEval envs' cont acc
                    | x -> x |> cont)
                    (Ok(SUnspecified, None))
