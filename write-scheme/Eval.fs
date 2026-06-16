namespace WriteScheme

open Type

module Eval =
    [<TailCall>]
    let rec apply context cont args =
        function
        | SParameter(param, converter), pos -> args |> applyParameter context cont param converter pos
        | SSyntax fn, pos
        | SProcedure fn, pos -> args |> fn context pos cont
        | SContinuation fn, pos ->
            match args with
            | [ arg ] -> Ok arg |> fn
            | _ -> (SValues args, pos) |> Ok |> fn
        | x -> EvalError(sprintf "'%s' not operator." (x |> Print.print), snd x) |> Error

    and [<TailCall>] applyParameter context cont (param: SExpression ref) converterOpt pos =
        function
        | [] -> param.Value |> Ok |> cont
        | [ v ] ->
            match converterOpt with
            | Some converter ->
                converter
                |> apply
                    context
                    (Result.map (fun converted ->
                        param.Value <- converted
                        converted)
                     >> cont)
                    [ v ]
            | None ->
                param.Value <- v
                v |> Ok |> cont
        | x ->
            EvalError(sprintf "'%s' invalid parameter object call." (x |> toSPair |> Print.print), pos)
            |> Error

    [<TailCall>]
    let rec eval context cont =
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
        | SSymbol x, pos ->
            x
            |> Context.lookupEnvironments context pos
            |> Result.map (fun v -> v.Value)
            |> cont
        | SPair pair, _ -> pair |> evalPair context cont
        | SQuote x, pos -> [ SSymbol "quote", pos; x ] |> toSPair |> eval context cont
        | SQuasiquote x, pos -> [ SSymbol "quasiquote", pos; x ] |> toSPair |> eval context cont

    and [<TailCall>] evalPair context cont pair =
        pair.car
        |> eval context (function
            | Ok(SSyntax fn, pos) ->
                pair.cdr
                |> toList
                |> function
                    | Ok args -> args |> fn context pos cont
                    | Error e -> Error e |> cont
            | Ok op ->
                pair.cdr
                |> toList
                |> function
                    | Ok args -> args |> evalArgs context cont (fun e c a -> op |> apply e c a) []
                    | Error e -> Error e |> cont
            | x -> x |> cont)

    and [<TailCall>] evalArgs context cont fn acc =
        function
        | [] -> acc |> List.rev |> fn context cont
        | x :: xs ->
            x
            |> eval context (function
                | Ok(SValues _, p) -> EvalError("Multiple values in single value context.", p) |> Error |> cont
                | Ok a -> xs |> evalArgs context cont fn (a :: acc)
                | x -> x |> cont)

    [<TailCall>]
    let rec eachEval context cont acc =
        function
        | [] -> acc |> cont
        | x :: xs ->
            x
            |> eval context (function
                | Ok a -> xs |> eachEval context cont (Ok a)
                | x -> x |> cont)

    [<TailCall>]
    let rec getVariablesFromFormals acc =
        function
        | SEmpty, _ -> acc |> List.rev
        | SSymbol var, _ -> var :: acc |> List.rev
        | SPair { car = SSymbol var, _; cdr = rest }, _ -> rest |> getVariablesFromFormals (var :: acc)
        | _ -> acc |> List.rev

    let getDefineVar body =
        match body with
        | SSymbol var, _ -> [ var ]
        | SPair { car = SSymbol var, _; cdr = _ }, _ -> [ var ]
        | _ -> []

    let getDefinedVariables =
        function
        | SPair { car = SSymbol "define", _
                  cdr = SPair { car = body; cdr = _ }, _ },
          _ -> body |> getDefineVar
        | SPair { car = SSymbol "define-values", _
                  cdr = SPair { car = formals; cdr = _ }, _ },
          _ -> formals |> getVariablesFromFormals []
        | _ -> []

    [<TailCall>]
    let rec collectInternalDefinitions acc =
        function
        | [] -> acc |> List.rev, []
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
                | Error _ -> acc |> List.rev, head :: rest @ List.concat stack
            | _ -> acc |> List.rev, head :: rest @ List.concat stack

    let isDefinition =
        function
        | SPair { car = SSymbol "define", _; cdr = _ }, _ -> true
        | SPair { car = SSymbol "define-values", _
                  cdr = _ },
          _ -> true
        | _ -> false

    let evalBody context cont acc body =
        let definitions, expressions = [ body ] |> collectInternalDefinitions []

        match expressions |> List.tryFind isDefinition with
        | Some(_, pos) ->
            EvalError("Definitions must appear at the beginning of a body.", pos)
            |> Error
            |> cont
        | None ->
            if definitions |> List.isEmpty then
                body |> eachEval context cont acc
            else if expressions |> List.isEmpty then
                EvalError(
                    "Internal definitions must be followed by at least one expression.",
                    definitions |> List.last |> snd
                )
                |> Error
                |> cont
            else
                let context' =
                    definitions
                    |> List.collect getDefinedVariables
                    |> List.distinct
                    |> List.map (fun var -> var, ref (SUnspecified, None))
                    |> Context.extendEnvironments context

                definitions
                |> eachEval
                    context'
                    (function
                    | Ok _ -> expressions |> eachEval context' cont acc
                    | x -> x |> cont)
                    (Ok(SUnspecified, None))
