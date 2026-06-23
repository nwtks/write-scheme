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
        | x -> EvalError($"'{x |> Print.print}' not operator.", snd x) |> Error

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
            EvalError($"'{x |> toSPair |> Print.print}' invalid parameter object call.", pos)
            |> Error

    let evalSyntaxArgs context pos cont fn =
        function
        | Ok args -> args |> fn context pos cont
        | Error e -> Error e |> cont

    [<TailCall>]
    let rec eval context cont =
        function
        | SEmpty, pos -> EvalError("() is not a valid expression.", pos) |> Error
        | SSymbol x, pos ->
            x
            |> Context.lookupEnvironments context pos
            |> Result.map (fun v -> v.Value)
            |> cont
        | SPair pair, _ -> pair |> evalPair context cont
        | SQuote x, pos -> [ SSymbol "quote", pos; x ] |> toSPair |> eval context cont
        | SQuasiquote x, pos -> [ SSymbol "quasiquote", pos; x ] |> toSPair |> eval context cont
        | expr -> expr |> Ok |> cont

    and [<TailCall>] evalProcedureArgs context cont op =
        function
        | Ok args -> args |> evalArgs context cont (fun e c a -> op |> apply e c a) []
        | Error e -> Error e |> cont

    and [<TailCall>] evalPair context cont pair =
        pair.car
        |> eval context (function
            | Ok(SSyntax fn, pos) -> pair.cdr |> toList |> evalSyntaxArgs context pos cont fn
            | Ok op -> pair.cdr |> toList |> evalProcedureArgs context cont op
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
    let rec expandBeginInBody acc =
        function
        | [] -> acc |> List.rev
        | SPair { car = SSymbol "begin", _
                  cdr = inner },
          _ as expr :: rest ->
            match inner |> toList with
            | Ok ilist -> ilist @ rest |> expandBeginInBody acc
            | Error _ -> rest |> expandBeginInBody (expr :: acc)
        | expr :: rest -> rest |> expandBeginInBody (expr :: acc)

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
            | _ -> acc |> List.rev, head :: rest @ List.concat stack

    let isDefinition =
        function
        | SPair { car = SSymbol "define", _; cdr = _ }, _ -> true
        | SPair { car = SSymbol "define-values", _
                  cdr = _ },
          _ -> true
        | _ -> false

    let validateBodyStructure definitions expressions =
        match expressions |> List.tryFind isDefinition with
        | Some(_, pos) -> Error("Definitions must appear at the beginning of a body.", pos)
        | None ->
            if definitions |> List.isEmpty then
                Ok None
            elif expressions |> List.isEmpty then
                Error(
                    "Internal definitions must be followed by at least one expression.",
                    definitions |> List.last |> snd
                )
            else
                Ok(Some(definitions, expressions))

    let prepareDefinitionContext context definitions =
        definitions
        |> List.collect getDefinedVariables
        |> List.distinct
        |> List.map (fun var -> var, ref (SUnspecified, None))
        |> Context.extendEnvironments context

    let evalDefinitionsAndBody context cont acc definitions expressions =
        let context' = definitions |> prepareDefinitionContext context

        definitions
        |> eachEval
            context'
            (function
            | Ok _ -> expressions |> eachEval context' cont acc
            | x -> x |> cont)
            (Ok(SUnspecified, None))

    let evalBody context cont acc body =
        let expandedBody = body |> expandBeginInBody []
        let definitions, expressions = [ expandedBody ] |> collectInternalDefinitions []

        match validateBodyStructure definitions expressions with
        | Ok None -> body |> eachEval context cont acc
        | Ok(Some(defs, exprs)) -> evalDefinitionsAndBody context cont acc defs exprs
        | Error(msg, pos) -> EvalError(msg, pos) |> Error |> cont
