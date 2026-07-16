namespace WriteScheme.Builtins

open WriteScheme
open Type

[<AutoOpen>]
module Conditional =
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

    let supportedFeatures () =
        [ "r7rs"
          "exact-closed"
          "exact-rational"
          "ieee-float"
          "full-unicode"
          "ratios"
          if System.OperatingSystem.IsWindows() then
              "windows"
          if System.OperatingSystem.IsLinux() then
              "linux"
              "unix"
              "posix"
          if System.BitConverter.IsLittleEndian then
              "little-endian"
          else
              "big-endian"
          let arch = System.Runtime.InteropServices.RuntimeInformation.ProcessArchitecture

          if arch = System.Runtime.InteropServices.Architecture.X64 then
              "x86-64"
          elif arch = System.Runtime.InteropServices.Architecture.Arm64 then
              "arm64" ]
        |> Set.ofList

    [<TailCall>]
    let rec checkFeatureRequirement context pos negated =
        function
        | SSymbol feature, _ -> supportedFeatures () |> Set.contains feature <> negated
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

    let parseDoBindings bindings =
        bindings |> toList |> Result.bind (mapResult parseDoBinding)

    let sDo context pos cont =
        function
        | bindings :: testClause :: commands ->
            match testClause with
            | SPair { car = test; cdr = expressions }, _ ->
                match bindings |> parseDoBindings with
                | Ok bindings' ->
                    match expressions |> toList with
                    | Ok elist -> bindings' |> initDoVariables context pos cont bindings' test elist commands []
                    | Error e -> Error e |> cont
                | Error e -> Error e |> cont
            | _ -> [ testClause ] |> invalidParameter pos "'%s' invalid do test clause." |> cont
        | x -> x |> invalidParameter pos "'%s' invalid do parameter." |> cont

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
