namespace WriteScheme.Builtins

open WriteScheme
open Type

[<AutoOpen>]
module Environment =
    [<TailCall>]
    let rec loopEnvironment context pos cont freshEnv =
        function
        | [] -> Ok(SEnvironment freshEnv, pos) |> cont
        | spec :: rest ->
            spec
            |> processImportSet context pos (function
                | Ok bindings ->
                    bindings
                    |> Map.iter (fun name refVal -> freshEnv.Value <- freshEnv.Value |> Map.add name refVal)

                    rest |> loopEnvironment context pos cont freshEnv
                | Error e -> Error e |> cont)

    let sEnvironment context pos cont specs =
        let freshEnv = ref Map.empty
        specs |> loopEnvironment context pos cont freshEnv

    let sSchemeReportEnvironment context pos cont =
        function
        | [ SRational(n, d), _ ] when d = 1I ->
            if n = 5I then
                match context.libraries.Value |> Map.tryFind "(scheme r5rs)" with
                | Some lib -> Ok(SEnvironment lib.environment, pos) |> cont
                | None ->
                    EvalError("scheme-report-environment: library (scheme r5rs) not found.", pos)
                    |> Error
                    |> cont
            else
                EvalError($"scheme-report-environment: only version 5 is supported, got {n}.", pos)
                |> Error
                |> cont
        | [ other, _ ] ->
            EvalError(
                $"scheme-report-environment: argument must be an exact integer, got {Print.print (other, pos)}.",
                pos
            )
            |> Error
            |> cont
        | x ->
            let msg =
                match x with
                | [] -> "scheme-report-environment: missing argument."
                | _ -> $"scheme-report-environment: expected 1 argument, got {List.length x}."

            EvalError(msg, pos) |> Error |> cont

    let sNullEnvironment context pos cont =
        function
        | [ SRational(n, d), _ ] when d = 1I ->
            if n = 5I then
                Ok(SEnvironment(ref Map.empty), pos) |> cont
            else
                EvalError($"null-environment: only version 5 is supported, got {n}.", pos)
                |> Error
                |> cont
        | [ other, _ ] ->
            EvalError($"null-environment: argument must be an exact integer, got {Print.print (other, pos)}.", pos)
            |> Error
            |> cont
        | x ->
            let msg =
                match x with
                | [] -> "null-environment: missing argument."
                | _ -> $"null-environment: expected 1 argument, got {List.length x}."

            EvalError(msg, pos) |> Error |> cont

    let sInteractionEnvironment context pos cont =
        function
        | [] ->
            let combined =
                context.environments
                |> List.rev
                |> List.collect (fun env -> env.Value |> Map.toList)
                |> Map.ofList
                |> ref

            Ok(SEnvironment combined, pos) |> cont
        | x ->
            x
            |> invalidParameter pos "'%s' invalid interaction-environment parameter."
            |> cont

    let sEval context pos cont =
        function
        | [ datum; SEnvironment env, _ ] ->
            let evalContext = { context with environments = [ env ] }
            datum |> Eval.eval evalContext cont
        | [ _; other, _ ] ->
            EvalError($"eval: second argument must be an environment object, got {Print.print (other, pos)}.", pos)
            |> Error
            |> cont
        | x ->
            let msg =
                match x with
                | [ _ ] -> "eval: missing environment argument."
                | _ -> $"eval: expected 2 arguments, got {List.length x}."

            EvalError(msg, pos) |> Error |> cont
