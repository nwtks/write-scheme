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
