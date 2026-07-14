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
