namespace WriteScheme

open Repl

module Program =
    [<EntryPoint>]
    let main argv =
        "Welcome" |> repl (newContext (argv |> Array.toList))
        0
