namespace WriteScheme

open Repl

module Program =
    [<EntryPoint>]
    let main argv =
        "Welcome" |> repl (newContext ())
        0
