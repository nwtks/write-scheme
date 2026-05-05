namespace WriteScheme

open Repl

module Program =
    [<EntryPoint>]
    let main argv =
        "Welcome" |> repl (newEnvs ())
        0 // return an integer exit code
