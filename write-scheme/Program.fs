namespace WriteScheme

open Repl

module Program =
    [<EntryPoint>]
    let main argv =
        runRepl ()
        0 // return an integer exit code
