namespace WriteScheme

open Repl

module Program =
    [<EntryPoint>]
    let main argv =
        repl ()
        0 // return an integer exit code
