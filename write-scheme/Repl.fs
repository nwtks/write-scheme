namespace WriteScheme

module Repl =
    let rep envs =
        Read.read >> Eval.eval envs id >> Print.print

    let newEnvs () = Context.extendEnvs Builtin.builtin []

    [<TailCall>]
    let rec repl envs output =
        printf "%s\n> " output
        let line = System.Console.ReadLine()
        if isNull line then () else line |> rep envs |> repl envs

    let runRepl () =
        let envs = newEnvs ()

        try
            "Welcome" |> repl envs
        with x ->
            Context.setWinders envs []
            x.Message |> repl envs
