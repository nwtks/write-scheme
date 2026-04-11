namespace WriteScheme

module Repl =
    let rep envs =
        Read.read >> Eval.eval envs id >> Print.print

    let newEnvs () = Eval.extendEnvs Builtin.builtin []

    [<TailCall>]
    let rec repl' envs output =
        printf "%s\n> " output
        let line = System.Console.ReadLine()
        if isNull line then () else line |> rep envs |> repl' envs

    let repl () =
        let envs = newEnvs ()

        try
            repl' envs "Welcome"
        with x ->
            WriteScheme.Builtins.Helper.currentWinders.Value <- []
            x.Message |> repl' envs
