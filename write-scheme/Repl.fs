namespace WriteScheme

module Repl =
    let rep envs =
        Read.read >> Eval.evalWrapped envs id >> Print.print

    let newEnvs () = Context.extendEnvs Builtin.builtin []

    [<TailCall>]
    let rec replInner envs output =
        printf "%s\n> " output
        let line = System.Console.ReadLine()

        if isNull line then
            ()
        else
            line |> rep envs |> replInner envs

    let repl () =
        let envs = newEnvs ()

        try
            replInner envs "Welcome"
        with x ->
            Context.setWinders envs []
            x.Message |> replInner envs
