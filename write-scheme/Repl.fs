namespace WriteScheme

open Type

module Repl =
    let rep envs =
        Read.read
        >> Result.bind Eval.resolveLabels
        >> Result.map (Eval.eval envs id)
        >> Result.map Print.print
        >> Result.defaultWith (fun e ->
            Context.setWinders envs []

            match e with
            | ParseError(msg, pos) -> sprintf "%s%s" msg (pos |> formatPosition)
            | EvalError(msg, pos) -> sprintf "%s%s" msg (pos |> formatPosition))

    let newEnvs () = Context.extendEnvs Builtin.builtin []

    [<TailCall>]
    let rec repl envs output =
        printf "%s\n> " output
        let line = System.Console.ReadLine()
        if isNull line then () else line |> rep envs |> repl envs

    let runRepl () =
        let envs = newEnvs ()
        "Welcome" |> repl envs
