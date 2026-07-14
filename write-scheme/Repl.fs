namespace WriteScheme

open Type

module Repl =
    let formatPosition =
        function
        | Some pos -> $" (at line {pos.line}, column {pos.column})"
        | None -> ""

    let rep context =
        Read.read false
        >> Result.bind DatumLabel.resolveLabels
        >> Result.bind (Eval.eval context id)
        >> Result.map Print.print
        >> Result.defaultWith (fun e ->
            context |> Context.reset

            match e with
            | ParseError(msg, pos) -> $"{msg}{pos |> formatPosition}"
            | EvalError(msg, pos) -> $"{msg}{pos |> formatPosition}"
            | SchemeRaise(expr, pos) -> $"{expr |> Print.print}{pos |> formatPosition}")

    let newContext argv =
        let context = Builtin.builtinContext

        { context with
            commandLineArgs = argv
            environments = (Map.empty |> ref) :: context.environments
            winders = ref []
            handlers = ref Context.initialHandlers
            nextWinderId = ref 0 }

    [<TailCall>]
    let rec repl context output =
        printf "%s\n> " output
        let line = System.Console.ReadLine()

        if isNull line then
            ()
        else
            line |> rep context |> repl context
