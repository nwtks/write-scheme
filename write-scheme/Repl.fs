namespace WriteScheme

open Type

module Repl =
    let formatPosition =
        function
        | Some pos -> sprintf " (at line %d, column %d)" pos.line pos.column
        | None -> ""

    let rep context =
        Read.read false
        >> Result.bind DatumLabel.resolveLabels
        >> Result.bind (Eval.eval context id)
        >> Result.map Print.print
        >> Result.defaultWith (fun e ->
            context |> Context.reset

            match e with
            | ParseError(msg, pos) -> sprintf "%s%s" msg (pos |> formatPosition)
            | EvalError(msg, pos) -> sprintf "%s%s" msg (pos |> formatPosition)
            | SchemeRaise(expr, pos) -> sprintf "%s%s" (expr |> Print.print) (pos |> formatPosition))

    let newContext () =
        let context = Builtin.builtinContext

        { context with
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
