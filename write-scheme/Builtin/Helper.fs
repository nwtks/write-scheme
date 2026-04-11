namespace WriteScheme.Builtins

open WriteScheme
open Type

[<AutoOpen>]
module Helper =
    let invalidParameter fmt =
        toSList >> Print.print >> sprintf fmt >> failwith

    [<TailCall>]
    let rec eachEval envs cont acc =
        function
        | [] -> acc |> cont
        | x :: xs -> x |> Eval.eval envs (fun a -> xs |> eachEval envs cont a)

    let zipFormals args =
        let zipVarArg vars args' =
            let varsLen = List.length vars
            let argsLen = List.length args'

            if argsLen < varsLen then
                sprintf "%d parameters requires, but %d." varsLen argsLen |> failwith

            List.zip vars (args' |> List.take varsLen)
            |> List.map (function
                | SSymbol var, expr -> var, expr
                | x, _ -> Print.print x |> sprintf "'%s' not symbol." |> failwith)

        let argsExpr =
            function
            | [] -> SEmpty
            | [ x ] -> x
            | xs -> xs |> SList |> SQuote

        function
        | SSymbol var -> [ var, args |> argsExpr ]
        | SEmpty -> []
        | SList vars -> zipVarArg vars args
        | SPair(vars, SSymbol var) ->
            let varsLen = List.length vars

            zipVarArg vars (args |> List.take varsLen)
            @ [ var, args |> List.skip varsLen |> argsExpr ]
        | x -> Print.print x |> sprintf "'%s' not symbol." |> failwith

    let eachBinding =
        function
        | SList [ SSymbol var; expr ] -> var, expr
        | x -> Print.print x |> sprintf "'%s' not symbol." |> failwith
