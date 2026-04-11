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

    [<TailCall>]
    let rec eqv =
        function
        | SBool a, SBool b -> a = b
        | SRational(a1, a2), SRational(b1, b2) -> a1 = b1 && a2 = b2
        | SReal a, SReal b -> a = b
        | SChar a, SChar b -> a = b
        | SSymbol a, SSymbol b -> a = b
        | SQuote a, SQuote b -> (a, b) |> eqv
        | SUnquote a, SUnquote b -> (a, b) |> eqv
        | a, b -> a = b

    [<TailCall>]
    let rec loopEqual =
        function
        | [] -> true
        | (a, b) :: xs ->
            match a, b with
            | SList la, SList lb ->
                if la.Length <> lb.Length then
                    false
                else
                    List.zip la lb @ xs |> loopEqual
            | SPair(la, ra), SPair(lb, rb) ->
                if la.Length <> lb.Length then
                    false
                else
                    List.zip la lb @ (ra, rb) :: xs |> loopEqual
            | SQuote a', SQuote b' -> (a', b') :: xs |> loopEqual
            | SUnquote a', SUnquote b' -> (a', b') :: xs |> loopEqual
            | SBool a', SBool b' -> a' = b' && loopEqual xs
            | SRational(a1, a2), SRational(b1, b2) -> a1 = b1 && a2 = b2 && loopEqual xs
            | SReal a', SReal b' -> a' = b' && loopEqual xs
            | SString a', SString b' -> a' = b' && loopEqual xs
            | SChar a', SChar b' -> a' = b' && loopEqual xs
            | SSymbol a', SSymbol b' -> a' = b' && loopEqual xs
            | a', b' -> a' = b' && loopEqual xs

    let equal (a, b) = [ a, b ] |> loopEqual
