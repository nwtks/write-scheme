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

    [<TailCall>]
    let rec loopDiffWinders sList tList lenS lenT accS accT =
        if lenS > lenT then
            match sList with
            | hd :: tl -> loopDiffWinders tl tList (lenS - 1) lenT (hd :: accS) accT
            | [] -> failwith "unreachable"
        elif lenT > lenS then
            match tList with
            | hd :: tl -> loopDiffWinders sList tl lenS (lenT - 1) accS (hd :: accT)
            | [] -> failwith "unreachable"
        else
            match sList, tList with
            | [], [] -> List.rev accS, List.rev accT
            | h1 :: _, h2 :: _ when h1.Id = h2.Id -> List.rev accS, List.rev accT
            | h1 :: t1, h2 :: t2 -> loopDiffWinders t1 t2 (lenS - 1) (lenT - 1) (h1 :: accS) (h2 :: accT)
            | _ -> List.rev accS, List.rev accT

    let diffWinders src tgt =
        loopDiffWinders src tgt (List.length src) (List.length tgt) [] []

    [<TailCall>]
    let rec runWindLeaves envs cont cur =
        function
        | [] -> cont cur
        | head :: rest ->
            let nextCur =
                match cur with
                | h :: t when h.Id = head.Id -> t
                | _ -> cur

            currentWinders.Value <- nextCur

            head.After
            |> Eval.apply envs (fun _ -> rest |> runWindLeaves envs cont nextCur) []

    [<TailCall>]
    let rec runWindEnters envs cont cur =
        function
        | [] -> cont cur
        | head :: rest ->
            head.Before
            |> Eval.apply
                envs
                (fun _ ->
                    let nextCur = head :: cur
                    currentWinders.Value <- nextCur
                    rest |> runWindEnters envs cont nextCur)
                []

    let doWind envs src tgt next =
        let leaves, enters = diffWinders src tgt
        let entersRev = List.rev enters

        leaves
        |> runWindLeaves envs (fun cur -> entersRev |> runWindEnters envs next cur) src
