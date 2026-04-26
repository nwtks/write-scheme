namespace WriteScheme.Builtins

open WriteScheme
open Type

[<AutoOpen>]
module Helper =
    let invalid fmt = Print.print >> sprintf fmt >> failwith
    let invalidParameter fmt = toSPair >> invalid fmt

    let eachBinding =
        function
        | SPair { car = SSymbol var
                  cdr = SPair { car = expr; cdr = SEmpty } } -> var, expr
        | x -> x |> invalid "'%s' invalid binding."

    [<TailCall>]
    let rec eqv =
        function
        | SBool a, SBool b -> a = b
        | SRational(a1, a2), SRational(b1, b2) -> a1 = b1 && a2 = b2
        | SReal a, SReal b -> a = b
        | SComplex a, SComplex b -> a = b
        | SChar a, SChar b -> a = b
        | SSymbol a, SSymbol b -> a = b
        | SQuote a, SQuote b -> (a, b) |> eqv
        | SUnquote a, SUnquote b -> (a, b) |> eqv
        | (a, _), (b, _) -> a = b

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
            | h1 :: _, h2 :: _ when h1.id = h2.id -> List.rev accS, List.rev accT
            | h1 :: t1, h2 :: t2 -> loopDiffWinders t1 t2 (lenS - 1) (lenT - 1) (h1 :: accS) (h2 :: accT)
            | _ -> List.rev accS, List.rev accT

    [<TailCall>]
    let rec runWindLeaves envs cont cur =
        function
        | [] -> cont cur
        | head :: rest ->
            let nextCur = Context.leaveWinder envs cur head.id

            head.after
            |> Eval.apply envs (fun _ -> rest |> runWindLeaves envs cont nextCur) []

    [<TailCall>]
    let rec runWindEnters envs cont cur =
        function
        | [] -> cont cur
        | head :: rest ->
            head.before
            |> Eval.apply
                envs
                (fun _ ->
                    let nextCur = Context.enterWinder envs cur head
                    rest |> runWindEnters envs cont nextCur)
                []

    let doWind envs cont tgt =
        let src = envs.currentWinders.Value

        let leaves, enters =
            loopDiffWinders src tgt (List.length src) (List.length tgt) [] []

        let entersRev = List.rev enters

        leaves
        |> runWindLeaves envs (fun cur -> entersRev |> runWindEnters envs cont cur) src
