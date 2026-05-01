namespace WriteScheme.Builtins

open WriteScheme
open Type

[<AutoOpen>]
module Helper =
    let invalid pos fmt =
        Print.print
        >> sprintf fmt
        >> fun msg -> failwithf "%s%s" msg (pos |> formatPosition)

    let invalidParameter pos fmt = toSPair >> invalid pos fmt

    let eachBinding =
        function
        | SPair { car = SSymbol var, _
                  cdr = SPair { car = expr; cdr = SEmpty, _ }, _ },
          _ -> var, expr
        | _, pos as x -> x |> invalid pos "'%s' invalid binding."

    [<TailCall>]
    let rec eqv =
        function
        | (SBool a, _), (SBool b, _) -> a = b
        | (SRational(a1, a2), _), (SRational(b1, b2), _) -> a1 = b1 && a2 = b2
        | (SReal a', _), (SReal b', _) -> a' = b'
        | (SComplex a', _), (SComplex b', _) -> a' = b'
        | (SChar a', _), (SChar b', _) -> a' = b'
        | (SSymbol a', _), (SSymbol b', _) -> a' = b'
        | (SQuote a, _), (SQuote b, _) -> (a, b) |> eqv
        | (SUnquote a, _), (SUnquote b, _) -> (a, b) |> eqv
        | (a, _), (b, _) -> a = b

    [<TailCall>]
    let rec loopDiffWinders sList tList lenS lenT accS accT =
        if lenS > lenT then
            match sList with
            | hd :: tl -> loopDiffWinders tl tList (lenS - 1) lenT (hd :: accS) accT
            | [] -> failwith "unreachable."
        elif lenT > lenS then
            match tList with
            | hd :: tl -> loopDiffWinders sList tl lenS (lenT - 1) accS (hd :: accT)
            | [] -> failwith "unreachable."
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
