namespace WriteScheme.Builtins

open WriteScheme
open Type

[<AutoOpen>]
module SpecialForm =
    let sQuote envs cont =
        function
        | [ x ] -> x |> cont
        | x -> x |> invalidParameter "'%s' invalid quote parameter."

    [<TailCall>]
    let rec bindArgs body envs' cont' acc =
        function
        | [] -> body |> eachEval (List.rev acc |> Eval.extendEnvs envs') cont' SEmpty
        | (var, expr) :: xs ->
            expr
            |> Eval.eval envs' (fun a -> xs |> bindArgs body envs' cont' ((var, ref a) :: acc))

    and closure envs formals body envs' cont' args =
        formals |> zipFormals args |> bindArgs body (envs' @ envs) cont' []

    let sLambda envs cont =
        function
        | formals :: body -> closure envs formals body |> SSyntax |> cont
        | x -> x |> invalidParameter "'%s' invalid lambda parameter."

    let sIf envs cont =
        let if' test conseq alter =
            test
            |> Eval.eval envs (function
                | SBool false -> alter |> Eval.eval envs cont
                | _ -> conseq |> Eval.eval envs cont)

        function
        | [ test; conseq; alter ] -> if' test conseq alter
        | [ test; conseq ] -> if' test conseq SEmpty
        | x -> x |> invalidParameter "'%s' invalid if parameter."

    let sSet envs cont =
        function
        | [ SSymbol var; expr ] ->
            expr
            |> Eval.eval envs (fun x ->
                (Eval.lookupEnvs envs var).Value <- x
                x |> cont)
        | x -> x |> invalidParameter "'%s' invalid set! parameter."

    [<TailCall>]
    let rec sCond envs cont =
        function
        | [] -> SEmpty |> cont
        | [ SList(SSymbol "else" :: exprs) ] -> exprs |> eachEval envs cont SEmpty
        | SList [ test; SSymbol "=>"; expr ] :: clauses ->
            test
            |> sCondTest envs cont (fun a -> [ expr; SQuote a ] |> toSList |> Eval.eval envs cont) clauses
        | SList(test :: exprs) :: clauses ->
            test |> sCondTest envs cont (fun a -> exprs |> eachEval envs cont a) clauses
        | x -> x |> invalidParameter "'%s' invalid cond parameter."

    and sCondTest envs cont conseq clauses =
        Eval.eval envs (function
            | SBool false -> clauses |> sCond envs cont
            | x -> conseq x)

    [<TailCall>]
    let rec sAnd envs cont =
        function
        | [] -> STrue |> cont
        | [ test ] ->
            test
            |> Eval.eval envs (function
                | SBool false -> SFalse |> cont
                | x -> x |> cont)
        | test :: tests ->
            test
            |> Eval.eval envs (function
                | SBool false -> SFalse |> cont
                | _ -> tests |> sAnd envs cont)

    [<TailCall>]
    let rec sOr envs cont =
        function
        | [] -> SFalse |> cont
        | test :: tests ->
            test
            |> Eval.eval envs (function
                | SBool false -> tests |> sOr envs cont
                | x -> x |> cont)

    let sWhen envs cont =
        function
        | test :: exprs ->
            test
            |> Eval.eval envs (function
                | SBool false -> SEmpty |> cont
                | _ -> exprs |> eachEval envs cont SEmpty)
        | x -> x |> invalidParameter "'%s' invalid when parameter."

    let sUnless envs cont =
        function
        | test :: exprs ->
            test
            |> Eval.eval envs (function
                | SBool false -> exprs |> eachEval envs cont SEmpty
                | _ -> SEmpty |> cont)
        | x -> x |> invalidParameter "'%s' invalid unless parameter."

    [<TailCall>]
    let rec bindLet envs cont body acc =
        function
        | [] -> body |> eachEval (List.rev acc |> Eval.extendEnvs envs) cont SEmpty
        | (var, expr) :: xs ->
            expr
            |> Eval.eval envs (fun a -> xs |> bindLet envs cont body ((var, ref a) :: acc))

    let sLet envs cont =
        function
        | SList bindings :: body -> bindings |> List.map eachBinding |> bindLet envs cont body []
        | x -> x |> invalidParameter "'%s' invalid let parameter."

    [<TailCall>]
    let rec bindLetStar cont body envs' =
        function
        | [] -> body |> eachEval envs' cont SEmpty
        | (var, expr) :: xs ->
            expr
            |> Eval.eval envs' (fun a -> xs |> bindLetStar cont body ([ var, ref a ] |> Eval.extendEnvs envs'))

    let sLetStar envs cont =
        function
        | SList bindings :: body -> bindings |> List.map eachBinding |> bindLetStar cont body envs
        | x -> x |> invalidParameter "'%s' invalid let* parameter."

    [<TailCall>]
    let rec bindLetRecExpr cont body envs' =
        function
        | [] -> body |> eachEval envs' cont SEmpty
        | (var, expr) :: xs ->
            expr
            |> Eval.eval envs' (fun a ->
                envs'.Head.[var].Value <- a
                xs |> bindLetRecExpr cont body envs')

    let sLetRec envs cont =
        let bindRef bindings =
            bindings
            |> List.map (function
                | var, _ -> var, ref SEmpty)
            |> Eval.extendEnvs envs

        function
        | SList bindings :: body ->
            let bindings' = bindings |> List.map eachBinding
            bindings' |> bindLetRecExpr cont body (bindings' |> bindRef)
        | x -> x |> invalidParameter "'%s' invalid letrec parameter."

    [<TailCall>]
    let rec bindLetRecStarExpr cont body envs' =
        function
        | [], _
        | _, [] -> body |> eachEval envs' cont SEmpty
        | (_, expr) :: xs, r: SExpression ref :: rs ->
            expr
            |> Eval.eval envs' (fun a ->
                r.Value <- a
                (xs, rs) |> bindLetRecStarExpr cont body envs')

    let sLetRecStar envs cont =
        let eachRef (envs', refs) =
            function
            | var, _ ->
                let r = ref SEmpty
                [ var, r ] |> Eval.extendEnvs envs', r :: refs

        let bindRef bindings =
            let envs', refs = bindings |> List.fold eachRef (envs, [])
            envs', List.rev refs

        function
        | SList bindings :: body ->
            let bindings' = bindings |> List.map eachBinding
            let envs', refs = bindRef bindings'
            (bindings', refs) |> bindLetRecStarExpr cont body envs'
        | x -> x |> invalidParameter "'%s' invalid letrec* parameter."

    let sBegin envs cont = eachEval envs cont SEmpty

    [<TailCall>]
    let rec replaceQuasiquote envs cont n next =
        function
        | SEmpty -> SEmpty |> next
        | SList xs -> replaceQuasiquoteList envs cont n xs |> next
        | SPair(x1, x2) ->
            match replaceQuasiquoteList envs cont n x1 with
            | SList ys -> SPair(ys, replaceQuasiquoteDatum envs cont n x2) |> next
            | _ -> replaceQuasiquoteDatum envs cont n x2 |> next
        | x -> replaceQuasiquoteDatum envs cont n x |> next

    and replaceQuasiquoteList envs cont n =
        let cons x =
            function
            | SEmpty -> [ x ] |> toSList
            | SList ys -> x :: ys |> toSList
            | y -> [ x; y ] |> toSList

        let join =
            function
            | SEmpty, SEmpty -> SEmpty
            | SList xs, SEmpty -> xs |> toSList
            | SEmpty, SList ys -> ys |> toSList
            | SList xs, SList ys -> xs @ ys |> toSList
            | x, y -> [ x; y ] |> toSList

        function
        | [] -> SEmpty
        | SUnquote x :: xs
        | SList [ SSymbol "unquote"; x ] :: xs ->
            if n = 0 then
                x
                |> Eval.eval envs (fun a -> xs |> toSList |> replaceQuasiquote envs cont n (fun b -> cons a b |> cont))
            else
                x
                |> replaceQuasiquote envs cont (n - 1) (fun a ->
                    xs |> toSList |> replaceQuasiquote envs cont n (fun b -> cons (SUnquote a) b))
        | SUnquoteSplicing x :: xs
        | SList [ SSymbol "unquote-splicing"; x ] :: xs ->
            if n = 0 then
                x
                |> Eval.eval envs (fun a ->
                    xs |> toSList |> replaceQuasiquote envs cont n (fun b -> join (a, b) |> cont))
            else
                x
                |> replaceQuasiquote envs cont (n - 1) (fun a ->
                    xs
                    |> toSList
                    |> replaceQuasiquote envs cont n (fun b -> cons (SUnquoteSplicing a) b))
        | SQuasiquote x :: xs
        | SList [ SSymbol "quasiquote"; x ] :: xs ->
            x
            |> replaceQuasiquote envs cont (n + 1) (fun a ->
                xs |> toSList |> replaceQuasiquote envs cont n (fun b -> cons (SQuasiquote a) b))
        | SQuote x :: xs
        | SList [ SSymbol "quote"; x ] :: xs ->
            x
            |> replaceQuasiquote envs cont n (fun a ->
                xs |> toSList |> replaceQuasiquote envs cont n (fun b -> cons (SQuote a) b))
        | x :: xs ->
            x
            |> replaceQuasiquote envs cont n (fun a ->
                xs |> toSList |> replaceQuasiquote envs cont n (fun b -> cons a b))

    and replaceQuasiquoteDatum envs cont n =
        function
        | SUnquote x
        | SList [ SSymbol "unquote"; x ] ->
            if n = 0 then
                x |> Eval.eval envs cont
            else
                x |> replaceQuasiquote envs cont (n - 1) SUnquote
        | SUnquoteSplicing x
        | SList [ SSymbol "unquote-splicing"; x ] ->
            if n = 0 then
                x |> Eval.eval envs cont
            else
                x |> replaceQuasiquote envs cont (n - 1) SUnquoteSplicing
        | SQuasiquote x
        | SList [ SSymbol "quasiquote"; x ] -> x |> replaceQuasiquote envs cont (n + 1) SQuasiquote
        | SQuote x
        | SList [ SSymbol "quote"; x ] -> x |> replaceQuasiquote envs cont n SQuote
        | x -> x

    let sQuasiquote envs cont =
        function
        | [ x ] -> replaceQuasiquote envs cont 0 id x
        | x -> x |> invalidParameter "'%s' invalid quasiquote parameter."

    let sDefine (envs: SEnv list) cont =
        let define' var =
            envs.Head.TryAdd(var, ref SEmpty) |> ignore

            Eval.eval envs (fun x ->
                envs.Head.[var].Value <- x
                var |> SSymbol |> cont)

        function
        | [ SSymbol var; expr ] -> expr |> define' var
        | SList(SSymbol var :: formals) :: body -> sLambda envs cont (SList formals :: body) |> define' var
        | SPair([ SSymbol var ], formal) :: body -> sLambda envs cont (formal :: body) |> define' var
        | x -> x |> invalidParameter "'%s' invalid define parameter."
