namespace WriteScheme.Builtins

open WriteScheme
open Type
open Eval

[<AutoOpen>]
module SpecialForm =
    let sQuote envs cont =
        function
        | [ x ] -> x |> cont
        | x -> x |> invalidParameter "'%s' invalid quote parameter."

    let sLambda envs cont =
        let rec bindArgs body envs' cont' acc =
            function
            | [] -> body |> eachEval (List.rev acc |> extendEnvs envs') cont' SEmpty
            | (var, expr) :: xs ->
                expr
                |> eval envs' (fun a -> xs |> bindArgs body envs' cont' ((var, ref a) :: acc))

        let closure formals body envs' cont' args =
            formals |> zipFormals args |> bindArgs body (envs' @ envs) cont' []

        function
        | formals :: body -> closure formals body |> SSyntax |> cont
        | x -> x |> invalidParameter "'%s' invalid lambda parameter."

    let sIf envs cont =
        let if' test conseq alter =
            test
            |> eval envs (function
                | SBool false -> alter |> eval envs cont
                | _ -> conseq |> eval envs cont)

        function
        | [ test; conseq; alter ] -> if' test conseq alter
        | [ test; conseq ] -> if' test conseq SEmpty
        | x -> x |> invalidParameter "'%s' invalid if parameter."

    let sSet envs cont =
        function
        | [ SSymbol var; expr ] ->
            expr
            |> eval envs (fun x ->
                (lookupEnvs envs var).Value <- x
                x |> cont)
        | x -> x |> invalidParameter "'%s' invalid set! parameter."

    let rec sCond envs cont =
        let eachTest conseq clauses =
            eval envs (function
                | SBool false -> clauses |> sCond envs cont
                | x -> conseq x)

        function
        | [] -> SEmpty |> cont
        | [ SList(SSymbol "else" :: exprs) ] -> exprs |> eachEval envs cont SEmpty
        | SList [ test; SSymbol "=>"; expr ] :: clauses ->
            test
            |> eachTest (fun a -> [ expr; SQuote a ] |> newList |> eval envs cont) clauses
        | SList(test :: exprs) :: clauses -> test |> eachTest (fun a -> exprs |> eachEval envs cont a) clauses
        | x -> x |> invalidParameter "'%s' invalid cond parameter."

    let rec sAnd envs cont =
        function
        | [] -> STrue |> cont
        | [ test ] ->
            test
            |> eval envs (function
                | SBool false -> SFalse |> cont
                | x -> x |> cont)
        | test :: tests ->
            test
            |> eval envs (function
                | SBool false -> SFalse |> cont
                | _ -> tests |> sAnd envs cont)

    let rec sOr envs cont =
        function
        | [] -> SFalse |> cont
        | test :: tests ->
            test
            |> eval envs (function
                | SBool false -> tests |> sOr envs cont
                | x -> x |> cont)

    let sWhen envs cont =
        function
        | test :: exprs ->
            test
            |> eval envs (function
                | SBool false -> SEmpty |> cont
                | _ -> exprs |> eachEval envs cont SEmpty)
        | x -> x |> invalidParameter "'%s' invalid when parameter."

    let sUnless envs cont =
        function
        | test :: exprs ->
            test
            |> eval envs (function
                | SBool false -> exprs |> eachEval envs cont SEmpty
                | _ -> SEmpty |> cont)
        | x -> x |> invalidParameter "'%s' invalid unless parameter."

    let sLet envs cont =
        let rec bind body acc =
            function
            | [] -> body |> eachEval (List.rev acc |> extendEnvs envs) cont SEmpty
            | (var, expr) :: xs -> expr |> eval envs (fun a -> xs |> bind body ((var, ref a) :: acc))

        function
        | SList bindings :: body -> bindings |> List.map eachBinding |> bind body []
        | x -> x |> invalidParameter "'%s' invalid let parameter."

    let sLetStar envs cont =
        let rec bind body envs' =
            function
            | [] -> body |> eachEval envs' cont SEmpty
            | (var, expr) :: xs ->
                expr
                |> eval envs' (fun a -> xs |> bind body ([ var, ref a ] |> extendEnvs envs'))

        function
        | SList bindings :: body -> bindings |> List.map eachBinding |> bind body envs
        | x -> x |> invalidParameter "'%s' invalid let* parameter."

    let sLetRec envs cont =
        let bindRef bindings =
            bindings
            |> List.map (function
                | (var, _) -> var, ref SEmpty)
            |> extendEnvs envs

        let rec bindExpr body envs' =
            function
            | [] -> body |> eachEval envs' cont SEmpty
            | (var, expr) :: xs ->
                expr
                |> eval envs' (fun a ->
                    envs'.Head.[var].Value <- a
                    xs |> bindExpr body envs')

        function
        | SList bindings :: body ->
            let bindings' = bindings |> List.map eachBinding
            bindings' |> bindExpr body (bindings' |> bindRef)
        | x -> x |> invalidParameter "'%s' invalid letrec parameter."

    let sLetRecStar envs cont =
        let eachRef (envs', refs) =
            function
            | (var, _) ->
                let r = ref SEmpty
                [ var, r ] |> extendEnvs envs', r :: refs

        let bindRef bindings =
            let envs', refs = bindings |> List.fold eachRef (envs, [])
            envs', List.rev refs

        let rec bindExpr body envs' =
            function
            | [], _
            | _, [] -> body |> eachEval envs' cont SEmpty
            | (_, expr) :: xs, (r: SExpression ref) :: rs ->
                expr
                |> eval envs' (fun a ->
                    r.Value <- a
                    (xs, rs) |> bindExpr body envs')

        function
        | SList bindings :: body ->
            let bindings' = bindings |> List.map eachBinding
            let envs', refs = bindRef bindings'
            (bindings', refs) |> bindExpr body envs'
        | x -> x |> invalidParameter "'%s' invalid letrec* parameter."

    let sBegin envs cont = eachEval envs cont SEmpty

    let sQuasiquote envs cont =
        let cons x =
            function
            | SEmpty -> [ x ] |> newList
            | SList ys -> x :: ys |> newList
            | y -> [ x; y ] |> newList

        let join =
            function
            | SEmpty, SEmpty -> SEmpty
            | SList xs, SEmpty -> xs |> newList
            | SEmpty, SList ys -> ys |> newList
            | SList xs, SList ys -> xs @ ys |> newList
            | x, y -> [ x; y ] |> newList

        let rec replace n next =
            function
            | SEmpty -> SEmpty |> next
            | SList xs -> replaceList n xs |> next
            | SPair(x1, x2) ->
                match replaceList n x1 with
                | SList ys -> SPair(ys, replaceDatum n x2) |> next
                | _ -> replaceDatum n x2 |> next
            | x -> replaceDatum n x |> next

        and replaceList n =
            function
            | [] -> SEmpty
            | SUnquote x :: xs
            | SList [ SSymbol "unquote"; x ] :: xs ->
                if n = 0 then
                    x |> eval envs (fun a -> xs |> newList |> replace n (fun b -> cons a b |> cont))
                else
                    x
                    |> replace (n - 1) (fun a -> xs |> newList |> replace n (fun b -> cons (SUnquote a) b))
            | SUnquoteSplicing x :: xs
            | SList [ SSymbol "unquote-splicing"; x ] :: xs ->
                if n = 0 then
                    x
                    |> eval envs (fun a -> xs |> newList |> replace n (fun b -> join (a, b) |> cont))
                else
                    x
                    |> replace (n - 1) (fun a -> xs |> newList |> replace n (fun b -> cons (SUnquoteSplicing a) b))
            | SQuasiquote x :: xs
            | SList [ SSymbol "quasiquote"; x ] :: xs ->
                x
                |> replace (n + 1) (fun a -> xs |> newList |> replace n (fun b -> cons (SQuasiquote a) b))
            | SQuote x :: xs
            | SList [ SSymbol "quote"; x ] :: xs ->
                x
                |> replace n (fun a -> xs |> newList |> replace n (fun b -> cons (SQuote a) b))
            | x :: xs -> x |> replace n (fun a -> xs |> newList |> replace n (fun b -> cons a b))

        and replaceDatum n =
            function
            | SUnquote x
            | SList [ SSymbol "unquote"; x ] ->
                if n = 0 then
                    x |> eval envs cont
                else
                    x |> replace (n - 1) SUnquote
            | SUnquoteSplicing x
            | SList [ SSymbol "unquote-splicing"; x ] ->
                if n = 0 then
                    x |> eval envs cont
                else
                    x |> replace (n - 1) SUnquoteSplicing
            | SQuasiquote x
            | SList [ SSymbol "quasiquote"; x ] -> x |> replace (n + 1) SQuasiquote
            | SQuote x
            | SList [ SSymbol "quote"; x ] -> x |> replace n SQuote
            | x -> x

        function
        | [ x ] -> replace 0 id x
        | x -> x |> invalidParameter "'%s' invalid quasiquote parameter."

    let sDefine (envs: SEnv list) cont =
        let define' var =
            envs.Head.TryAdd(var, ref SEmpty) |> ignore

            eval envs (fun x ->
                envs.Head.[var].Value <- x
                var |> SSymbol |> cont)

        function
        | [ SSymbol var; expr ] -> expr |> define' var
        | SList(SSymbol var :: formals) :: body -> sLambda envs cont (SList(formals) :: body) |> define' var
        | SPair([ SSymbol var ], formal) :: body -> sLambda envs cont (formal :: body) |> define' var
        | x -> x |> invalidParameter "'%s' invalid define parameter."
