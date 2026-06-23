namespace WriteScheme.Builtins

open WriteScheme
open Type

[<AutoOpen>]
module Binding =
    [<TailCall>]
    let rec bindLet context pos cont body acc =
        function
        | [] ->
            body
            |> Eval.evalBody (acc |> List.rev |> Context.extendEnvironments context) cont (Ok(SUnspecified, pos))
        | (variable, init) :: bindings ->
            init
            |> Eval.eval context (function
                | Ok i -> bindings |> bindLet context pos cont body ((variable, ref i) :: acc)
                | x -> x |> cont)

    let evalNamedLet context pos cont variable body bindings' =
        let r = ref (SUnspecified, pos)
        let context' = [ variable, r ] |> Context.extendEnvironments context
        let formals = bindings' |> List.map (fun (v, _) -> SSymbol v, pos) |> toSPair
        let proc = SProcedure(closure context' formals body), pos
        r.Value <- proc

        bindings'
        |> List.map snd
        |> Eval.evalArgs context' cont (fun e c a -> proc |> Eval.apply e c a) []

    let parseLetBindings bindings =
        bindings |> toList |> Result.bind (mapResult eachBinding)

    let sLet context pos cont =
        function
        | (SSymbol variable, _) :: bindings :: body ->
            match bindings |> parseLetBindings with
            | Ok bindings' -> evalNamedLet context pos cont variable body bindings'
            | Error e -> Error e |> cont
        | bindings :: body ->
            match bindings |> parseLetBindings with
            | Ok bindings' -> bindings' |> bindLet context pos cont body []
            | Error e -> Error e |> cont
        | x -> x |> invalidParameter pos "'%s' invalid let parameter." |> cont

    [<TailCall>]
    let rec bindLetStar context pos cont body =
        function
        | [] -> body |> Eval.evalBody context cont (Ok(SUnspecified, pos))
        | (variable, init) :: bindings ->
            init
            |> Eval.eval context (function
                | Ok i ->
                    bindings
                    |> bindLetStar ([ variable, ref i ] |> Context.extendEnvironments context) pos cont body
                | x -> x |> cont)

    let sLetStar context pos cont =
        function
        | bindings :: body ->
            match bindings |> parseLetBindings with
            | Ok bindings' -> bindings' |> bindLetStar context pos cont body
            | Error e -> Error e |> cont
        | x -> x |> invalidParameter pos "'%s' invalid let* parameter." |> cont

    [<TailCall>]
    let rec bindLetRec context pos cont body =
        function
        | [] -> body |> Eval.evalBody context cont (Ok(SUnspecified, pos))
        | (variable, init) :: bindings ->
            init
            |> Eval.eval context (function
                | Ok i ->
                    Context.defineEnvironmentVariable context variable i
                    bindings |> bindLetRec context pos cont body
                | x -> x |> cont)

    let sLetRec context pos cont =
        let bindRef bindings =
            bindings
            |> List.map (function
                | variable, _ -> variable, ref (SEmpty, pos))
            |> Context.extendEnvironments context

        function
        | bindings :: body ->
            match bindings |> parseLetBindings with
            | Ok bindings' -> bindings' |> bindLetRec (bindings' |> bindRef) pos cont body
            | Error e -> Error e |> cont
        | x -> x |> invalidParameter pos "'%s' invalid letrec parameter." |> cont

    [<TailCall>]
    let rec bindLetRecStar context pos cont body =
        function
        | [], _
        | _, [] -> body |> Eval.evalBody context cont (Ok(SUnspecified, pos))
        | (_, init) :: bindings, refs: SExpression ref :: rs ->
            init
            |> Eval.eval context (function
                | Ok i ->
                    refs.Value <- i
                    (bindings, rs) |> bindLetRecStar context pos cont body
                | x -> x |> cont)

    let sLetRecStar context pos cont =
        let eachRef (context', refs) (variable, _) =
            let r = ref (SEmpty, pos)
            [ variable, r ] |> Context.extendEnvironments context', r :: refs

        let bindRef bindings =
            let context', refs = bindings |> List.fold eachRef (context, [])
            context', refs |> List.rev

        function
        | bindings :: body ->
            match bindings |> parseLetBindings with
            | Ok bindings' ->
                let context', refs = bindRef bindings'
                (bindings', refs) |> bindLetRecStar context' pos cont body
            | Error e -> Error e |> cont
        | x -> x |> invalidParameter pos "'%s' invalid letrec* parameter." |> cont

    let eachValuesBinding =
        function
        | SPair { car = formals
                  cdr = SPair { car = init; cdr = SEmpty, _ }, _ },
          _ ->
            match formals |> toList with
            | Ok flist ->
                flist
                |> mapResult (function
                    | SSymbol v, _ -> Ok v
                    | x -> x |> invalid (snd x) "'%s' is not a symbol.")
                |> Result.map (fun vars -> vars, init)
            | Error e -> Error e
        | x -> x |> invalid (snd x) "'%s' invalid values binding."

    let parseLetValuesBindings bindings =
        bindings |> toList |> Result.bind (mapResult eachValuesBinding)

    let matchValuesBinding pos cont name variables init next =
        let values =
            match init with
            | SValues vs, _ -> vs
            | value -> [ value ]

        if List.length variables <> List.length values then
            EvalError($"Values count mismatch in {name}.", pos) |> Error |> cont
        else
            List.zip variables values
            |> List.map (fun (variable, value) -> variable, ref value)
            |> next

    [<TailCall>]
    let rec bindLetValues context pos cont body acc =
        function
        | [] ->
            body
            |> Eval.evalBody (acc |> List.rev |> Context.extendEnvironments context) cont (Ok(SUnspecified, pos))
        | (variables, init) :: bindings ->
            init
            |> Eval.eval context (function
                | Ok i ->
                    matchValuesBinding pos cont "let-values" variables i (fun bindings' ->
                        bindings |> bindLetValues context pos cont body (bindings' @ acc))
                | x -> x |> cont)

    let sLetValues context pos cont =
        function
        | bindings :: body ->
            match bindings |> parseLetValuesBindings with
            | Ok bindings' -> bindings' |> bindLetValues context pos cont body []
            | Error e -> Error e |> cont
        | x -> x |> invalidParameter pos "'%s' invalid let-values parameter." |> cont

    [<TailCall>]
    let rec bindLetStarValues context pos cont body =
        function
        | [] -> body |> Eval.evalBody context cont (Ok(SUnspecified, pos))
        | (variables, init) :: bindings ->
            init
            |> Eval.eval context (function
                | Ok i ->
                    matchValuesBinding pos cont "let-star-values" variables i (fun bindings' ->
                        let nextContext = bindings' |> Context.extendEnvironments context
                        bindings |> bindLetStarValues nextContext pos cont body)
                | x -> x |> cont)

    let sLetStarValues context pos cont =
        function
        | bindings :: body ->
            match bindings |> parseLetValuesBindings with
            | Ok bindings' -> bindings' |> bindLetStarValues context pos cont body
            | Error e -> Error e |> cont
        | x -> x |> invalidParameter pos "'%s' invalid let*-values parameter." |> cont
