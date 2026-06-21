namespace WriteScheme.Builtins

open WriteScheme
open Type

[<AutoOpen>]
module Inclusion =
    [<TailCall>]
    let rec sIncludeFiles foldCase context pos cont acc =
        function
        | [] ->
            match acc |> List.rev with
            | [] -> Ok(SUnspecified, pos) |> cont
            | expressions -> expressions |> Eval.eachEval context cont (Ok(SUnspecified, pos))
        | (SString f, p) :: rest ->
            match readAndResolveInclude foldCase f p with
            | Ok resolvedExpressions ->
                rest
                |> sIncludeFiles foldCase context pos cont (List.rev resolvedExpressions @ acc)
            | Error e -> Error e |> cont
        | x :: _ -> [ x ] |> invalidParameter pos "'%s' invalid include parameter." |> cont

    let sInclude context pos cont files =
        files |> sIncludeFiles false context pos cont []

    let sIncludeCi context pos cont files =
        files |> sIncludeFiles true context pos cont []
