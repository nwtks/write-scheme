module Print

open Type

let rec print =
    function
    | SEmpty -> "()"
    | SBool true -> "#t"
    | SBool false -> "#f"
    | SRational (x1, x2) ->
        if x2 = 1I then
            x1.ToString()
        else
            sprintf "%A/%A" x1 x2
    | SReal x -> x.ToString()
    | SString x -> x.Replace("\"", "\\\"") |> sprintf "\"%s\""
    | SChar x -> sprintf "#\\%s" x
    | SSymbol x -> x
    | SQuote x -> print x |> sprintf "(quote %s)"
    | SQuasiquote x -> print x |> sprintf "(quasiquote %s)"
    | SUnquote x -> print x |> sprintf "(unquote %s)"
    | SUnquoteSplicing x -> print x |> sprintf "(unquote-splicing %s)"
    | SList xs -> xs |> printList |> sprintf "(%s)"
    | SPair (x1, x2) -> sprintf "(%s . %s)" (printList x1) (print x2)
    | SClosure _
    | FFunction _ -> "Procedure"

and printList xs =
    xs |> List.map print |> String.concat " "
