module Print

open Type

let rec print =
    function
    | SEmpty -> "()"
    | SBool true -> "#t"
    | SBool false -> "#f"
    | SRational (n1, n2) ->
        if n2 = 1I then
            n1.ToString()
        else
            n1.ToString() + "/" + n2.ToString()
    | SReal n -> n.ToString()
    | SString s -> "\"" + s.Replace("\"", "\\\"") + "\""
    | SChar s -> "#\\" + s
    | SSymbol s -> s
    | SQuote s -> "(quote " + (print s) + ")"
    | SQuasiquote s -> "(quasiquote " + (print s) + ")"
    | SUnquote s -> "(unquote " + (print s) + ")"
    | SUnquoteSplicing s -> "(unquote-splicing " + (print s) + ")"
    | SList list ->
        "("
        + (list |> List.map print |> String.concat " ")
        + ")"
    | SPair (list, rest) ->
        "("
        + (list |> List.map print |> String.concat " ")
        + " . "
        + print rest
        + ")"
    | SClosure _
    | FFunction _ -> "Procedure"
