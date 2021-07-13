module Print

open Type

let rec print =
    function
    | SEmpty -> "()"
    | SBool true -> "#t"
    | SBool false -> "#f"
    | SNumber n -> n.ToString()
    | SString s -> "\"" + s.Replace("\"", "\\\"") + "\""
    | SChar s -> "#\\" + s
    | SSymbol s -> s
    | SQuote s -> "(quote " + (print s) + ")"
    | SUnquote s -> "(unquote " + (print s) + ")"
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
