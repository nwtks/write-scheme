module Print

open Type

let rec print =
    function
    | SEmpty -> "()"
    | SBool true -> "#t"
    | SBool false -> "#f"
    | SRational (x1, x2) ->
        if x2 = 1I then
            string x1
        else
            sprintf "%A/%A" x1 x2
    | SReal x ->
        if System.Double.IsNaN(x) then
            "+nan.0"
        elif System.Double.IsPositiveInfinity(x) then
            "+inf.0"
        elif System.Double.IsNegativeInfinity(x) then
            "-inf.0"
        else
            string x
    | SString x -> x.Replace("\"", "\\\"") |> sprintf "\"%s\""
    | SChar x -> sprintf "#\\%s" x
    | SSymbol x -> x
    | SQuote x -> print x |> sprintf "'%s"
    | SQuasiquote x -> print x |> sprintf "`%s"
    | SUnquote x -> print x |> sprintf ",%s"
    | SUnquoteSplicing x -> print x |> sprintf ",@%s"
    | SList xs -> xs |> printList |> sprintf "(%s)"
    | SPair (x1, x2) -> sprintf "(%s . %s)" (printList x1) (print x2)
    | SSyntax _
    | SProcedure _ -> "Procedure"
    | SContinuation _ -> "Continuation"

and printList xs =
    xs |> List.map print |> String.concat " "
