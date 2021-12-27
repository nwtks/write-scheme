module Type

[<ReferenceEqualityAttribute>]
type SExpression =
    | SEmpty
    | SBool of bool
    | SNumber of float
    | SString of string
    | SChar of string
    | SSymbol of string
    | SList of SExpression list
    | SPair of SExpression list * SExpression
    | SQuote of SExpression
    | SQuasiquote of SExpression
    | SUnquote of SExpression
    | SUnquoteSplicing of SExpression
    | SClosure of (Env list -> Continuation -> SExpression list -> SExpression)
    | FFunction of (Continuation -> SExpression list -> SExpression)

and Env = System.Collections.Generic.Dictionary<string, SExpression ref>

and Continuation = SExpression -> SExpression
