module Type

[<ReferenceEqualityAttribute>]
type SExpression =
    | SEmpty
    | SBool of bool
    | SRational of bigint * bigint
    | SReal of float
    | SString of string
    | SChar of string
    | SSymbol of string
    | SList of SExpression list
    | SPair of SExpression list * SExpression
    | SQuote of SExpression
    | SQuasiquote of SExpression
    | SUnquote of SExpression
    | SUnquoteSplicing of SExpression
    | SSyntax of (SEnv list -> SContinuation -> SExpression list -> SExpression)
    | SProcedure of (SEnv list -> SContinuation -> SExpression list -> SExpression)
    | SContinuation of SContinuation

and SEnv = System.Collections.Generic.Dictionary<string, SExpression ref>
and SContinuation = SExpression -> SExpression

let STrue = SBool true
let SFalse = SBool false

let SZero = SRational(0I, 1I)

let SPositiveInfinity = SReal System.Double.PositiveInfinity
let SNegativeInfinity = SReal System.Double.NegativeInfinity
let SNaN = SReal System.Double.NaN

let newBool x = if x then STrue else SFalse

let newList =
    function
    | [] -> SEmpty
    | xs -> SList xs

let newRational (x1: bigint) (x2: bigint) =
    if x2.IsZero then
        failwith "denominator zero."
    elif x1.IsZero then
        SZero
    else
        let gcd = bigint.GreatestCommonDivisor(abs x1, abs x2)
        let x1', x2' = if x2.Sign < 0 then -x1, -x2 else x1, x2
        SRational(x1' / gcd, x2' / gcd)
