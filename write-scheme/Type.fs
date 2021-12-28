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
    | SClosure of (Env list -> Continuation -> SExpression list -> SExpression)
    | FFunction of (Continuation -> SExpression list -> SExpression)

and Env = System.Collections.Generic.Dictionary<string, SExpression ref>

and Continuation = SExpression -> SExpression

let STrue = SBool true
let SFalse = SBool false

let SZero = SRational(0I, 1I)

let SPositiveInfinity = SReal System.Double.PositiveInfinity
let SNegativeInfinity = SReal System.Double.NegativeInfinity
let SNaN = SReal nan

let newSBool b = if b then STrue else SFalse

let newSRational n1 n2 =
    if n2 = 0I then
        failwith "denominator zero."

    if n1 = 0I then
        SZero
    else
        let gcd =
            System.Numerics.BigInteger.GreatestCommonDivisor(
                System.Numerics.BigInteger.Abs(n1),
                System.Numerics.BigInteger.Abs(n2)
            )

        let n1', n2' =
            if n2.Sign < 0 then -n1, -n2 else n1, n2

        SRational(n1' / gcd, n2' / gcd)
