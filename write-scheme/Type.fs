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
    | SClosure of (SEnv list -> SContinuation -> SExpression list -> SExpression)
    | FFunction of (SContinuation -> SExpression list -> SExpression)

and SEnv = System.Collections.Generic.Dictionary<string, SExpression ref>
and SContinuation = SExpression -> SExpression

let STrue = SBool true
let SFalse = SBool false

let SZero = SRational(0I, 1I)

let SPositiveInfinity = SReal System.Double.PositiveInfinity
let SNegativeInfinity = SReal System.Double.NegativeInfinity
let SNaN = SReal nan

let newBool x = if x then STrue else SFalse

let newRational n1 n2 =
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
