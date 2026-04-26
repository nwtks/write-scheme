namespace WriteScheme

module Type =
    [<CustomEquality; CustomComparison>]
    type SStringData =
        { runes: System.Text.Rune array
          mutable isImmutable: bool }

        override this.Equals other =
            match other with
            | :? SStringData as o -> this.runes.Length = o.runes.Length && Array.forall2 (=) this.runes o.runes
            | _ -> false

        override this.GetHashCode() =
            let mutable h = 17

            for r in this.runes do
                h <- h * 31 + r.Value

            h

        interface System.IComparable with
            member this.CompareTo other =
                match other with
                | :? SStringData as o ->
                    let len = min this.runes.Length o.runes.Length

                    let rec loop i =
                        if i = len then
                            this.runes.Length.CompareTo o.runes.Length
                        else
                            let cmp = this.runes.[i].Value.CompareTo(o.runes.[i].Value)
                            if cmp <> 0 then cmp else loop (i + 1)

                    loop 0
                | _ -> invalidArg "other" "not a SStringData"

    let runesToString (runes: System.Text.Rune array) =
        let sb = System.Text.StringBuilder runes.Length

        for r in runes do
            sb.Append(r.ToString()) |> ignore

        sb.ToString()

    type Position = { Line: int64; Column: int64 }

    [<ReferenceEquality>]
    type SExpressionKind =
        | SUnspecified
        | SEmpty
        | SBool of bool
        | SRational of bigint * bigint
        | SReal of float
        | SComplex of System.Numerics.Complex
        | SString of SStringData
        | SChar of System.Text.Rune
        | SSymbol of string
        | SPair of SPairData
        | SVector of SExpression array
        | SByteVector of byte array
        | SValues of SExpression list
        | SRecord of typeId: int * typeName: string * fields: SExpression ref array
        | SError of SStringData * SExpression list
        | SQuote of SExpression
        | SQuasiquote of SExpression
        | SUnquote of SExpression
        | SUnquoteSplicing of SExpression
        | SPromise of (bool * SExpression) ref
        | SParameter of SExpression ref * SExpression option
        | SSyntax of (Context -> SContinuation -> SExpression list -> SExpression)
        | SProcedure of (Context -> SContinuation -> SExpression list -> SExpression)
        | SContinuation of SContinuation

    and SExpression = SExpressionKind * Position option

    and [<ReferenceEquality>] SPairData =
        { mutable car: SExpression
          mutable cdr: SExpression }

    and SContinuation = SExpression -> SExpression

    and Context =
        { environments: Environment list
          mutable nextExpansionId: int
          mutable nextRecordTypeId: int
          currentWinders: Winder list ref
          nextWinderId: int ref }

    and Environment = Map<string, SExpression ref> ref

    and Winder =
        { id: int
          before: SExpression
          after: SExpression }

    let (|SUnspecified|_|) =
        function
        | SUnspecified, _ -> Some()
        | _ -> None

    let (|SEmpty|_|) =
        function
        | SEmpty, _ -> Some()
        | _ -> None

    let (|SBool|_|) =
        function
        | SBool b, _ -> Some b
        | _ -> None

    let (|SRational|_|) =
        function
        | SRational(n, d), _ -> Some(n, d)
        | _ -> None

    let (|SReal|_|) =
        function
        | SReal r, _ -> Some r
        | _ -> None

    let (|SComplex|_|) =
        function
        | SComplex c, _ -> Some c
        | _ -> None

    let (|SString|_|) =
        function
        | SString s, _ -> Some s
        | _ -> None

    let (|SChar|_|) =
        function
        | SChar c, _ -> Some c
        | _ -> None

    let (|SSymbol|_|) =
        function
        | SSymbol s, _ -> Some s
        | _ -> None

    let (|SPair|_|) =
        function
        | SPair p, _ -> Some p
        | _ -> None

    let (|SVector|_|) =
        function
        | SVector v, _ -> Some v
        | _ -> None

    let (|SByteVector|_|) =
        function
        | SByteVector bv, _ -> Some bv
        | _ -> None

    let (|SValues|_|) =
        function
        | SValues vs, _ -> Some vs
        | _ -> None

    let (|SRecord|_|) =
        function
        | SRecord(id, n, fs), _ -> Some(id, n, fs)
        | _ -> None

    let (|SError|_|) =
        function
        | SError(m, i), _ -> Some(m, i)
        | _ -> None

    let (|SQuote|_|) =
        function
        | SQuote q, _ -> Some q
        | _ -> None

    let (|SQuasiquote|_|) =
        function
        | SQuasiquote q, _ -> Some q
        | _ -> None

    let (|SUnquote|_|) =
        function
        | SUnquote u, _ -> Some u
        | _ -> None

    let (|SUnquoteSplicing|_|) =
        function
        | SUnquoteSplicing u, _ -> Some u
        | _ -> None

    let (|SPromise|_|) =
        function
        | SPromise p, _ -> Some p
        | _ -> None

    let (|SParameter|_|) =
        function
        | SParameter(r, c), _ -> Some(r, c)
        | _ -> None

    let (|SSyntax|_|) =
        function
        | SSyntax s, _ -> Some s
        | _ -> None

    let (|SProcedure|_|) =
        function
        | SProcedure p, _ -> Some p
        | _ -> None

    let (|SContinuation|_|) =
        function
        | SContinuation c, _ -> Some c
        | _ -> None

    let SUnspecified: SExpression = SUnspecified, None
    let SEmpty: SExpression = SEmpty, None
    let SBool b : SExpression = SBool b, None
    let SRational (n, d) : SExpression = SRational(n, d), None
    let SReal r : SExpression = SReal r, None
    let SComplex c : SExpression = SComplex c, None
    let SString s : SExpression = SString s, None
    let SChar c : SExpression = SChar c, None
    let SSymbol s : SExpression = SSymbol s, None
    let SPair p : SExpression = SPair p, None
    let SVector v : SExpression = SVector v, None
    let SByteVector bv : SExpression = SByteVector bv, None
    let SValues vs : SExpression = SValues vs, None
    let SRecord (id, name, fs) : SExpression = SRecord(id, name, fs), None
    let SError (m, i) : SExpression = SError(m, i), None
    let SQuote q : SExpression = SQuote q, None
    let SQuasiquote q : SExpression = SQuasiquote q, None
    let SUnquote u : SExpression = SUnquote u, None
    let SUnquoteSplicing u : SExpression = SUnquoteSplicing u, None
    let SPromise p : SExpression = SPromise p, None
    let SParameter (r, c) : SExpression = SParameter(r, c), None
    let SSyntax s : SExpression = SSyntax s, None
    let SProcedure p : SExpression = SProcedure p, None
    let SContinuation c : SExpression = SContinuation c, None

    let STrue = SBool true
    let SFalse = SBool false
    let toSBool x = if x then STrue else SFalse

    let toSPair xs =
        List.foldBack (fun x acc -> SPair { car = x; cdr = acc }) xs SEmpty

    [<TailCall>]
    let rec loopProperList tortoise =
        function
        | SEmpty -> true
        | SPair pHare ->
            match pHare.cdr with
            | SEmpty -> true
            | SPair pHareNext ->
                match tortoise with
                | SPair pTortoise when obj.ReferenceEquals(pTortoise, pHareNext) -> false
                | SPair pTortoise -> pHareNext.cdr |> loopProperList pTortoise.cdr
                | _ -> false
            | _ -> false
        | _ -> false

    let isProperList =
        function
        | SEmpty -> true
        | SPair p as expr -> p.cdr |> loopProperList expr
        | _ -> false

    [<TailCall>]
    let rec loopToList acc =
        function
        | SEmpty -> List.rev acc
        | SPair p -> loopToList (p.car :: acc) p.cdr
        | _ -> failwith "not a proper list"

    let toList expr = loopToList [] expr

    let SZero = SRational(0I, 1I)

    let newSRational (numerator: bigint) (denominator: bigint) =
        if denominator.IsZero then
            failwith "denominator zero."
        elif numerator.IsZero then
            SZero
        else
            let gcd = bigint.GreatestCommonDivisor(abs numerator, abs denominator)

            let numerator', denominator' =
                if denominator.Sign < 0 then
                    -numerator, -denominator
                else
                    numerator, denominator

            SRational(numerator' / gcd, denominator' / gcd)

    let newSString isImmutable (str: string) =
        { runes = str.EnumerateRunes() |> Seq.toArray
          isImmutable = isImmutable }
        |> SString

    exception SchemeRaise of SExpression
