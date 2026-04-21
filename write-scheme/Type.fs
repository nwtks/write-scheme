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

    [<ReferenceEquality>]
    type SExpression =
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

    let STrue = SBool true
    let SFalse = SBool false

    let SZero = SRational(0I, 1I)

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

    let newSRational (x1: bigint) (x2: bigint) =
        if x2.IsZero then
            failwith "denominator zero."
        elif x1.IsZero then
            SZero
        else
            let gcd = bigint.GreatestCommonDivisor(abs x1, abs x2)
            let x1', x2' = if x2.Sign < 0 then -x1, -x2 else x1, x2
            SRational(x1' / gcd, x2' / gcd)

    let newSString isImmutable (str: string) =
        { runes = str.EnumerateRunes() |> Seq.toArray
          isImmutable = isImmutable }
        |> SString

    exception SchemeRaise of SExpression
