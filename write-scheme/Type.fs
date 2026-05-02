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
            r |> string |> sb.Append |> ignore

        sb |> string

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
        | SDatumLabel of int * SExpression
        | SDatumRef of int
        | SPromise of (bool * SExpression) ref
        | SParameter of SExpression ref * SExpression option
        | SSyntax of (Context -> Position option -> SContinuation -> SExpression list -> SExpression)
        | SProcedure of (Context -> Position option -> SContinuation -> SExpression list -> SExpression)
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

    let STrue = SBool true
    let SFalse = SBool false
    let toSBool x = if x then STrue else SFalse

    let toSPair xs =
        List.foldBack (fun x acc -> SPair { car = x; cdr = acc }, snd x) xs (SEmpty, None)

    // Floyd's cycle-finding algorithm
    [<TailCall>]
    let rec loopProperList tortoise =
        function
        | SEmpty, _ -> true
        | SPair pHare, _ ->
            match pHare.cdr with
            | SEmpty, _ -> true
            | SPair pHareNext, _ ->
                match tortoise with
                | SPair pTortoise, _ when obj.ReferenceEquals(pTortoise, pHareNext) -> false
                | SPair pTortoise, _ -> pHareNext.cdr |> loopProperList pTortoise.cdr
                | _ -> false
            | _ -> false
        | _ -> false

    let isProperList =
        function
        | SEmpty, _ -> true
        | SPair p, _ as expr -> p.cdr |> loopProperList expr
        | _ -> false

    let formatPosition pos =
        match pos with
        | Some p -> sprintf " (at line %d, column %d)" p.Line p.Column
        | None -> ""

    [<TailCall>]
    let rec loopToList acc =
        function
        | SEmpty, _ -> List.rev acc
        | SPair p, _ -> loopToList (p.car :: acc) p.cdr
        | x -> failwithf "not a proper list.%s" (x |> snd |> formatPosition)

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
