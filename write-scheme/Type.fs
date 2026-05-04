namespace WriteScheme

module Type =
    type Position = { Line: int64; Column: int64 }

    let formatPosition =
        function
        | Some pos -> sprintf " (at line %d, column %d)" pos.Line pos.Column
        | None -> ""

    type SStringData =
        { runes: System.Text.Rune array
          isImmutable: bool }

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
        | SSyntax of SProcedureKind
        | SProcedure of SProcedureKind
        | SContinuation of SContinuation

    and SExpression = SExpressionKind * Position option

    and [<ReferenceEquality>] SPairData =
        { mutable car: SExpression
          mutable cdr: SExpression }

    and SProcedureKind =
        Context -> Position option -> SContinuation -> SExpression list -> Result<SExpression, SkipResult>

    and SContinuation = Result<SExpression, SkipResult> -> Result<SExpression, SkipResult>

    and Context =
        { environments: Environment list
          mutable nextExpansionId: int
          mutable nextRecordTypeId: int
          currentWinders: Winder list ref
          nextWinderId: int ref
          currentHandler: SExpression ref }

    and Environment = Map<string, SExpression ref> ref

    and Winder =
        { id: int
          before: SExpression
          after: SExpression }

    and SkipResult =
        | EvalError of string * Position option
        | ParseError of string * Position option
        | SchemeRaise of SExpression * Position option

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

    [<TailCall>]
    let rec loopToList acc =
        function
        | SEmpty, _ -> Ok(acc |> List.rev)
        | SPair p, _ -> p.cdr |> loopToList (p.car :: acc)
        | _, p -> Error(EvalError("not a proper list.", p))

    let toList = loopToList []

    let SZero = SRational(0I, 1I)

    let newInteger n =
        if n = 0I then SZero else SRational(n, 1I)

    let newSRational n d =
        if d = 0I then
            Error "division by zero"
        elif n = 0I then
            Ok SZero
        else
            let g = bigint.GreatestCommonDivisor(abs n, abs d)
            let n', d' = n / g, d / g

            if d' < 0I then
                Ok(SRational(-n', -d'))
            else
                Ok(SRational(n', d'))

    let runesToString runes =
        let sb = System.Text.StringBuilder()

        for r in runes do
            r |> string |> sb.Append |> ignore

        sb |> string

    let newSString isImmutable (str: string) =
        { runes = str.EnumerateRunes() |> Seq.toArray
          isImmutable = isImmutable }
        |> SString
