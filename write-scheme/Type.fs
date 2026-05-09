namespace WriteScheme

module Type =
    type Position = { Line: int64; Column: int64 }

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
          libraries: Map<string, Library> ref
          mutable nextExpansionId: int
          mutable nextRecordTypeId: int
          winders: Winder list ref
          nextWinderId: int ref
          handlers: SExpression list ref }

    and Environment = Map<string, SExpression ref> ref

    and Library =
        { name: string
          env: Environment
          exports: Set<string> }

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
        (SEmpty, None)
        |> List.foldBack (fun x acc -> SPair { car = x; cdr = acc }, snd x) xs

    // Floyd's cycle-finding algorithm
    [<TailCall>]
    let rec loopListInfo tortoise hare accLength accList =
        match hare with
        | SEmpty, _ -> Ok(accList |> Option.map List.rev, accLength)
        | SPair pHare, _ ->
            match pHare.cdr with
            | SEmpty, _ -> Ok(accList |> Option.map (fun l -> pHare.car :: l |> List.rev), accLength + 1I)
            | SPair pHareNext, _ ->
                match tortoise with
                | SPair pTortoise, _ when obj.ReferenceEquals(pTortoise, pHareNext) -> Error "circular list."
                | SPair pTortoise, _ ->
                    accList
                    |> Option.map (fun l -> pHareNext.car :: pHare.car :: l)
                    |> loopListInfo pTortoise.cdr pHareNext.cdr (accLength + 2I)
                | _ -> Error "invalid list structure."
            | _ -> Error "not a proper list."
        | _ -> Error "not a proper list."

    let isProperList =
        function
        | SEmpty, _ -> true
        | SPair _, _ as pair ->
            match loopListInfo pair pair 0I None with
            | Ok _ -> true
            | Error _ -> false
        | _ -> false

    let toList =
        function
        | SEmpty, _ -> Ok []
        | SPair _, _ as pair ->
            match loopListInfo pair pair 0I (Some []) with
            | Ok(Some l, _) -> Ok l
            | Ok(None, _) -> failwith "unreachable."
            | Error msg -> EvalError(msg, snd pair) |> Error
        | _, pos -> EvalError("not a proper list.", pos) |> Error

    let SZero = SRational(0I, 1I)

    let newInteger n =
        if n = 0I then SZero else SRational(n, 1I)

    let newSRational n d =
        if d = 0I then
            Error "Division by zero."
        elif n = 0I then
            Ok SZero
        else
            let g = bigint.GreatestCommonDivisor(abs n, abs d)
            let n', d' = n / g, d / g

            if d' < 0I then
                SRational(-n', -d') |> Ok
            else
                SRational(n', d') |> Ok

    let realToRational x =
        if System.Double.IsInfinity x || System.Double.IsNaN x then
            SReal x
        else
            let s = sprintf "%.17g" x

            let res =
                if s.Contains '.' || s.Contains 'e' || s.Contains 'E' then
                    let parts = s.Split [| 'e'; 'E' |]
                    let baseNum = parts.[0]
                    let exp = if parts.Length > 1 then int parts.[1] else 0
                    let dotIdx = baseNum.IndexOf '.'
                    let digits = baseNum.Replace(".", "")
                    let scale = if dotIdx < 0 then 0 else baseNum.Length - dotIdx - 1
                    let numerator = bigint.Parse digits

                    if scale - exp < 0 then
                        numerator * bigint.Pow(10I, exp - scale) |> newInteger |> Ok
                    else
                        let denominator = bigint.Pow(10I, scale - exp)
                        newSRational numerator denominator
                else
                    bigint x |> newInteger |> Ok

            match res with
            | Ok r -> r
            | Error _ -> SReal x

    let runesToString runes =
        let sb = System.Text.StringBuilder()
        runes |> Seq.iter (string >> sb.Append >> ignore)
        sb |> string

    let newSString isImmutable (str: string) =
        { runes = str.EnumerateRunes() |> Seq.toArray
          isImmutable = isImmutable }
        |> SString
