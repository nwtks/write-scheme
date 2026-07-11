namespace WriteScheme

module Type =
    type Position = { line: int64; column: int64 }

    type SStringData =
        { runes: System.Text.Rune array
          isImmutable: bool }

    [<ReferenceEquality>]
    type SExpressionKind =
        | SUnspecified
        | SEmpty
        | SEof
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
        | SPort of SPortData
        | SSyntax of SProcedureKind
        | SProcedure of SProcedureKind
        | SContinuation of SContinuation

    and SExpression = SExpressionKind * Position option

    and PortDirection =
        | Input
        | Output

    and [<ReferenceEquality>] SPortData =
        { direction: PortDirection
          isTextual: bool
          mutable isOpen: bool
          inputReader: System.IO.TextReader option
          outputWriter: System.IO.TextWriter option
          fileStream: System.IO.Stream option
          filePath: string option }

    and PortSet =
        { input: SPortData
          output: SPortData
          error: SPortData }

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
          mutable ports: PortSet
          winders: Winder list ref
          nextWinderId: int ref
          handlers: SExpression list ref }

    and Environment = Map<string, SExpression ref> ref

    and Library =
        { name: string
          environment: Environment
          exports: Map<string, string> }

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

    let toSPair list =
        (SEmpty, None)
        |> List.foldBack (fun x acc -> SPair { car = x; cdr = acc }, snd x) list

    // Floyd's cycle-finding algorithm
    [<TailCall>]
    let rec loopListInfo tortoise hare accLength accList =
        match hare with
        | SEmpty, _ -> Ok(List.rev accList, accLength)
        | SPair pHare, _ ->
            match pHare.cdr with
            | SEmpty, _ -> Ok(List.rev (pHare.car :: accList), accLength + 1I)
            | SPair pHareNext, _ -> checkAndAdvance tortoise pHareNext accLength accList pHare.car
            | _ -> Error "not a proper list."
        | _ -> Error "not a proper list."

    and checkAndAdvance tortoise pHareNext accLength accList pCar =
        match tortoise with
        | SPair pTortoise, _ when obj.ReferenceEquals(pTortoise, pHareNext) -> Error "circular list."
        | SPair pTortoise, _ ->
            loopListInfo pTortoise.cdr pHareNext.cdr (accLength + 2I) (pHareNext.car :: pCar :: accList)
        | _ -> Error "invalid list structure."

    let isProperList =
        function
        | SEmpty, _ -> true
        | SPair _, _ as pair ->
            match loopListInfo pair pair 0I [] with
            | Ok _ -> true
            | Error _ -> false
        | _ -> false

    let toList =
        function
        | SEmpty, _ -> Ok []
        | SPair _, _ as pair ->
            match loopListInfo pair pair 0I [] with
            | Ok(l, _) -> Ok l
            | Error msg -> EvalError(msg, snd pair) |> Error
        | _, pos -> EvalError("not a proper list.", pos) |> Error

    let SZero = SRational(0I, 1I)

    let newInteger n =
        if n = 0I then SZero else SRational(n, 1I)

    let normalizeRational n d =
        if d = 0I then
            Error "Division by zero."
        elif n = 0I then
            Ok(0I, 1I)
        else
            let g = bigint.GreatestCommonDivisor(abs n, abs d)
            let n', d' = n / g, d / g

            if d' < 0I then Ok(-n', -d') else Ok(n', d')

    let newSRational n d =
        normalizeRational n d
        |> Result.map (fun (n', d') -> if n' = 0I then SZero else SRational(n', d'))

    let parseFloatString (s: string) =
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

    let realToRational r =
        if System.Double.IsInfinity r || System.Double.IsNaN r then
            SReal r
        else
            let s = $"{r:g17}"

            let res =
                if s.Contains '.' || s.Contains 'e' || s.Contains 'E' then
                    parseFloatString s
                else
                    bigint r |> newInteger |> Ok

            match res with
            | Ok r -> r
            | Error _ -> SReal r

    let runesToString runes =
        let sb = System.Text.StringBuilder()
        runes |> Seq.iter (string >> sb.Append >> ignore)
        sb |> string

    let newSString isImmutable (str: string) =
        { runes = str.EnumerateRunes() |> Seq.toArray
          isImmutable = isImmutable }
        |> SString
