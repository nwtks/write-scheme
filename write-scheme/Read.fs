namespace WriteScheme

open FParsec
open Type

module Read =
    type SState = unit
    type Parser<'t> = Parser<'t, SState>

    let pIntralineWhitespace = anyOf " \t"
    let pLineEnding = newline
    let pAtmosphere, pAtmosphereRef = createParserForwardedToRef<unit, SState> ()
    let pIntertokenSpace = skipMany pAtmosphere
    let pIntertokenSpace1 = skipMany1 pAtmosphere
    let pLetter = asciiLetter
    let pSpecialInitial = anyOf "!$%&*/:<=>?^_~"
    let pDigit = digit
    let pHexDigit = hex
    let pExplicitSign = anyOf "+-"
    let pSpecialSubsequent = choice [ pExplicitSign; anyOf ".@" ]
    let hex2int x = (int x &&& 15) + (int x >>> 6) * 9

    let parseWithPos p =
        pipe2 getPosition p (fun pos expr -> expr, Some { Line = pos.Line; Column = pos.Column })

    let pHexScalarValue =
        pHexDigit |> many1Chars
        |>> fun x ->
            x
            |> Seq.map hex2int
            |> Seq.fold (fun acc a -> acc * 16 + a) 0
            |> System.Char.ConvertFromUtf32

    let pInlineHexEscape = between (pstringCI "\\x") (pchar ';') pHexScalarValue

    let pInitial =
        choice [ pLetter |>> string; pSpecialInitial |>> string; pInlineHexEscape ]

    let pSubsequent =
        choice [ pInitial; pDigit |>> string; pSpecialSubsequent |>> string ]

    let pMnemonicEscape =
        choice
            [ stringReturn "\\a" "\u0007"
              stringReturn "\\b" "\u0008"
              stringReturn "\\t" "\u0009"
              stringReturn "\\n" "\u000a"
              stringReturn "\\v" "\u000b"
              stringReturn "\\f" "\u000c"
              stringReturn "\\r" "\u000d" ]

    let pSignSubsequent = choice [ pInitial; pExplicitSign |>> string; pstring "@" ]
    let pDotSubsequent = choice [ pSignSubsequent; pstring "." ]

    let pPeculiarIdentifier =
        choice
            [ attempt (
                  pipe3 pExplicitSign pSignSubsequent (pSubsequent |> manyStrings) (fun c1 s2 s3 ->
                      sprintf "%c%s%s" c1 s2 s3)
              )
              attempt (
                  pipe4 pExplicitSign (pchar '.') pDotSubsequent (pSubsequent |> manyStrings) (fun c1 _ s3 s4 ->
                      sprintf "%c.%s%s" c1 s3 s4)
              )
              pExplicitSign |>> string
              pipe3 (pchar '.') pDotSubsequent (pSubsequent |> manyStrings) (fun _ s2 s3 -> sprintf ".%s%s" s2 s3) ]

    let pSymbolElement =
        choice
            [ noneOf "|\\" |>> string
              stringReturn "\\\\" "\\"
              stringReturn "\\|" "|"
              pMnemonicEscape
              pInlineHexEscape ]

    let pIdentifier =
        choice
            [ pipe2 pInitial (pSubsequent |> manyStrings) (fun s1 s2 -> s1 + s2)
              pPeculiarIdentifier
              between (pchar '|') (pchar '|') (pSymbolElement |> manyStrings) ]

    let pCharacterName =
        choice
            [ stringCIReturn "#\\alarm" "\u0007"
              stringCIReturn "#\\backspace" "\u0008"
              stringCIReturn "#\\delete" "\u007f"
              stringCIReturn "#\\escape" "\u001b"
              stringCIReturn "#\\newline" "\u000a"
              stringCIReturn "#\\null" "\u0000"
              stringCIReturn "#\\return" "\u000d"
              stringCIReturn "#\\space" "\u0020"
              stringCIReturn "#\\tab" "\u0009" ]

    let pAnyRune =
        anyChar
        >>= fun c1 ->
            if System.Char.IsHighSurrogate c1 then
                anyChar
                >>= fun c2 ->
                    if System.Char.IsLowSurrogate c2 then
                        System.Text.Rune(c1, c2) |> string |> preturn
                    else
                        fail "invalid surrogate pair."
            elif System.Char.IsLowSurrogate c1 then
                fail "unexpected low surrogate."
            else
                System.Text.Rune c1 |> string |> preturn

    let pCharacter =
        choice
            [ attempt pCharacterName
              attempt (pstringCI "#\\x" >>. pHexScalarValue)
              pstring "#\\" >>. pAnyRune ]

    let pStringElement =
        choice
            [ noneOf "\"\\" |>> string
              stringReturn "\\\"" "\""
              stringReturn "\\\\" "\\"
              stringReturn "\\|" "|"
              pMnemonicEscape
              pInlineHexEscape
              pchar '\\'
              >>. pipe3
                  (pIntralineWhitespace |> manyChars)
                  pLineEnding
                  (pIntralineWhitespace |> manyChars)
                  (fun _ _ _ -> "") ]

    let pString = between (pchar '"') (pchar '"') (pStringElement |> manyStrings)

    let pSign = anyOf "+-" |> opt |>> Option.defaultValue '+'

    let toBigInteger radix x =
        x |> Seq.map hex2int |> Seq.fold (fun acc a -> acc * radix + bigint a) 0I

    let pUinteger radix =
        let pDigits =
            match radix with
            | 2 -> anyOf "01" |> many1Chars
            | 8 -> anyOf "01234567" |> many1Chars
            | 10 -> digit |> many1Chars
            | 16 -> hex |> many1Chars
            | _ -> failwith "unreachable."

        pDigits |>> toBigInteger (bigint radix)

    let pRationalN radix =
        let pU = pUinteger radix

        choice
            [ attempt (pipe4 pSign pU (pchar '/') pU (fun c1 n2 _ n4 -> (if c1 = '-' then -n2 else n2), n4))
              pipe2 pSign pU (fun c1 n2 -> (if c1 = '-' then -n2 else n2), 1I) ]

    let pRational radix =
        pRationalN radix
        >>= fun (n, d) ->
            match newSRational n d with
            | Result.Ok n -> n |> preturn
            | Result.Error msg -> fail msg

    let pDecimalSuffix =
        anyOf "Ee"
        >>. pipe2 pSign (pDigit |> many1Chars) (fun c1 s2 -> sprintf "E%c%s" c1 s2)

    let pDecimalSuffixOpt = pDecimalSuffix |> opt |>> Option.defaultValue ""

    let pDecimal10 =
        choice
            [ attempt (
                  pipe5
                      pSign
                      (pDigit |> many1Chars)
                      (pchar '.')
                      (pDigit |> many1Chars)
                      pDecimalSuffixOpt
                      (fun c1 s2 _ s4 s5 -> sprintf "%c%s.%s%s" c1 s2 s4 s5 |> System.Double.Parse)
              )
              attempt (
                  pipe4 pSign (pDigit |> many1Chars) (pchar '.') pDecimalSuffixOpt (fun c1 s2 _ s4 ->
                      sprintf "%c%s%s" c1 s2 s4 |> System.Double.Parse)
              )
              attempt (
                  pipe4 pSign (pchar '.') (pDigit |> many1Chars) pDecimalSuffixOpt (fun c1 _ s3 s4 ->
                      sprintf "%c0.%s%s" c1 s3 s4 |> System.Double.Parse)
              )
              attempt (
                  pipe3 pSign (pDigit |> many1Chars) pDecimalSuffix (fun c1 s2 s3 ->
                      sprintf "%c%s%s" c1 s2 s3 |> System.Double.Parse)
              ) ]

    let pRealDouble =
        choice
            [ stringCIReturn "+inf.0" System.Double.PositiveInfinity
              stringCIReturn "-inf.0" System.Double.NegativeInfinity
              stringCIReturn "+nan.0" System.Double.NaN
              stringCIReturn "-nan.0" System.Double.NaN
              pstringCI "#d" >>. pDecimal10
              pDecimal10 ]

    let pReal = pRealDouble |>> SReal

    let pFloat radix =
        if radix = 10 then
            pRealDouble <|> (pRationalN radix |>> fun (n, d) -> float n / float d)
        else
            pRationalN radix |>> fun (n, d) -> float n / float d

    let pComplex radix =
        let pFloatNum = pFloat radix

        choice
            [ attempt (
                  pipe3 pFloatNum (pchar '@') pFloatNum (fun r _ i ->
                      System.Numerics.Complex.FromPolarCoordinates(r, i) |> SComplex)
              )
              attempt (
                  pipe2 pFloatNum (pFloatNum .>> (pchar 'i' <|> pchar 'I')) (fun r i ->
                      System.Numerics.Complex(r, i) |> SComplex)
              )
              attempt (
                  pipe2
                      pFloatNum
                      (pstringCI "+i" >>. notFollowedBy (anyOf "nN") >>% 1.0
                       <|> (pstringCI "-i" >>. notFollowedBy (anyOf "nN") >>% -1.0))
                      (fun r i -> System.Numerics.Complex(r, i) |> SComplex)
              )
              attempt (
                  pFloatNum .>> (pchar 'i' <|> pchar 'I')
                  |>> fun i -> System.Numerics.Complex(0.0, i) |> SComplex
              )
              attempt (
                  pstringCI "+i" >>. notFollowedBy (anyOf "nN")
                  >>% (System.Numerics.Complex(0.0, 1.0) |> SComplex)
              )
              attempt (
                  pstringCI "-i" >>. notFollowedBy (anyOf "nN")
                  >>% (System.Numerics.Complex(0.0, -1.0) |> SComplex)
              ) ]

    let pNumberBody radix =
        if radix = 10 then
            choice [ attempt (pComplex radix); attempt pReal; pRational radix ]
        else
            choice [ attempt (pComplex radix); pRational radix ]

    let pExactnessOnly = choice [ pstringCI "#e" >>% true; pstringCI "#i" >>% false ]

    let pRadixOnly =
        choice
            [ pstringCI "#b" >>% 2
              pstringCI "#o" >>% 8
              pstringCI "#d" >>% 10
              pstringCI "#x" >>% 16 ]

    let pPrefix =
        choice
            [ attempt (pipe2 pExactnessOnly (pRadixOnly |> opt) (fun e r -> Some e, r))
              attempt (pipe2 pRadixOnly (pExactnessOnly |> opt) (fun r e -> e, Some r))
              preturn (None, None) ]

    let parseExactnessRadixNumber exactness radix =
        radix |> Option.defaultValue 10 |> pNumberBody
        |>> fun num ->
            match exactness with
            | Some true ->
                match num with
                | SReal x -> realToRational x
                | SComplex c -> if c.Imaginary = 0.0 then realToRational c.Real else num
                | _ -> num
            | Some false ->
                match num with
                | SRational(n, d) -> SReal(float n / float d)
                | _ -> num
            | None -> num

    let parseNumber =
        pPrefix
        >>= fun (exactness, radix) -> parseExactnessRadixNumber exactness radix |> parseWithPos

    let parseBool =
        choice
            [ attempt (stringCIReturn "#true" STrue)
              attempt (stringCIReturn "#false" SFalse)
              stringCIReturn "#t" STrue
              stringCIReturn "#f" SFalse ]
        |> parseWithPos

    let parseSymbol = pIdentifier |>> SSymbol |> parseWithPos

    let parseChar =
        pCharacter |>> (fun s -> System.Text.Rune.GetRuneAt(s, 0) |> SChar)
        |> parseWithPos

    let parseString = pString |>> (fun s -> newSString true s) |> parseWithPos

    let parseByteVector =
        let parseByte =
            parseNumber
            >>= function
                | SRational(num, den), _ when den = 1I && num >= 0I && num <= 255I -> byte num |> preturn
                | _ -> fail "bytevector elements must be exact integers between 0 and 255."

        pstringCI "#u8(" >>. pIntertokenSpace >>. many (parseByte .>> pIntertokenSpace)
        .>> pchar ')'
        |>> (List.toArray >> SByteVector)
        |> parseWithPos

    let parseDatum, parseDatumRef = createParserForwardedToRef ()

    let pWhitespace = choice [ pIntralineWhitespace; pLineEnding ]
    let pLineComment = pchar ';' >>. restOfLine true
    let pBlockComment, pBlockCommentRef = createParserForwardedToRef<unit, SState> ()

    pBlockCommentRef.Value <-
        between
            (pstring "#|")
            (pstring "|#")
            (skipMany (
                choice
                    [ attempt pBlockComment
                      skipMany1 (noneOf "#|")
                      attempt (pchar '#' .>> notFollowedBy (pchar '|')) >>% ()
                      attempt (pchar '|' .>> notFollowedBy (pchar '#')) >>% () ]
            ))

    pAtmosphereRef.Value <-
        choice
            [ pWhitespace >>% ()
              pLineComment >>% ()
              pBlockComment
              pstring "#;" >>. pIntertokenSpace >>. parseDatum >>% () ]

    let parseList =
        between (pchar '(') (pchar ')') (pIntertokenSpace >>. many (parseDatum .>> pIntertokenSpace) |>> toSPair)

    let parseDotList =
        between
            (pchar '(')
            (pchar ')')
            (pIntertokenSpace
             >>. pipe2
                 (many1 (parseDatum .>> pIntertokenSpace1))
                 (pchar '.' >>. pIntertokenSpace1 >>. parseDatum .>> pIntertokenSpace)
                 (fun xs x -> List.foldBack (fun h acc -> SPair { car = h; cdr = acc }, snd h) xs x))

    let parseVector =
        pstring "#(" >>. pIntertokenSpace >>. many (parseDatum .>> pIntertokenSpace)
        .>> pchar ')'
        |>> (List.toArray >> SVector)
        |> parseWithPos

    let parseQuoted = pchar '\'' >>. parseDatum |>> SQuote |> parseWithPos
    let parseQuasiquote = pchar '`' >>. parseDatum |>> SQuasiquote |> parseWithPos

    let parseUnquoteSplicing =
        pstring ",@" >>. parseDatum |>> SUnquoteSplicing |> parseWithPos

    let parseUnquoted = pchar ',' >>. parseDatum |>> SUnquote |> parseWithPos

    let parseDatumLabelDef =
        attempt (pchar '#' >>. (digit |> many1Chars |>> int) .>> pchar '=')
        >>= fun n -> parseDatum |>> fun d -> SDatumLabel(n, d)
        |> parseWithPos

    let parseDatumLabelRef =
        attempt (pchar '#' >>. (digit |> many1Chars |>> int) .>> pchar '#')
        |>> SDatumRef
        |> parseWithPos

    parseDatumRef.Value <-
        choice
            [ parseBool
              parseChar
              parseString
              parseByteVector
              attempt parseNumber
              attempt parseSymbol
              parseVector
              attempt parseDotList
              parseList
              parseQuoted
              parseQuasiquote
              parseUnquoteSplicing
              parseUnquoted
              parseDatumLabelDef
              parseDatumLabelRef ]

    let read input =
        match input |> run (pIntertokenSpace >>. parseDatum .>> pIntertokenSpace .>> eof) with
        | Success(res, _, _) -> Result.Ok res
        | Failure(msg, error, _) ->
            Result.Error(
                ParseError(
                    msg,
                    Some
                        { Line = int64 error.Position.Line
                          Column = int64 error.Position.Column }
                )
            )
