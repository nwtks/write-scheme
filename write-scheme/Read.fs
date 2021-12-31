module Read

open FParsec
open Type

type SState = unit
type Parser<'t> = Parser<'t, SState>

let pIntralineWhitespace = anyOf " \t"
let pLineEnding = newline

let pWhitespace =
    choice [ pIntralineWhitespace
             pLineEnding ]

let pLineComment = pchar ';' >>. restOfLine true

let pBlockComment =
    pstring "#|"
    >>. charsTillString "|#" true System.Int32.MaxValue

let pAtmosphere =
    choice [ pWhitespace |>> string
             pLineComment
             pBlockComment ]

let pIntertokenSpace = many pAtmosphere
let pIntertokenSpace1 = many1 pAtmosphere
let pLetter = asciiLetter
let pSpecialInitial = anyOf "!$%&*/:<=>?^_~"
let pInitial = choice [ pLetter; pSpecialInitial ]
let pDigit = digit
let pHexDigit = hex
let pExplicitSign = anyOf "+-"
let pSpecialSubsequent = choice [ pExplicitSign; anyOf ".@" ]

let pSubsequent =
    choice [ pInitial
             pDigit
             pSpecialSubsequent ]

let hex2int c = (int c &&& 15) + (int c >>> 6) * 9

let pHexScalarValue =
    pHexDigit |> many1Chars
    |>> (fun s ->
        s.ToCharArray()
        |> Seq.map hex2int
        |> Seq.fold (fun acc a -> acc * 16 + a) 0
        |> System.Char.ConvertFromUtf32)

let pInlineHexEscape =
    between (pstring "\\x") (pchar ';') pHexScalarValue

let pMnemonicEscape =
    choice [ stringReturn "\\a" "\u0007"
             stringReturn "\\b" "\u0008"
             stringReturn "\\t" "\u0009"
             stringReturn "\\n" "\u000a"
             stringReturn "\\r" "\u000d" ]

let pSignSubsequent =
    choice [ pInitial
             pExplicitSign
             pchar '@' ]

let pDotSubsequent = choice [ pSignSubsequent; pchar '.' ]

let pPeculiarIdentifier =
    choice [ attempt (
                 pipe3 pExplicitSign pSignSubsequent (pSubsequent |> manyChars) (fun c1 c2 s3 ->
                     sprintf "%c%c%s" c1 c2 s3)
             )
             attempt (
                 pipe4 pExplicitSign (pchar '.') pDotSubsequent (pSubsequent |> manyChars) (fun c1 c2 c3 s4 ->
                     sprintf "%c%c%c%s" c1 c2 c3 s4)
             )
             pExplicitSign |>> string
             pipe3 (pchar '.') pDotSubsequent (pSubsequent |> manyChars) (fun c1 c2 s3 -> sprintf "%c%c%s" c1 c2 s3) ]

let pSymbolElement =
    choice [ noneOf "|\\" |>> string
             stringReturn "\\|" "|"
             pMnemonicEscape
             pInlineHexEscape ]

let pIdentifier =
    choice [ pipe2 pInitial (pSubsequent |> manyChars) (fun c1 s2 -> sprintf "%c%s" c1 s2)
             pPeculiarIdentifier
             between (pchar '|') (pchar '|') (pSymbolElement |> manyStrings) ]

let pCharacterName =
    choice [ stringCIReturn "#\\alarm" "\u0007"
             stringCIReturn "#\\backspace" "\u0008"
             stringCIReturn "#\\delete" "\u007f"
             stringCIReturn "#\\escape" "\u001b"
             stringCIReturn "#\\newline" "\u000a"
             stringCIReturn "#\\null" "\u0000"
             stringCIReturn "#\\return" "\u000d"
             stringCIReturn "#\\space" "\u0020"
             stringCIReturn "#\\tab" "\u0009" ]

let pCharacter =
    choice [ attempt pCharacterName
             attempt (pstringCI "#\\x" >>. pHexScalarValue)
             pstring "#\\" >>. anyChar |>> string ]

let pStringElement =
    choice [ noneOf "\"\\" |>> string
             stringReturn "\\\"" "\""
             stringReturn "\\\\" "\\"
             pMnemonicEscape
             pInlineHexEscape
             (pchar '\\')
             >>. pipe3
                 (pIntralineWhitespace |> manyChars)
                 pLineEnding
                 (pIntralineWhitespace |> manyChars)
                 (fun s1 c2 s3 -> sprintf "%s%c%s" s1 c2 s3) ]

let pString =
    between (pchar '"') (pchar '"') (pStringElement |> manyStrings)

let pSign =
    anyOf "+-" |> opt |>> Option.defaultValue '+'

let toBigInteger radix n =
    n
    |> Seq.fold
        (fun acc c ->
            acc * radix
            + System.Numerics.BigInteger.Parse(c |> string))
        0I

let pUinteger10 =
    pDigit |> many1Chars
    |>> (fun s -> System.Numerics.BigInteger.Parse(s))

let pUinteger16 =
    pHexDigit |> many1Chars
    |>> (fun s -> System.Numerics.BigInteger.Parse(s, System.Globalization.NumberStyles.HexNumber))

let pUinteger2 = anyOf "01" |> many1 |>> toBigInteger 2I

let pUinteger8 =
    anyOf "01234567" |> many1 |>> toBigInteger 8I

let pRationalN pUinteger =
    choice [ attempt (
                 pipe4 pSign pUinteger (pchar '/') pUinteger (fun c1 n2 _ n4 -> ((if c1 = '-' then -n2 else n2), n4))
             )
             pipe2 pSign pUinteger (fun c1 n2 -> ((if c1 = '-' then -n2 else n2), 1I)) ]

let pRational =
    choice [ pstringCI "#x" >>. pRationalN pUinteger16
             pstringCI "#b" >>. pRationalN pUinteger2
             pstringCI "#o" >>. pRationalN pUinteger8
             pstringCI "#d" >>. pRationalN pUinteger10
             pRationalN pUinteger10 ]

let pSuffix =
    anyOf "Ee"
    >>. pipe2 (pSign |>> string) (pDigit |> many1Chars) (fun s1 s2 -> "E" + s1 + s2)

let pDecimal10 =
    choice [ attempt (
                 pipe5
                     (pSign |>> string)
                     (pDigit |> many1Chars)
                     (pchar '.')
                     (pDigit |> many1Chars)
                     (pSuffix |> opt)
                     (fun s1 s2 _ s4 s5 -> System.Double.Parse(s1 + s2 + "." + s4 + (Option.defaultValue "" s5)))
             )
             attempt (
                 pipe4 (pSign |>> string) (pDigit |> many1Chars) (pchar '.') (pSuffix |> opt) (fun s1 s2 _ s4 ->
                     System.Double.Parse(s1 + s2 + (Option.defaultValue "" s4)))
             )
             pipe4 (pSign |>> string) (pchar '.') (pDigit |> many1Chars) (pSuffix |> opt) (fun s1 _ s3 s4 ->
                 System.Double.Parse(s1 + "0." + s3 + (Option.defaultValue "" s4))) ]

let parseSymbol = pIdentifier |>> SSymbol
let parseChar = pCharacter |>> SChar
let parseString = pString |>> SString

let parseBool =
    choice [ stringCIReturn "#true" STrue
             stringCIReturn "#t" STrue
             stringCIReturn "#false" SFalse
             stringCIReturn "#f" SFalse ]

let parseRational =
    pRational |>> (fun (n1, n2) -> newRational n1 n2)

let parseReal =
    choice [ stringCIReturn "+inf.0" SPositiveInfinity
             stringCIReturn "-inf.0" SNegativeInfinity
             stringCIReturn "+nan.0" SNaN
             stringCIReturn "-nan.0" SNaN
             pstringCI "#d" >>. pDecimal10 |>> SReal
             pDecimal10 |>> SReal ]

let parseDatum, parseDatumRef = createParserForwardedToRef ()
let parseQuoted = pchar '\'' >>. parseDatum |>> SQuote
let parseQuasiquote = pchar '`' >>. parseDatum |>> SQuasiquote
let parseUnquoted = pchar ',' >>. parseDatum |>> SUnquote

let parseUnquoteSplicing =
    pstring ",@" >>. parseDatum |>> SUnquoteSplicing

let parseList =
    between
        (pchar '(')
        (pchar ')')
        (pIntertokenSpace
         >>. many (parseDatum .>> pIntertokenSpace)
         |>> function
             | [] -> SEmpty
             | x -> SList x)

let parseDotList =
    between
        (pchar '(')
        (pchar ')')
        (pIntertokenSpace
         >>. pipe2
             (many1 (parseDatum .>> pIntertokenSpace1))
             (pchar '.' >>. pIntertokenSpace1 >>. parseDatum
              .>> pIntertokenSpace)
             (fun d1 d2 -> SPair(d1, d2)))

parseDatumRef.Value <-
    choice [ parseQuoted
             parseQuasiquote
             attempt parseUnquoteSplicing
             parseUnquoted
             attempt parseDotList
             parseList
             parseBool
             parseChar
             parseString
             attempt parseReal
             attempt parseRational
             attempt parseSymbol ]

let read input =
    match
        run
            (pIntertokenSpace >>. parseDatum
             .>> pIntertokenSpace
             .>> eof)
            input
        with
    | Failure (err, _, _) -> sprintf "No match %s" err |> failwith
    | Success (v, _, _) -> v
