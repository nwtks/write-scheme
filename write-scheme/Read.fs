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
             digit
             pSpecialSubsequent ]

let hex2int c = (int c &&& 15) + (int c >>> 6) * 9

let pHexScalarValue =
    hex |> many1Chars
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
             pMnemonicEscape
             pInlineHexEscape
             stringReturn "\\|" "|" ]

let pIdentifier =
    choice [ pipe2 pInitial (pSubsequent |> manyChars) (fun c1 s2 -> sprintf "%c%s" c1 s2)
             pPeculiarIdentifier
             between (pchar '|') (pchar '|') (pSymbolElement |> manyStrings) ]

let pBool =
    choice [ stringReturn "#true" true
             stringReturn "#t" true
             stringReturn "#false" false
             stringReturn "#f" false ]

let pCharacterName =
    choice [ stringReturn "#\\alarm" "\u0007"
             stringReturn "#\\backspace" "\u0008"
             stringReturn "#\\delete" "\u007f"
             stringReturn "#\\escape" "\u001b"
             stringReturn "#\\newline" "\u000a"
             stringReturn "#\\null" "\u0000"
             stringReturn "#\\return" "\u000d"
             stringReturn "#\\space" "\u0020"
             stringReturn "#\\tab" "\u0009" ]

let pCharacter =
    choice [ attempt pCharacterName
             attempt (pstring "#\\x" >>. pHexScalarValue)
             pstring "#\\" >>. anyChar |>> string ]

let pStringElement =
    choice [ pMnemonicEscape
             pInlineHexEscape
             stringReturn "\\\"" "\""
             stringReturn "\\\\" "\\"
             (pchar '\\')
             >>. pipe3
                 (pIntralineWhitespace |> manyChars)
                 pLineEnding
                 (pIntralineWhitespace |> manyChars)
                 (fun s1 c2 s3 -> sprintf "%s%c%s" s1 c2 s3)
             noneOf "\"\\" |>> string ]

let parseSymbol = pIdentifier |>> SSymbol

let parseBool = pBool |>> SBool

let parseChar = pCharacter |>> SChar

let parseString =
    between (pchar '"') (pchar '"') (pStringElement |> manyStrings |>> SString)

let parseNumber = pfloat |>> SNumber

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
             parseNumber
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
