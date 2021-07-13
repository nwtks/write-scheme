module Read

open FParsec
open Type

type SState = unit
type Parser<'t> = Parser<'t, SState>

//let pIntralineWhitespace = pstring " " <|> pstring "\t"
//let pLineEnding =  pstring "\r" <|> pstring "\n" <|> pstring "\r\n"
//let pWhitespace = pIntralineWhitespace <|> pLineEnding

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
    choice [ pstring "\\a" |>> (fun _ -> "\u0007")
             pstring "\\b" |>> (fun _ -> "\u0008")
             pstring "\\t" |>> (fun _ -> "\u0009")
             pstring "\\n" |>> (fun _ -> "\u000a")
             pstring "\\r" |>> (fun _ -> "\u000d") ]

let pSpecialInitial = anyOf "!$%&*/:<=>?^_~"
let pExplicitSign = anyOf "+-"
let pSpecialSubsequent = choice [ pExplicitSign; anyOf ".@" ]

let pInitial = choice [ asciiLetter; pSpecialInitial ]

let pSubsequent =
    choice [ pInitial
             digit
             pSpecialSubsequent ]

let pSignSubsequent =
    choice [ pInitial
             pExplicitSign
             pchar '@' ]

let pDotSubsequent = choice [ pSignSubsequent; pchar '.' ]

let pPeculiarIdentifier =
    choice [ attempt (
                 pipe3
                     pExplicitSign
                     pSignSubsequent
                     (pSubsequent |> manyChars)
                     (fun c1 c2 s -> sprintf "%c%c%s" c1 c2 s)
             )
             attempt (
                 pipe4
                     pExplicitSign
                     (pchar '.')
                     pDotSubsequent
                     (pSubsequent |> manyChars)
                     (fun c1 c2 c3 s -> sprintf "%c%c%c%s" c1 c2 c3 s)
             )
             pExplicitSign |>> string
             pipe3 (pchar '.') pDotSubsequent (pSubsequent |> manyChars) (fun c1 c2 s -> sprintf "%c%c%s" c1 c2 s) ]

let pSymbolElement =
    choice [ noneOf "|\\" |>> string
             pMnemonicEscape
             pInlineHexEscape
             pstring "\\|" |>> (fun _ -> "|")
             pstring "\\\\" |>> (fun _ -> "\\") ]

let pIdentifier =
    choice [ pipe2 pInitial (pSubsequent |> manyChars) (fun c s -> sprintf "%c%s" c s)
             pPeculiarIdentifier
             between (pchar '|') (pchar '|') (pSymbolElement |> manyStrings) ]

let parseSymbol = pIdentifier |>> SSymbol

let parseBool =
    choice [ stringReturn "#true" (SBool true)
             stringReturn "#t" (SBool true)
             stringReturn "#false" (SBool false)
             stringReturn "#f" (SBool false) ]

let pCharacterName =
    choice [ pstring "#\\alarm" |>> (fun _ -> "\u0007")
             pstring "#\\backspace" |>> (fun _ -> "\u0008")
             pstring "#\\delete" |>> (fun _ -> "\u007f")
             pstring "#\\escape" |>> (fun _ -> "\u001b")
             pstring "#\\newline" |>> (fun _ -> "\u000a")
             pstring "#\\null" |>> (fun _ -> "\u0000")
             pstring "#\\return" |>> (fun _ -> "\u000d")
             pstring "#\\space" |>> (fun _ -> "\u0020")
             pstring "#\\tab" |>> (fun _ -> "\u0009") ]

let pCharacter =
    choice [ pCharacterName
             pstring "#\\x" >>. pHexScalarValue
             pstring "#\\" >>. anyChar |>> string ]

let parseChar = pCharacter |>> SChar

let pStringElement =
    choice [ noneOf "\"\\" |>> string
             pMnemonicEscape
             pInlineHexEscape
             pstring "\\\"" |>> (fun _ -> "\"")
             pstring "\\|" |>> (fun _ -> "|")
             pstring "\\\\" |>> (fun _ -> "\\") ]

let parseString =
    between (pchar '"') (pchar '"') (pStringElement |> manyStrings |>> SString)

let parseNumber = pfloat |>> SNumber

let parseExpr, parseExprRef = createParserForwardedToRef ()

let parseQuoted = pchar '\'' >>. parseExpr |>> SQuote
let parseUnquoted = pchar ',' >>. parseExpr |>> SUnquote

let parseEmpty = stringReturn "()" SEmpty

let parseList = sepBy parseExpr spaces1 |>> SList

let parseDotList =
    pipe3
        (parseExpr .>> spaces1)
        (sepEndBy parseExpr spaces1)
        (pchar '.' >>. spaces1 >>. parseExpr)
        (fun s1 s2 s3 -> SPair(s1 :: s2, s3))

parseExprRef
:= choice [ parseBool
            parseChar
            parseString
            parseNumber
            parseQuoted
            parseUnquoted
            attempt parseSymbol
            parseEmpty
            between
                (pchar '(')
                (pchar ')')
                (choice [ attempt parseDotList
                          parseList ]) ]

let read input =
    match run (parseExpr .>> eof) input with
    | Failure (err, _, _) -> sprintf "No match %s" err |> failwith
    | Success (v, _, _) -> v
