namespace WriteScheme.Builtins

open WriteScheme
open Type

[<AutoOpen>]
module Char =
    let isChar envs pos cont =
        function
        | [ SChar _, _ ] -> Ok(STrue, pos) |> cont
        | [ _ ] -> Ok(SFalse, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid char? parameter." |> cont

    let compareCharsBase transformer pred name pos cont =
        mapResult (function
            | SChar c, _ -> Ok(transformer c)
            | x -> x |> invalid (snd x) (sprintf "'%%s' is not a char in %s." name))
        >> Result.map (
            List.pairwise
            >> List.forall (fun (a, b) -> pred a b)
            >> toSBool
            >> fun x -> x, pos
        )
        >> cont

    let compareChars pred = compareCharsBase id pred

    let compareCharsCi pred =
        compareCharsBase (fun r -> r.ToString().ToLowerInvariant()) pred

    let sCharEq envs = compareChars (=) "char=?"
    let sCharLt envs = compareChars (<) "char<?"
    let sCharGt envs = compareChars (>) "char>?"
    let sCharLe envs = compareChars (<=) "char<=?"
    let sCharGe envs = compareChars (>=) "char>=?"
    let sCharCiEq envs = compareCharsCi (=) "char-ci=?"
    let sCharCiLt envs = compareCharsCi (<) "char-ci<?"
    let sCharCiGt envs = compareCharsCi (>) "char-ci>?"
    let sCharCiLe envs = compareCharsCi (<=) "char-ci<=?"
    let sCharCiGe envs = compareCharsCi (>=) "char-ci>=?"

    let checkCharProp pred name pos cont =
        function
        | [ SChar c, _ ] -> Ok(c |> pred |> toSBool, pos) |> cont
        | x -> x |> invalidParameter pos (sprintf "'%%s' invalid %s parameter." name) |> cont

    let sCharAlphabetic envs =
        checkCharProp System.Text.Rune.IsLetter "char-alphabetic?"

    let sCharNumeric envs =
        checkCharProp System.Text.Rune.IsNumber "char-numeric?"

    let sCharWhitespace envs =
        checkCharProp System.Text.Rune.IsWhiteSpace "char-whitespace?"

    let sCharUpperCase envs =
        checkCharProp System.Text.Rune.IsUpper "char-upper-case?"

    let sCharLowerCase envs =
        checkCharProp System.Text.Rune.IsLower "char-lower-case?"

    let sDigitValue envs pos cont =
        function
        | [ SChar c, _ ] ->
            let num = System.Text.Rune.GetNumericValue c

            if System.Text.Rune.IsDigit c && num >= 0.0 then
                Ok(newInteger (bigint num), pos) |> cont
            else
                Ok(SFalse, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid digit-value parameter." |> cont

    let sCharToInteger envs pos cont =
        function
        | [ SChar c, _ ] -> Ok(newInteger (bigint c.Value), pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid char->integer parameter." |> cont

    let sIntegerToChar envs pos cont =
        function
        | [ SRational(n, d), _ ] when d = 1I && System.Text.Rune.IsValid(int n) ->
            Ok(int n |> System.Text.Rune |> SChar, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid integer->char parameter." |> cont

    let sCharUpcase envs pos cont =
        function
        | [ SChar c, _ ] -> Ok(System.Text.Rune.ToUpperInvariant c |> SChar, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid char-upcase parameter." |> cont

    let sCharDowncase envs pos cont =
        function
        | [ SChar c, _ ] -> Ok(System.Text.Rune.ToLowerInvariant c |> SChar, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid char-downcase parameter." |> cont

    let sCharFoldcase envs pos cont =
        function
        | [ SChar c, _ ] -> Ok(System.Text.Rune.ToLowerInvariant c |> SChar, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid char-foldcase parameter." |> cont
