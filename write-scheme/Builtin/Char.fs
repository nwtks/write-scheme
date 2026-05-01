namespace WriteScheme.Builtins

open WriteScheme
open Type

[<AutoOpen>]
module Char =
    let isChar envs pos cont =
        function
        | [ SChar _, _ ] -> (STrue, pos) |> cont
        | _ -> (SFalse, pos) |> cont

    let compareCharsBase transformer pred name pos cont =
        List.map (function
            | SChar c, _ -> transformer c
            | x -> failwithf "'%s' is not a char in %s.%s" (x |> Print.print) name (x |> snd |> formatPosition))
        >> List.pairwise
        >> List.forall (fun (a, b) -> pred a b)
        >> toSBool
        >> fun x -> x, pos
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
        | [ SChar c, _ ] -> (c |> pred |> toSBool, pos) |> cont
        | x -> failwithf "'%s' invalid %s parameter.%s" (x |> toSPair |> Print.print) name (pos |> formatPosition)

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
                (newSRational (bigint num) 1I, pos) |> cont
            else
                (SFalse, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid digit-value parameter."

    let sCharToInteger envs pos cont =
        function
        | [ SChar c, _ ] -> (newSRational (bigint c.Value) 1I, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid char->integer parameter."

    let sIntegerToChar envs pos cont =
        function
        | [ SRational(k, d), _ ] when d = 1I && System.Text.Rune.IsValid(int k) ->
            (int k |> System.Text.Rune |> SChar, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid integer->char parameter."

    let sCharUpcase envs pos cont =
        function
        | [ SChar c, _ ] -> (System.Text.Rune.ToUpperInvariant c |> SChar, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid char-upcase parameter."

    let sCharDowncase envs pos cont =
        function
        | [ SChar c, _ ] -> (System.Text.Rune.ToLowerInvariant c |> SChar, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid char-downcase parameter."

    let sCharFoldcase envs pos cont =
        function
        | [ SChar c, _ ] -> (System.Text.Rune.ToLowerInvariant c |> SChar, pos) |> cont
        | x -> x |> invalidParameter pos "'%s' invalid char-foldcase parameter."
