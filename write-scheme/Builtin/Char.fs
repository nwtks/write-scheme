namespace WriteScheme.Builtins

open WriteScheme
open Type

[<AutoOpen>]
module Char =
    let isChar envs cont =
        function
        | [ SChar _ ] -> STrue |> cont
        | _ -> SFalse |> cont

    let compareCharsBase transformer pred name cont =
        function
        | []
        | [ _ ] -> STrue |> cont
        | args ->
            let chars =
                args
                |> List.map (function
                    | SChar c -> transformer c
                    | x -> Print.print x |> sprintf "'%s' is not a char in %s." name |> failwith)

            chars
            |> List.pairwise
            |> List.forall (fun (a, b) -> pred a b)
            |> toSBool
            |> cont

    let compareChars pred name cont args = compareCharsBase id pred name cont args

    let compareCharsCi pred name cont args =
        compareCharsBase (fun s -> s.ToLowerInvariant()) pred name cont args

    let sCharEq envs cont = compareChars (=) "char=?" cont
    let sCharLt envs cont = compareChars (<) "char<?" cont
    let sCharGt envs cont = compareChars (>) "char>?" cont
    let sCharLe envs cont = compareChars (<=) "char<=?" cont
    let sCharGe envs cont = compareChars (>=) "char>=?" cont
    let sCharCiEq envs cont = compareCharsCi (=) "char-ci=?" cont
    let sCharCiLt envs cont = compareCharsCi (<) "char-ci<?" cont
    let sCharCiGt envs cont = compareCharsCi (>) "char-ci>?" cont
    let sCharCiLe envs cont = compareCharsCi (<=) "char-ci<=?" cont
    let sCharCiGe envs cont = compareCharsCi (>=) "char-ci>=?" cont

    let checkCharProp pred name cont =
        function
        | [ SChar c ] -> System.Text.Rune.GetRuneAt(c, 0) |> pred |> toSBool |> cont
        | x ->
            toSList x
            |> Print.print
            |> sprintf "'%s' invalid parameter for %s." name
            |> failwith

    let sCharAlphabetic envs cont =
        checkCharProp System.Text.Rune.IsLetter "char-alphabetic?" cont

    let sCharNumeric envs cont =
        checkCharProp System.Text.Rune.IsNumber "char-numeric?" cont

    let sCharWhitespace envs cont =
        checkCharProp System.Text.Rune.IsWhiteSpace "char-whitespace?" cont

    let sCharUpperCase envs cont =
        checkCharProp System.Text.Rune.IsUpper "char-upper-case?" cont

    let sCharLowerCase envs cont =
        checkCharProp System.Text.Rune.IsLower "char-lower-case?" cont

    let sDigitValue envs cont =
        function
        | [ SChar c ] ->
            let rune = System.Text.Rune.GetRuneAt(c, 0)
            let num = System.Text.Rune.GetNumericValue rune

            if System.Text.Rune.IsDigit rune && num >= 0.0 then
                newSRational (bigint num) 1I |> cont
            else
                SFalse |> cont
        | x -> x |> invalidParameter "'%s' invalid digit-value parameter."

    let sCharToInteger envs cont =
        function
        | [ SChar c ] -> newSRational (bigint (System.Char.ConvertToUtf32(c, 0))) 1I |> cont
        | x -> x |> invalidParameter "'%s' invalid char->integer parameter."

    let sIntegerToChar envs cont =
        function
        | [ SRational(k, d) ] when d = 1I && System.Text.Rune.IsValid(int k) ->
            System.Char.ConvertFromUtf32(int k) |> SChar |> cont
        | x -> x |> invalidParameter "'%s' invalid integer->char parameter."

    let sCharUpcase envs cont =
        function
        | [ SChar c ] -> c.ToUpperInvariant() |> SChar |> cont
        | x -> x |> invalidParameter "'%s' invalid char-upcase parameter."

    let sCharDowncase envs cont =
        function
        | [ SChar c ] -> c.ToLowerInvariant() |> SChar |> cont
        | x -> x |> invalidParameter "'%s' invalid char-downcase parameter."

    let sCharFoldcase envs cont =
        function
        | [ SChar c ] -> c.ToLowerInvariant() |> SChar |> cont
        | x -> x |> invalidParameter "'%s' invalid char-foldcase parameter."
