namespace WriteScheme.Builtins

open WriteScheme
open Type

[<AutoOpen>]
module Char =
    let isChar envs cont =
        function
        | [ SChar _ ] -> STrue |> cont
        | _ -> SFalse |> cont

    let compareChars (pred: string -> string -> bool) name envs cont =
        function
        | []
        | [ _ ] -> STrue |> cont
        | args ->
            let chars =
                args
                |> List.map (function
                    | SChar c -> c
                    | x -> Print.print x |> sprintf "'%s' is not a char in %s." name |> failwith)

            chars
            |> List.pairwise
            |> List.forall (fun (a, b) -> pred a b)
            |> toSBool
            |> cont

    let sCharEq envs cont = compareChars (=) "char=?" envs cont
    let sCharLt envs cont = compareChars (<) "char<?" envs cont
    let sCharGt envs cont = compareChars (>) "char>?" envs cont
    let sCharLe envs cont = compareChars (<=) "char<=?" envs cont
    let sCharGe envs cont = compareChars (>=) "char>=?" envs cont

    let compareCharsCi (pred: string -> string -> bool) name envs cont =
        function
        | []
        | [ _ ] -> STrue |> cont
        | args ->
            let chars =
                args
                |> List.map (function
                    | SChar c -> c.ToLowerInvariant()
                    | x -> Print.print x |> sprintf "'%s' is not a char in %s." name |> failwith)

            chars
            |> List.pairwise
            |> List.forall (fun (a, b) -> pred a b)
            |> toSBool
            |> cont

    let sCharCiEq envs cont =
        compareCharsCi (=) "char-ci=?" envs cont

    let sCharCiLt envs cont =
        compareCharsCi (<) "char-ci<?" envs cont

    let sCharCiGt envs cont =
        compareCharsCi (>) "char-ci>?" envs cont

    let sCharCiLe envs cont =
        compareCharsCi (<=) "char-ci<=?" envs cont

    let sCharCiGe envs cont =
        compareCharsCi (>=) "char-ci>=?" envs cont

    let checkCharProp (pred: char -> bool) name envs cont =
        function
        | [ SChar c ] -> c.[0] |> pred |> toSBool |> cont
        | x ->
            toSList x
            |> Print.print
            |> sprintf "'%s' invalid parameter for %s." name
            |> failwith

    let sCharAlphabetic envs cont =
        checkCharProp System.Char.IsLetter "char-alphabetic?" envs cont

    let sCharNumeric envs cont =
        checkCharProp System.Char.IsDigit "char-numeric?" envs cont

    let sCharWhitespace envs cont =
        checkCharProp System.Char.IsWhiteSpace "char-whitespace?" envs cont

    let sCharUpperCase envs cont =
        checkCharProp System.Char.IsUpper "char-upper-case?" envs cont

    let sCharLowerCase envs cont =
        checkCharProp System.Char.IsLower "char-lower-case?" envs cont

    let sDigitValue envs cont =
        function
        | [ SChar c ] ->
            let num = System.Char.GetNumericValue(c.[0])

            if System.Char.IsDigit(c.[0]) && num >= 0.0 then
                newSRational (bigint (int num)) 1I |> cont
            else
                SFalse |> cont
        | x -> x |> invalidParameter "'%s' invalid digit-value parameter."

    let sCharToInteger envs cont =
        function
        | [ SChar c ] -> newSRational (bigint (System.Char.ConvertToUtf32(c, 0))) 1I |> cont
        | x -> x |> invalidParameter "'%s' invalid char->integer parameter."

    let sIntegerToChar envs cont =
        function
        | [ SRational(k, d) ] when d = 1I && k >= 0I && k <= bigint 0x10FFFF ->
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
