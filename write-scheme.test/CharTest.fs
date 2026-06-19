module WriteScheme.Tests.CharTest

open Xunit
open FsUnit.Xunit

let rep = WriteScheme.Repl.rep WriteScheme.Builtin.builtinContext

[<Fact>]
let ``char?`` () =
    "(char? #\\a)" |> rep |> should equal "#t"
    "(char? #\\space)" |> rep |> should equal "#t"
    "(char? 1)" |> rep |> should equal "#f"
    "(char? \"a\")" |> rep |> should equal "#f"

    "(char? #\\a #\\b)"
    |> rep
    |> should startWith "'(#\\a #\\b)' invalid char? parameter"

[<Fact>]
let ``char=?`` () =
    "(char=? #\\a #\\a)" |> rep |> should equal "#t"
    "(char=? #\\a #\\b)" |> rep |> should equal "#f"
    "(char=? #\\a #\\a #\\a)" |> rep |> should equal "#t"
    "(char=? #\\a #\\a #\\b)" |> rep |> should equal "#f"
    "(char=? #\\a)" |> rep |> should equal "#t"
    "(char=? #\\🍎 #\\🍎)" |> rep |> should equal "#t"
    "(char=? #\\a 1)" |> rep |> should startWith "'1' is not a char in char=?"

[<Fact>]
let ``char<?`` () =
    "(char<? #\\a #\\b)" |> rep |> should equal "#t"
    "(char<? #\\b #\\a)" |> rep |> should equal "#f"
    "(char<? #\\a #\\b #\\c)" |> rep |> should equal "#t"
    "(char<? #\\a #\\a #\\b)" |> rep |> should equal "#f"

[<Fact>]
let ``char>?`` () =
    "(char>? #\\b #\\a)" |> rep |> should equal "#t"
    "(char>? #\\a #\\b)" |> rep |> should equal "#f"

[<Fact>]
let ``char<=?`` () =
    "(char<=? #\\a #\\a)" |> rep |> should equal "#t"
    "(char<=? #\\a #\\b)" |> rep |> should equal "#t"
    "(char<=? #\\b #\\a)" |> rep |> should equal "#f"

[<Fact>]
let ``char>=?`` () =
    "(char>=? #\\a #\\a)" |> rep |> should equal "#t"
    "(char>=? #\\b #\\a)" |> rep |> should equal "#t"
    "(char>=? #\\a #\\b)" |> rep |> should equal "#f"

[<Fact>]
let ``char-ci=?`` () =
    "(char-ci=? #\\A #\\a)" |> rep |> should equal "#t"
    "(char-ci=? #\\A #\\b)" |> rep |> should equal "#f"
    "(char-ci=? #\\a #\\A #\\a)" |> rep |> should equal "#t"
    "(char-ci=? #\\a #\\A #\\b)" |> rep |> should equal "#f"

    "(char-ci=? #\\a \"a\")"
    |> rep
    |> should startWith "'\"a\"' is not a char in char-ci=?"

[<Fact>]
let ``char-ci<?`` () =
    "(char-ci<? #\\a #\\B)" |> rep |> should equal "#t"
    "(char-ci<? #\\B #\\a)" |> rep |> should equal "#f"

[<Fact>]
let ``char-ci>?`` () =
    "(char-ci>? #\\B #\\a)" |> rep |> should equal "#t"
    "(char-ci>? #\\a #\\B)" |> rep |> should equal "#f"

[<Fact>]
let ``char-ci<=?`` () =
    "(char-ci<=? #\\A #\\a)" |> rep |> should equal "#t"
    "(char-ci<=? #\\a #\\B)" |> rep |> should equal "#t"
    "(char-ci<=? #\\B #\\a)" |> rep |> should equal "#f"

[<Fact>]
let ``char-ci>=?`` () =
    "(char-ci>=? #\\A #\\a)" |> rep |> should equal "#t"
    "(char-ci>=? #\\B #\\a)" |> rep |> should equal "#t"
    "(char-ci>=? #\\a #\\B)" |> rep |> should equal "#f"

[<Fact>]
let ``char-alphabetic?`` () =
    "(char-alphabetic? #\\a)" |> rep |> should equal "#t"
    "(char-alphabetic? #\\A)" |> rep |> should equal "#t"
    "(char-alphabetic? #\\1)" |> rep |> should equal "#f"
    "(char-alphabetic? #\\x10400)" |> rep |> should equal "#t"
    "(char-alphabetic? #\\🍎)" |> rep |> should equal "#f"

[<Fact>]
let ``char-numeric?`` () =
    "(char-numeric? #\\1)" |> rep |> should equal "#t"
    "(char-numeric? #\\a)" |> rep |> should equal "#f"
    "(char-numeric? #\\x1D7DC)" |> rep |> should equal "#t"
    "(char-numeric? #\\🍎)" |> rep |> should equal "#f"

[<Fact>]
let ``char-whitespace?`` () =
    "(char-whitespace? #\\space)" |> rep |> should equal "#t"
    "(char-whitespace? #\\newline)" |> rep |> should equal "#t"
    "(char-whitespace? #\\a)" |> rep |> should equal "#f"
    "(char-whitespace? #\\🍎)" |> rep |> should equal "#f"

[<Fact>]
let ``char-upper-case?`` () =
    "(char-upper-case? #\\A)" |> rep |> should equal "#t"
    "(char-upper-case? #\\a)" |> rep |> should equal "#f"
    "(char-upper-case? #\\x10400)" |> rep |> should equal "#t"
    "(char-upper-case? #\\🍎)" |> rep |> should equal "#f"

[<Fact>]
let ``char-lower-case?`` () =
    "(char-lower-case? #\\a)" |> rep |> should equal "#t"
    "(char-lower-case? #\\A)" |> rep |> should equal "#f"
    "(char-lower-case? #\\x10428)" |> rep |> should equal "#t"
    "(char-lower-case? #\\🍎)" |> rep |> should equal "#f"

[<Fact>]
let ``digit-value`` () =
    "(digit-value #\\3)" |> rep |> should equal "3"
    "(digit-value #\\0)" |> rep |> should equal "0"
    "(digit-value #\\9)" |> rep |> should equal "9"
    "(digit-value #\\x1D7DC)" |> rep |> should equal "4"
    "(digit-value #\\a)" |> rep |> should equal "#f"

[<Fact>]
let ``char->integer`` () =
    "(char->integer #\\a)" |> rep |> should equal "97"
    "(char->integer #\\A)" |> rep |> should equal "65"
    "(char->integer #\\🍎)" |> rep |> should equal "127822"

[<Fact>]
let ``integer->char`` () =
    "(integer->char 97)" |> rep |> should equal "#\\a"
    "(integer->char 65)" |> rep |> should equal "#\\A"
    "(integer->char 127822)" |> rep |> should equal "#\\🍎"

    "(integer->char -1)"
    |> rep
    |> should startWith "'(-1)' invalid integer->char parameter"

    "(integer->char #x110000)"
    |> rep
    |> should startWith "'(1114112)' invalid integer->char parameter"

[<Fact>]
let ``char-upcase`` () =
    "(char-upcase #\\a)" |> rep |> should equal "#\\A"
    "(char-upcase #\\A)" |> rep |> should equal "#\\A"
    "(char-upcase #\\1)" |> rep |> should equal "#\\1"

[<Fact>]
let ``char-downcase`` () =
    "(char-downcase #\\A)" |> rep |> should equal "#\\a"
    "(char-downcase #\\a)" |> rep |> should equal "#\\a"
    "(char-downcase #\\1)" |> rep |> should equal "#\\1"

[<Fact>]
let ``char-foldcase`` () =
    "(char-foldcase #\\A)" |> rep |> should equal "#\\a"
    "(char-foldcase #\\a)" |> rep |> should equal "#\\a"

[<Fact>]
let ``char procedures error on non-char arguments`` () =
    "(digit-value 42)"
    |> rep
    |> should startWith "'(42)' invalid digit-value parameter"

    "(char->integer 42)"
    |> rep
    |> should startWith "'(42)' invalid char->integer parameter"

    "(char-upcase 42)"
    |> rep
    |> should startWith "'(42)' invalid char-upcase parameter"

    "(char-downcase 42)"
    |> rep
    |> should startWith "'(42)' invalid char-downcase parameter"

    "(char-foldcase 42)"
    |> rep
    |> should startWith "'(42)' invalid char-foldcase parameter"

    "(char-alphabetic? 42)"
    |> rep
    |> should startWith "'(42)' invalid char-alphabetic? parameter"

    "(char-numeric? 42)"
    |> rep
    |> should startWith "'(42)' invalid char-numeric? parameter"

    "(char-whitespace? 42)"
    |> rep
    |> should startWith "'(42)' invalid char-whitespace? parameter"

    "(char-upper-case? 42)"
    |> rep
    |> should startWith "'(42)' invalid char-upper-case? parameter"

    "(char-lower-case? 42)"
    |> rep
    |> should startWith "'(42)' invalid char-lower-case? parameter"
