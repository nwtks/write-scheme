module WriteScheme.Tests.StrTest

open Xunit
open FsUnit.Xunit

let rep = WriteScheme.Repl.rep WriteScheme.Builtin.builtin

[<Fact>]
let ``string?`` () =
    "(string? \"hello\")" |> rep |> should equal "#t"
    "(string? \"\")" |> rep |> should equal "#t"
    "(string? 'hello)" |> rep |> should equal "#f"
    "(string? 1)" |> rep |> should equal "#f"

[<Fact>]
let ``make-string`` () =
    "(make-string 3 #\\a)" |> rep |> should equal "\"aaa\""
    "(make-string 0 #\\a)" |> rep |> should equal "\"\""
    "(make-string 3)" |> rep |> should equal "\"\u0000\u0000\u0000\""

[<Fact>]
let ``string`` () =
    "(string #\\a #\\b #\\c)" |> rep |> should equal "\"abc\""
    "(string)" |> rep |> should equal "\"\""
    "(string #\\H #\\e #\\l #\\l #\\o)" |> rep |> should equal "\"Hello\""

[<Fact>]
let ``string-length`` () =
    "(string-length \"abc\")" |> rep |> should equal "3"
    "(string-length \"\")" |> rep |> should equal "0"
    "(string-length \"hello\")" |> rep |> should equal "5"

[<Fact>]
let ``string-ref`` () =
    "(string-ref \"abc\" 0)" |> rep |> should equal "#\\a"
    "(string-ref \"abc\" 2)" |> rep |> should equal "#\\c"
    "(string-ref \"hello\" 1)" |> rep |> should equal "#\\e"

[<Fact>]
let ``string=?`` () =
    "(string=? \"abc\" \"abc\")" |> rep |> should equal "#t"
    "(string=? \"abc\" \"def\")" |> rep |> should equal "#f"
    "(string=? \"abc\" \"abc\" \"abc\")" |> rep |> should equal "#t"
    "(string=? \"abc\" \"abc\" \"def\")" |> rep |> should equal "#f"

[<Fact>]
let ``string<?`` () =
    "(string<? \"abc\" \"abd\")" |> rep |> should equal "#t"
    "(string<? \"abd\" \"abc\")" |> rep |> should equal "#f"
    "(string<? \"abc\" \"abcd\")" |> rep |> should equal "#t"
    "(string<? \"abc\" \"abd\" \"abe\")" |> rep |> should equal "#t"
    "(string<? \"abc\" \"abc\" \"abd\")" |> rep |> should equal "#f"

[<Fact>]
let ``string>?`` () =
    "(string>? \"abd\" \"abc\")" |> rep |> should equal "#t"
    "(string>? \"abc\" \"abd\")" |> rep |> should equal "#f"

[<Fact>]
let ``string<=?`` () =
    "(string<=? \"abc\" \"abc\")" |> rep |> should equal "#t"
    "(string<=? \"abc\" \"abd\")" |> rep |> should equal "#t"
    "(string<=? \"abd\" \"abc\")" |> rep |> should equal "#f"

[<Fact>]
let ``string>=?`` () =
    "(string>=? \"abc\" \"abc\")" |> rep |> should equal "#t"
    "(string>=? \"abd\" \"abc\")" |> rep |> should equal "#t"
    "(string>=? \"abc\" \"abd\")" |> rep |> should equal "#f"

[<Fact>]
let ``string-ci=?`` () =
    "(string-ci=? \"ABC\" \"abc\")" |> rep |> should equal "#t"
    "(string-ci=? \"ABC\" \"def\")" |> rep |> should equal "#f"

[<Fact>]
let ``string-ci<?`` () =
    "(string-ci<? \"abc\" \"ABD\")" |> rep |> should equal "#t"
    "(string-ci<? \"ABD\" \"abc\")" |> rep |> should equal "#f"

[<Fact>]
let ``string-ci>?`` () =
    "(string-ci>? \"ABD\" \"abc\")" |> rep |> should equal "#t"
    "(string-ci>? \"abc\" \"ABD\")" |> rep |> should equal "#f"

[<Fact>]
let ``string-ci<=?`` () =
    "(string-ci<=? \"ABC\" \"abc\")" |> rep |> should equal "#t"
    "(string-ci<=? \"abc\" \"ABD\")" |> rep |> should equal "#t"
    "(string-ci<=? \"ABD\" \"abc\")" |> rep |> should equal "#f"

[<Fact>]
let ``string-ci>=?`` () =
    "(string-ci>=? \"ABC\" \"abc\")" |> rep |> should equal "#t"
    "(string-ci>=? \"ABD\" \"abc\")" |> rep |> should equal "#t"
    "(string-ci>=? \"abc\" \"ABD\")" |> rep |> should equal "#f"

[<Fact>]
let ``string-upcase`` () =
    "(string-upcase \"hello\")" |> rep |> should equal "\"HELLO\""
    "(string-upcase \"Hello World\")" |> rep |> should equal "\"HELLO WORLD\""
    "(string-upcase \"\")" |> rep |> should equal "\"\""

[<Fact>]
let ``string-downcase`` () =
    "(string-downcase \"HELLO\")" |> rep |> should equal "\"hello\""
    "(string-downcase \"Hello World\")" |> rep |> should equal "\"hello world\""
    "(string-downcase \"\")" |> rep |> should equal "\"\""

[<Fact>]
let ``string-foldcase`` () =
    "(string-foldcase \"HELLO\")" |> rep |> should equal "\"hello\""
    "(string-foldcase \"Hello\")" |> rep |> should equal "\"hello\""

[<Fact>]
let ``substring`` () =
    "(substring \"abcde\" 1 3)" |> rep |> should equal "\"bc\""
    "(substring \"abcde\" 0 5)" |> rep |> should equal "\"abcde\""
    "(substring \"abcde\" 2)" |> rep |> should equal "\"cde\""
    "(substring \"abcde\" 5)" |> rep |> should equal "\"\""

[<Fact>]
let ``string-append`` () =
    "(string-append \"abc\" \"def\")" |> rep |> should equal "\"abcdef\""
    "(string-append \"abc\" \"\" \"xyz\")" |> rep |> should equal "\"abcxyz\""
    "(string-append)" |> rep |> should equal "\"\""

[<Fact>]
let ``string->list`` () =
    "(string->list \"abc\")" |> rep |> should equal "(#\\a #\\b #\\c)"
    "(string->list \"\")" |> rep |> should equal "()"
    "(string->list \"abc\" 1)" |> rep |> should equal "(#\\b #\\c)"
    "(string->list \"abc\" 1 2)" |> rep |> should equal "(#\\b)"

[<Fact>]
let ``list->string`` () =
    "(list->string '(#\\a #\\b #\\c))" |> rep |> should equal "\"abc\""
    "(list->string '())" |> rep |> should equal "\"\""

[<Fact>]
let ``string-copy`` () =
    "(string-copy \"abc\")" |> rep |> should equal "\"abc\""
    "(string-copy \"abc\" 1)" |> rep |> should equal "\"bc\""
    "(string-copy \"abc\" 1 2)" |> rep |> should equal "\"b\""
