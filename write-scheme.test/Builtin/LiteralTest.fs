module WriteScheme.Tests.LiteralTest

open Xunit
open FsUnit.Xunit
open WriteScheme.Builtin
open WriteScheme.Repl

[<Fact>]
let number () =
    "0" |> rep builtin |> should equal "0"
    "1" |> rep builtin |> should equal "1"
    "+1" |> rep builtin |> should equal "1"
    "-1" |> rep builtin |> should equal "-1"
    "1.0" |> rep builtin |> should equal "1"
    "1." |> rep builtin |> should equal "1"
    "0.1" |> rep builtin |> should equal "0.1"
    ".1" |> rep builtin |> should equal "0.1"
    "-100.0e-3" |> rep builtin |> should equal "-0.1"
    "-0.1" |> rep builtin |> should equal "-0.1"
    "-.1" |> rep builtin |> should equal "-0.1"
    "100.0e-3" |> rep builtin |> should equal "0.1"
    "1e2" |> rep builtin |> should equal "100"
    "1.0E2" |> rep builtin |> should equal "100"
    "1.0e+2" |> rep builtin |> should equal "100"
    "+nan.0" |> rep builtin |> should equal "+nan.0"
    "+NaN.0" |> rep builtin |> should equal "+nan.0"
    "+inf.0" |> rep builtin |> should equal "+inf.0"
    "+Inf.0" |> rep builtin |> should equal "+inf.0"
    "-inf.0" |> rep builtin |> should equal "-inf.0"
    "-INF.0" |> rep builtin |> should equal "-inf.0"
    "1/2" |> rep builtin |> should equal "1/2"
    "10/2" |> rep builtin |> should equal "5"
    "-1/2" |> rep builtin |> should equal "-1/2"
    "0/2" |> rep builtin |> should equal "0"
    "#x11" |> rep builtin |> should equal "17"
    "#X11" |> rep builtin |> should equal "17"
    "#d11" |> rep builtin |> should equal "11"
    "#D11" |> rep builtin |> should equal "11"
    "#o11" |> rep builtin |> should equal "9"
    "#O11" |> rep builtin |> should equal "9"
    "#b11" |> rep builtin |> should equal "3"
    "#B11" |> rep builtin |> should equal "3"
    "#o7" |> rep builtin |> should equal "7"
    "#xA" |> rep builtin |> should equal "10"
    "#Xf" |> rep builtin |> should equal "15"
    "#x-10" |> rep builtin |> should equal "-16"
    "#d-10" |> rep builtin |> should equal "-10"
    "#o-10" |> rep builtin |> should equal "-8"
    "#b-10" |> rep builtin |> should equal "-2"
    "#d1." |> rep builtin |> should equal "1"
    "#d.1" |> rep builtin |> should equal "0.1"
    "#x10/2" |> rep builtin |> should equal "8"
    "#x11/2" |> rep builtin |> should equal "17/2"
    "#d11/2" |> rep builtin |> should equal "11/2"
    "#o11/2" |> rep builtin |> should equal "9/2"
    "#b11/10" |> rep builtin |> should equal "3/2"

[<Fact>]
let bool () =
    "#t" |> rep builtin |> should equal "#t"
    "#true" |> rep builtin |> should equal "#t"
    "#f" |> rep builtin |> should equal "#f"
    "#false" |> rep builtin |> should equal "#f"

[<Fact>]
let symbol () =
    "'..." |> rep builtin |> should equal "..."
    "'+" |> rep builtin |> should equal "+"
    "'+soup+" |> rep builtin |> should equal "+soup+"
    "'->string" |> rep builtin |> should equal "->string"
    "'<=?" |> rep builtin |> should equal "<=?"
    "'a34kTMNs" |> rep builtin |> should equal "a34kTMNs"
    "'lambda" |> rep builtin |> should equal "lambda"
    "'list->vector" |> rep builtin |> should equal "list->vector"
    "'q" |> rep builtin |> should equal "q"
    "'V17a" |> rep builtin |> should equal "V17a"
    "'|two words|" |> rep builtin |> should equal "two words"
    "'|two\\x20;words|" |> rep builtin |> should equal "two words"

    "'the-word-recursion-has-many-meanings"
    |> rep builtin
    |> should equal "the-word-recursion-has-many-meanings"

[<Fact>]
let string () =
    "\"\"" |> rep builtin |> should equal "\"\""
    "\"12345\"" |> rep builtin |> should equal "\"12345\""

    "\"1\\a2\\b3\\t4\\n5\\r6\\\"7\\\\8|90\""
    |> rep builtin
    |> should equal "\"1\a2\b3\t4\n5\r6\\\"7\\8|90\""

    "\"\\x3a;\"" |> rep builtin |> should equal "\":\""
    "\"\\x003A;\"" |> rep builtin |> should equal "\":\""
    "\"\\x3071;\"" |> rep builtin |> should equal "\"ぱ\""
    "\"\\x1F600;\"" |> rep builtin |> should equal "\"😀\""
