module WriteScheme.Tests.LiteralTest

open Xunit
open FsUnit.Xunit

let rep = WriteScheme.Repl.rep WriteScheme.Builtin.builtin

[<Fact>]
let number () =
    "0" |> rep |> should equal "0"
    "1" |> rep |> should equal "1"
    "+1" |> rep |> should equal "1"
    "-1" |> rep |> should equal "-1"
    "1.0" |> rep |> should equal "1"
    "1." |> rep |> should equal "1"
    "0.1" |> rep |> should equal "0.1"
    ".1" |> rep |> should equal "0.1"
    "-100.0e-3" |> rep |> should equal "-0.1"
    "-0.1" |> rep |> should equal "-0.1"
    "-.1" |> rep |> should equal "-0.1"
    "100.0e-3" |> rep |> should equal "0.1"
    "1e2" |> rep |> should equal "100"
    "1.0E2" |> rep |> should equal "100"
    "1.0e+2" |> rep |> should equal "100"
    "+nan.0" |> rep |> should equal "+nan.0"
    "+NaN.0" |> rep |> should equal "+nan.0"
    "+inf.0" |> rep |> should equal "+inf.0"
    "+Inf.0" |> rep |> should equal "+inf.0"
    "-inf.0" |> rep |> should equal "-inf.0"
    "-INF.0" |> rep |> should equal "-inf.0"
    "1/2" |> rep |> should equal "1/2"
    "10/2" |> rep |> should equal "5"
    "-1/2" |> rep |> should equal "-1/2"
    "0/2" |> rep |> should equal "0"
    "#x11" |> rep |> should equal "17"
    "#X11" |> rep |> should equal "17"
    "#d11" |> rep |> should equal "11"
    "#D11" |> rep |> should equal "11"
    "#o11" |> rep |> should equal "9"
    "#O11" |> rep |> should equal "9"
    "#b11" |> rep |> should equal "3"
    "#B11" |> rep |> should equal "3"
    "#o7" |> rep |> should equal "7"
    "#xA" |> rep |> should equal "10"
    "#Xf" |> rep |> should equal "15"
    "#x-10" |> rep |> should equal "-16"
    "#d-10" |> rep |> should equal "-10"
    "#o-10" |> rep |> should equal "-8"
    "#b-10" |> rep |> should equal "-2"
    "#d1." |> rep |> should equal "1"
    "#d.1" |> rep |> should equal "0.1"
    "#x10/2" |> rep |> should equal "8"
    "#x11/2" |> rep |> should equal "17/2"
    "#d11/2" |> rep |> should equal "11/2"
    "#o11/2" |> rep |> should equal "9/2"
    "#b11/10" |> rep |> should equal "3/2"

[<Fact>]
let bool () =
    "#t" |> rep |> should equal "#t"
    "#true" |> rep |> should equal "#t"
    "#f" |> rep |> should equal "#f"
    "#false" |> rep |> should equal "#f"

[<Fact>]
let symbol () =
    "'..." |> rep |> should equal "..."
    "'+" |> rep |> should equal "+"
    "'+soup+" |> rep |> should equal "+soup+"
    "'->string" |> rep |> should equal "->string"
    "'<=?" |> rep |> should equal "<=?"
    "'a34kTMNs" |> rep |> should equal "a34kTMNs"
    "'lambda" |> rep |> should equal "lambda"
    "'list->vector" |> rep |> should equal "list->vector"
    "'q" |> rep |> should equal "q"
    "'V17a" |> rep |> should equal "V17a"
    "'|two words|" |> rep |> should equal "two words"
    "'|two\\x20;words|" |> rep |> should equal "two words"

    "'the-word-recursion-has-many-meanings"
    |> rep
    |> should equal "the-word-recursion-has-many-meanings"

[<Fact>]
let string () =
    "\"\"" |> rep |> should equal "\"\""
    "\"12345\"" |> rep |> should equal "\"12345\""

    "\"1\\a2\\b3\\t4\\n5\\r6\\\"7\\\\8|90\""
    |> rep
    |> should equal "\"1\a2\b3\t4\n5\r6\\\"7\\8|90\""

    "\"\\x3a;\"" |> rep |> should equal "\":\""
    "\"\\x003A;\"" |> rep |> should equal "\":\""
    "\"\\x3071;\"" |> rep |> should equal "\"ぱ\""
    "\"\\x1F600;\"" |> rep |> should equal "\"😀\""
