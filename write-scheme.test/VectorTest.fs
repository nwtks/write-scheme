module WriteScheme.Tests.VectorTest

open Xunit
open FsUnit.Xunit

let rep = WriteScheme.Repl.rep WriteScheme.Builtin.builtin

[<Fact>]
let ``vector?`` () =
    "(vector? #(1 2 3))" |> rep |> should equal "#t"
    "(vector? '#(1 2 3))" |> rep |> should equal "#t"
    "(vector? '())" |> rep |> should equal "#f"
    "(vector? 1)" |> rep |> should equal "#f"

[<Fact>]
let ``make-vector`` () =
    "(make-vector 3 0)" |> rep |> should equal "#(0 0 0)"
    "(make-vector 0)" |> rep |> should equal "#()"
    "(make-vector 3 'a)" |> rep |> should equal "#(a a a)"
    "(make-vector 2 #t)" |> rep |> should equal "#(#t #t)"

[<Fact>]
let ``vector`` () =
    "(vector 1 2 3)" |> rep |> should equal "#(1 2 3)"
    "(vector)" |> rep |> should equal "#()"
    "(vector 'a 'b 'c)" |> rep |> should equal "#(a b c)"

[<Fact>]
let ``vector-length`` () =
    "(vector-length #(1 2 3))" |> rep |> should equal "3"
    "(vector-length #())" |> rep |> should equal "0"
    "(vector-length (make-vector 5))" |> rep |> should equal "5"

[<Fact>]
let ``vector-ref`` () =
    "(vector-ref #(1 2 3) 0)" |> rep |> should equal "1"
    "(vector-ref #(1 2 3) 2)" |> rep |> should equal "3"

[<Fact>]
let ``vector-set!`` () =
    "(let ((v (vector 1 2 3))) (vector-set! v 0 10) v)"
    |> rep
    |> should equal "#(10 2 3)"

    "(let ((v (vector 1 2 3))) (vector-set! v 2 20) v)"
    |> rep
    |> should equal "#(1 2 20)"

[<Fact>]
let ``vector->list`` () =
    "(vector->list #(1 2 3))" |> rep |> should equal "(1 2 3)"
    "(vector->list #())" |> rep |> should equal "()"

[<Fact>]
let ``list->vector`` () =
    "(list->vector '(1 2 3))" |> rep |> should equal "#(1 2 3)"
    "(list->vector '())" |> rep |> should equal "#()"

[<Fact>]
let ``vector->list with bounds`` () =
    "(vector->list '#(a b c d) 1)" |> rep |> should equal "(b c d)"
    "(vector->list '#(a b c d) 1 3)" |> rep |> should equal "(b c)"

[<Fact>]
let ``vector-fill! with bounds`` () =
    "(let ((v (vector 1 2 3 4))) (vector-fill! v 0 1) v)"
    |> rep
    |> should equal "#(1 0 0 0)"

    "(let ((v (vector 1 2 3 4))) (vector-fill! v 0 1 3) v)"
    |> rep
    |> should equal "#(1 0 0 4)"

[<Fact>]
let ``vector->string`` () =
    "(vector->string '#(#\\a #\\b #\\c))" |> rep |> should equal "\"abc\""

    "(vector->string '#(#\\a #\\b #\\c #\\d #\\e) 1)"
    |> rep
    |> should equal "\"bcde\""

    "(vector->string '#(#\\a #\\b #\\c #\\d #\\e) 1 4)"
    |> rep
    |> should equal "\"bcd\""

[<Fact>]
let ``string->vector`` () =
    "(string->vector \"ABC\")" |> rep |> should equal "#(#\\A #\\B #\\C)"
    "(string->vector \"abcde\" 1)" |> rep |> should equal "#(#\\b #\\c #\\d #\\e)"
    "(string->vector \"abcde\" 1 4)" |> rep |> should equal "#(#\\b #\\c #\\d)"
    "(string->vector \"🍎\")" |> rep |> should equal "#(#\\🍎)"
    "(string->vector \"a🍎b\" 1 2)" |> rep |> should equal "#(#\\🍎)"

[<Fact>]
let ``vector-copy`` () =
    "(vector-copy '#(a b c d))" |> rep |> should equal "#(a b c d)"
    "(vector-copy '#(a b c d) 1)" |> rep |> should equal "#(b c d)"
    "(vector-copy '#(a b c d) 1 3)" |> rep |> should equal "#(b c)"

[<Fact>]
let ``vector-copy!`` () =
    "(let ((a (vector 1 2 3 4 5)) (b (vector 10 20 30))) (vector-copy! a 1 b) a)"
    |> rep
    |> should equal "#(1 10 20 30 5)"

    "(let ((a (vector 1 2 3 4 5)) (b (vector 10 20 30))) (vector-copy! a 1 b 1) a)"
    |> rep
    |> should equal "#(1 20 30 4 5)"

    "(let ((a (vector 1 2 3 4 5)) (b (vector 10 20 30))) (vector-copy! a 1 b 0 2) a)"
    |> rep
    |> should equal "#(1 10 20 4 5)"

[<Fact>]
let ``vector-append`` () =
    "(vector-append '#(a b) '#(c d e) '#(f))"
    |> rep
    |> should equal "#(a b c d e f)"

    "(vector-append)" |> rep |> should equal "#()"

[<Fact>]
let ``vector-fill!`` () =
    "(let ((v (vector 1 2 3))) (vector-fill! v 5) v)"
    |> rep
    |> should equal "#(5 5 5)"

    "(let ((v (vector 1))) (vector-fill! v 'a) v)" |> rep |> should equal "#(a)"
