module WriteScheme.Tests.VectorTest

open Xunit
open FsUnit.Xunit

let rep = WriteScheme.Repl.rep WriteScheme.Builtin.builtinContext

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

    "(make-vector -1)"
    |> rep
    |> should startWith "'(-1)' invalid make-vector parameter"

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

    "(vector-ref #(1 2 3) 3)"
    |> rep
    |> should startWith "'(#(1 2 3) 3)' invalid vector-ref parameter"

    "(vector-ref #(1 2 3) -1)"
    |> rep
    |> should startWith "'(#(1 2 3) -1)' invalid vector-ref parameter"

[<Fact>]
let ``vector-set!`` () =
    "(let ((v (vector 1 2 3))) (vector-set! v 0 10) v)"
    |> rep
    |> should equal "#(10 2 3)"

    "(let ((v (vector 1 2 3))) (vector-set! v 2 20) v)"
    |> rep
    |> should equal "#(1 2 20)"

    "(let ((v (vector 1 2 3))) (vector-set! v 1 v) v)"
    |> rep
    |> should equal "#(1 ... 3)"

    "(vector-set! #(1 2 3) 3 10)"
    |> rep
    |> should startWith "'(#(1 2 3) 3 10)' invalid vector-set! parameter"

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

    "(vector-copy #(1 2 3) 1 4)"
    |> rep
    |> should startWith "'(#(1 2 3) 1 4)' invalid vector-copy parameter"

    "(vector-copy 1)"
    |> rep
    |> should startWith "'(1)' invalid vector-copy parameter"

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

    "(let ((v (vector 1 2 3 4 5))) (vector-copy! v 1 v 0 3) v)"
    |> rep
    |> should equal "#(1 1 2 3 5)"

    "(let ((v (vector 1 2 3 4 5))) (vector-copy! v 0 v 1 5) v)"
    |> rep
    |> should equal "#(2 3 4 5 5)"

    "(vector-copy! #(1 2 3) 0 #(4 5) 5)"
    |> rep
    |> should startWith "'(#(1 2 3) 0 #(4 5) 5)' invalid vector-copy! parameter"

    "(vector-copy! 1)"
    |> rep
    |> should startWith "'(1)' invalid vector-copy! parameter"

[<Fact>]
let ``vector-append`` () =
    "(vector-append '#(a b) '#(c d e) '#(f))"
    |> rep
    |> should equal "#(a b c d e f)"

    "(vector-append #(1 2) #(3 4))" |> rep |> should equal "#(1 2 3 4)"
    "(vector-append #() #(1))" |> rep |> should equal "#(1)"
    "(vector-append #(1))" |> rep |> should equal "#(1)"
    "(vector-append)" |> rep |> should equal "#()"

    "(vector-append 1)"
    |> rep
    |> should startWith "'1' is not a vector in vector-append"

[<Fact>]
let ``vector-fill!`` () =
    "(let ((v (vector 1 2 3))) (vector-fill! v 5) v)"
    |> rep
    |> should equal "#(5 5 5)"

    "(let ((v (vector 1))) (vector-fill! v 'a) v)" |> rep |> should equal "#(a)"

    "(let ((v (vector 1 2 3 4))) (vector-fill! v 0 1) v)"
    |> rep
    |> should equal "#(1 0 0 0)"

    "(let ((v (vector 1 2 3 4))) (vector-fill! v 0 1 3) v)"
    |> rep
    |> should equal "#(1 0 0 4)"

    "(vector-fill! #(1 2 3) 0 5)"
    |> rep
    |> should startWith "'(#(1 2 3) 0 5)' invalid vector-fill! parameter"

    "(vector-fill! 1)"
    |> rep
    |> should startWith "'(1)' invalid vector-fill! parameter"

[<Fact>]
let ``vector? arity`` () =
    "(vector? 1 2)" |> rep |> should startWith "'(1 2)' invalid vector? parameter"

[<Fact>]
let ``vector-length arity`` () =
    "(vector-length)"
    |> rep
    |> should startWith "'()' invalid vector-length parameter"

    "(vector-length 1 2)"
    |> rep
    |> should startWith "'(1 2)' invalid vector-length parameter"

[<Fact>]
let ``vector->list error`` () =
    "(vector->list 1)"
    |> rep
    |> should startWith "'(1)' invalid vector->list parameter"

    "(vector->list '#(a) 5)"
    |> rep
    |> should startWith "'(#(a) 5)' invalid vector->list parameter"

    "(vector->list '#(a b c) 1 2 3)"
    |> rep
    |> should startWith "'(#(a b c) 1 2 3)' invalid vector->list parameter"

[<Fact>]
let ``list->vector error`` () =
    "(list->vector 1)"
    |> rep
    |> should startWith "'(1)' invalid list->vector parameter"

    "(list->vector '(1 . 2))"
    |> rep
    |> should startWith "'((1 . 2))' invalid list->vector parameter"

[<Fact>]
let ``vector->string error`` () =
    "(vector->string 1)"
    |> rep
    |> should startWith "'(1)' invalid vector->string parameter"

    "(vector->string '#(#\\a) 5)"
    |> rep
    |> should startWith "'(#(#\\a) 5)' invalid vector->string parameter"

    "(vector->string '#(1 2 3))"
    |> rep
    |> should startWith "'1' is not a char in vector->string"

[<Fact>]
let ``string->vector error`` () =
    "(string->vector 1)"
    |> rep
    |> should startWith "'(1)' invalid string->vector parameter"

    "(string->vector \"ab\" 5)"
    |> rep
    |> should startWith "'(\"ab\" 5)' invalid string->vector parameter"
