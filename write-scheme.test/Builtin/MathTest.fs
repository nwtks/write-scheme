module WriteScheme.Tests.MathTest

open Xunit
open FsUnit.Xunit

let rep = WriteScheme.Repl.rep WriteScheme.Builtin.builtin

[<Fact>]
let ``eqv?`` () =
    "(eqv? 'a 'a)" |> rep |> should equal "#t"
    "(eqv? 'a 'b)" |> rep |> should equal "#f"
    "(eqv? 2 2)" |> rep |> should equal "#t"
    "(eqv? 2 3)" |> rep |> should equal "#f"
    "(eqv? \"a\" \"a\")" |> rep |> should equal "#f"
    "(let ((p \"a\")) (eqv? p p))" |> rep |> should equal "#t"
    "(eqv? '() '())" |> rep |> should equal "#t"
    "(eqv? 100000000 100000000)" |> rep |> should equal "#t"
    "(eqv? (cons 1 2) (cons 1 2))" |> rep |> should equal "#f"
    "(eqv? (lambda () 1) (lambda () 2))" |> rep |> should equal "#f"
    "(let ((p (lambda (x) x))) (eqv? p p))" |> rep |> should equal "#t"
    "(eqv? #f 'nil)" |> rep |> should equal "#f"

[<Fact>]
let ``equal?`` () =
    "(equal? 'a 'a)" |> rep |> should equal "#t"
    "(equal? '(a) '(a))" |> rep |> should equal "#t"
    "(equal? 'a '(a))" |> rep |> should equal "#f"
    "(equal? '(a (b) c) '(a (b) c))" |> rep |> should equal "#t"
    "(equal? '(a b c) '(a (b) c))" |> rep |> should equal "#f"
    "(equal? \"abc\" \"abc\")" |> rep |> should equal "#t"
    "(equal? \"a\" \"abc\")" |> rep |> should equal "#f"
    "(equal? 2 2)" |> rep |> should equal "#t"
    "(equal? 3 2)" |> rep |> should equal "#f"

[<Fact>]
let ``+`` () =
    "(+)" |> rep |> should equal "0"
    "(+ 10)" |> rep |> should equal "10"
    "(+ 10 2)" |> rep |> should equal "12"
    "(+ 10 2 3)" |> rep |> should equal "15"

[<Fact>]
let ``*`` () =
    "(*)" |> rep |> should equal "1"
    "(* 10)" |> rep |> should equal "10"
    "(* 10 2)" |> rep |> should equal "20"
    "(* 10 2 3)" |> rep |> should equal "60"

[<Fact>]
let ``-`` () =
    "(-)" |> rep |> should equal "0"
    "(- 10)" |> rep |> should equal "-10"
    "(- 10 2)" |> rep |> should equal "8"
    "(- 10 2 3)" |> rep |> should equal "5"

[<Fact>]
let ``/`` () =
    "(/)" |> rep |> should equal "1"
    "(/ 10)" |> rep |> should equal "1/10"
    "(/ 9 2)" |> rep |> should equal "9/2"
    "(/ 12 2 3)" |> rep |> should equal "2"
    "(/ 3 4 5)" |> rep |> should equal "3/20"
