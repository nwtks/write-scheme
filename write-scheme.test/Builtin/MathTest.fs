module WriteScheme.Tests.MathTest

open Xunit
open FsUnit.Xunit
open WriteScheme.Builtin
open WriteScheme.Repl

[<Fact>]
let ``eqv?`` () =
    "(eqv? 'a 'a)" |> rep builtin |> should equal "#t"
    "(eqv? 'a 'b)" |> rep builtin |> should equal "#f"
    "(eqv? 2 2)" |> rep builtin |> should equal "#t"
    "(eqv? 2 3)" |> rep builtin |> should equal "#f"
    "(eqv? \"a\" \"a\")" |> rep builtin |> should equal "#f"
    "(let ((p \"a\")) (eqv? p p))" |> rep builtin |> should equal "#t"
    "(eqv? '() '())" |> rep builtin |> should equal "#t"
    "(eqv? 100000000 100000000)" |> rep builtin |> should equal "#t"
    "(eqv? (cons 1 2) (cons 1 2))" |> rep builtin |> should equal "#f"
    "(eqv? (lambda () 1) (lambda () 2))" |> rep builtin |> should equal "#f"
    "(let ((p (lambda (x) x))) (eqv? p p))" |> rep builtin |> should equal "#t"
    "(eqv? #f 'nil)" |> rep builtin |> should equal "#f"

[<Fact>]
let ``equal?`` () =
    "(equal? 'a 'a)" |> rep builtin |> should equal "#t"
    "(equal? '(a) '(a))" |> rep builtin |> should equal "#t"
    "(equal? 'a '(a))" |> rep builtin |> should equal "#f"
    "(equal? '(a (b) c) '(a (b) c))" |> rep builtin |> should equal "#t"
    "(equal? '(a b c) '(a (b) c))" |> rep builtin |> should equal "#f"
    "(equal? \"abc\" \"abc\")" |> rep builtin |> should equal "#t"
    "(equal? \"a\" \"abc\")" |> rep builtin |> should equal "#f"
    "(equal? 2 2)" |> rep builtin |> should equal "#t"
    "(equal? 3 2)" |> rep builtin |> should equal "#f"

[<Fact>]
let ``+`` () =
    "(+)" |> rep builtin |> should equal "0"
    "(+ 10)" |> rep builtin |> should equal "10"
    "(+ 10 2)" |> rep builtin |> should equal "12"
    "(+ 10 2 3)" |> rep builtin |> should equal "15"

[<Fact>]
let ``*`` () =
    "(*)" |> rep builtin |> should equal "1"
    "(* 10)" |> rep builtin |> should equal "10"
    "(* 10 2)" |> rep builtin |> should equal "20"
    "(* 10 2 3)" |> rep builtin |> should equal "60"

[<Fact>]
let ``-`` () =
    "(-)" |> rep builtin |> should equal "0"
    "(- 10)" |> rep builtin |> should equal "-10"
    "(- 10 2)" |> rep builtin |> should equal "8"
    "(- 10 2 3)" |> rep builtin |> should equal "5"

[<Fact>]
let ``/`` () =
    "(/)" |> rep builtin |> should equal "1"
    "(/ 10)" |> rep builtin |> should equal "1/10"
    "(/ 9 2)" |> rep builtin |> should equal "9/2"
    "(/ 12 2 3)" |> rep builtin |> should equal "2"
    "(/ 3 4 5)" |> rep builtin |> should equal "3/20"
