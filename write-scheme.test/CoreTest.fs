module WriteScheme.Tests.CoreTest

open Xunit
open FsUnit.Xunit

let rep = WriteScheme.Repl.rep WriteScheme.Builtin.builtin

let repEnvs () =
    WriteScheme.Repl.newEnvs () |> WriteScheme.Repl.rep

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
    "(equal? '#(1 2 3) '#(1 2 3))" |> rep |> should equal "#t"
    "(equal? '#(1 2 3) '#(1 2 4))" |> rep |> should equal "#f"
    "(equal? (values 1 2) (values 1 2))" |> rep |> should equal "#t"
    "(equal? (values 1 2) (values 1 3))" |> rep |> should equal "#f"

[<Fact>]
let ``not`` () =
    "(not #t)" |> rep |> should equal "#f"
    "(not 3)" |> rep |> should equal "#f"
    "(not (list 3))" |> rep |> should equal "#f"
    "(not #f)" |> rep |> should equal "#t"
    "(not '())" |> rep |> should equal "#f"
    "(not (list))" |> rep |> should equal "#f"
    "(not 'nil)" |> rep |> should equal "#f"

[<Fact>]
let ``boolean?`` () =
    "(boolean? #f)" |> rep |> should equal "#t"
    "(boolean? #t)" |> rep |> should equal "#t"
    "(boolean? 0)" |> rep |> should equal "#f"
    "(boolean? '())" |> rep |> should equal "#f"

[<Fact>]
let ``symbol?`` () =
    "(symbol? 'foo)" |> rep |> should equal "#t"
    "(symbol? (car '(a b)))" |> rep |> should equal "#t"
    "(symbol? \"bar\")" |> rep |> should equal "#f"
    "(symbol? 'nil)" |> rep |> should equal "#t"
    "(symbol? '())" |> rep |> should equal "#f"
    "(symbol? #f)" |> rep |> should equal "#f"
