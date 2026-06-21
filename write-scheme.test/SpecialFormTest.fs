module WriteScheme.Tests.SpecialFormTest

open Xunit
open FsUnit.Xunit

let rep = WriteScheme.Repl.rep WriteScheme.Builtin.builtinContext

let newRep () =
    WriteScheme.Repl.newContext () |> WriteScheme.Repl.rep

[<Fact>]
let quote () =
    "(quote a)" |> rep |> should equal "a"
    "(quote (+ 1 2))" |> rep |> should equal "(+ 1 2)"
    "'a" |> rep |> should equal "a"
    "'()" |> rep |> should equal "()"
    "'(+ 1 2)" |> rep |> should equal "(+ 1 2)"
    "'(quote a)" |> rep |> should equal "(quote a)"
    "''a" |> rep |> should equal "'a"
    "'((a 1) (b 2) (c 3))" |> rep |> should equal "((a 1) (b 2) (c 3))"

    "(quote 1 2)" |> rep |> should startWith "'(1 2)' invalid quote parameter"

[<Fact>]
let ``lambda`` () =
    let rep = newRep ()

    "((lambda (x) (+ x x)) 4)" |> rep |> should equal "8"
    "((lambda x x))" |> rep |> should equal "()"
    "((lambda x x) 1)" |> rep |> should equal "(1)"
    "((lambda x x) 3 4 5 6)" |> rep |> should equal "(3 4 5 6)"
    "((lambda (x y . z) z) 3 4 5 6)" |> rep |> should equal "(5 6)"

    "(define reverse-subtract (lambda (x y) (- y x)))" |> rep |> ignore
    "(reverse-subtract 7 10)" |> rep |> should equal "3"

    "(define add4 (let ((x 4)) (lambda (y) (+ x y))))" |> rep |> ignore
    "(add4 6)" |> rep |> should equal "10"

    "(lambda)" |> rep |> should startWith "'()' invalid lambda parameter"
    "((lambda (1) 1) 2)" |> rep |> should startWith "'1' is not a symbol"
    "((lambda () 1) 2)" |> rep |> should startWith "Too many arguments"
    "((lambda (x) x) 1 2)" |> rep |> should startWith "Too many arguments"

[<Fact>]
let ``if`` () =
    "(if (> 3 2) 'yes)" |> rep |> should equal "yes"
    "(if (> 2 3) 'yes 'no)" |> rep |> should equal "no"
    "(if (> 3 2) (- 3 2) (+ 3 2))" |> rep |> should equal "1"
    "((if #t + *) 3 4)" |> rep |> should equal "7"
    "((if #f + *) 3 4)" |> rep |> should equal "12"
    "(if #t 42)" |> rep |> should equal "42"
    "(if #f 42 99)" |> rep |> should equal "99"

    "(if 1)" |> rep |> should startWith "'(1)' invalid if parameter"
    "(if 1 2 3 4)" |> rep |> should startWith "'(1 2 3 4)' invalid if parameter"

[<Fact>]
let ``set!`` () =
    let rep = newRep ()

    "(define x 2)" |> rep |> ignore
    "(+ x 1)" |> rep |> should equal "3"
    "(set! x 4)" |> rep |> ignore
    "(+ x 1)" |> rep |> should equal "5"

    "(set! 1 2)" |> rep |> should startWith "'(1 2)' invalid set! parameter"
    "(set!)" |> rep |> should startWith "'()' invalid set! parameter"

[<Fact>]
let ``begin`` () =
    let rep = newRep ()

    "(define x 0)" |> rep |> ignore
    "(and (= x 0) (begin (set! x 5) (+ x 1)))" |> rep |> should equal "6"
    "(begin)" |> rep |> should equal "#<unspecified>"

[<Fact>]
let ``define`` () =
    let rep = newRep ()

    "(define add3 (lambda (x) (+ x 3)))" |> rep |> should equal "#<unspecified>"
    "(add3 3)" |> rep |> should equal "6"

    "(define first car)" |> rep |> should equal "#<unspecified>"
    "(first '(1 2))" |> rep |> should equal "1"

    "(define (square x) (* x x))" |> rep |> ignore
    "(square 5)" |> rep |> should equal "25"

    "(define (add . xs) (apply + xs))" |> rep |> ignore
    "(add 1 2 3)" |> rep |> should equal "6"

    "(let () (begin (define x 1) (define y 2)) (+ x y))" |> rep |> should equal "3"
    "(let () (define x 1) (define y 2) (+ x y))" |> rep |> should equal "3"

    "(let () (define (even? n) (if (= n 0) #t (odd? (- n 1)))) (define (odd? n) (if (= n 0) #f (even? (- n 1)))) (even? 10))"
    |> rep
    |> should equal "#t"

    "(let () (define x 1) (+ x 1) (define y 2) (+ x y))"
    |> rep
    |> should startWith "Definitions must appear at the beginning of a body."

    "(let () (define x 1))"
    |> rep
    |> should startWith "Internal definitions must be followed by at least one expression."

[<Fact>]
let ``define-values`` () =
    let rep = newRep ()

    "(define-values (x y) (values 1 2))" |> rep |> ignore
    "(+ x y)" |> rep |> should equal "3"

    "(define-values (a . b) (values 10 20 30))" |> rep |> ignore
    "a" |> rep |> should equal "10"
    "b" |> rep |> should equal "(20 30)"

    "(define-values (z) 42)" |> rep |> ignore
    "z" |> rep |> should equal "42"

    "(let () (define-values (x y) (values 1 2)) (+ x y))" |> rep |> should equal "3"

    "(define-values 1)"
    |> rep
    |> should startWith "'(1)' invalid define-values parameter"

    "(define-values (x y) (values 1 2) 3)"
    |> rep
    |> should startWith "'((x y) (values 1 2) 3)' invalid define-values parameter"
