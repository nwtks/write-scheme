module WriteScheme.Tests.SpecialFormTest

open Xunit
open FsUnit.Xunit
open WriteScheme.Builtin
open WriteScheme.Repl

[<Fact>]
let quote () =
    "(quote a)" |> rep builtin |> should equal "a"
    "(quote (+ 1 2))" |> rep builtin |> should equal "(+ 1 2)"
    "'a" |> rep builtin |> should equal "a"
    "'()" |> rep builtin |> should equal "()"
    "'(+ 1 2)" |> rep builtin |> should equal "(+ 1 2)"
    "'(quote a)" |> rep builtin |> should equal "(quote a)"
    "''a" |> rep builtin |> should equal "'a"
    "'((a 1) (b 2) (c 3))" |> rep builtin |> should equal "((a 1) (b 2) (c 3))"

[<Fact>]
let ``lambda`` () =
    "((lambda (x) (+ x x)) 4)" |> rep builtin |> should equal "8"
    "((lambda x x) 3 4 5 6)" |> rep builtin |> should equal "(3 4 5 6)"
    "((lambda (x y . z) z) 3 4 5 6)" |> rep builtin |> should equal "(5 6)"

    let envs = newEnvs ()

    "(define reverse-subtract (lambda (x y) (- y x)))" |> rep envs |> ignore
    "(reverse-subtract 7 10)" |> rep envs |> should equal "3"

    "(define add4
      (let ((x 4))
       (lambda (y) (+ x y))))"
    |> rep envs
    |> ignore

    "(add4 6)" |> rep envs |> should equal "10"

[<Fact>]
let ``if`` () =
    "(if (> 3 2) 'yes)" |> rep builtin |> should equal "yes"
    "(if (> 2 3) 'yes 'no)" |> rep builtin |> should equal "no"
    "(if (> 3 2) (- 3 2) (+ 3 2))" |> rep builtin |> should equal "1"
    "((if #t + *) 3 4)" |> rep builtin |> should equal "7"
    "((if #f + *) 3 4)" |> rep builtin |> should equal "12"

[<Fact>]
let ``set!`` () =
    let envs = newEnvs ()

    "(define x 2)" |> rep envs |> ignore
    "(+ x 1)" |> rep envs |> should equal "3"
    "(set! x 4)" |> rep envs |> ignore
    "(+ x 1)" |> rep envs |> should equal "5"

[<Fact>]
let cond () =
    "(cond ((> 3 2) 'greater) ((< 3 2) 'less))"
    |> rep builtin
    |> should equal "greater"

    "(cond ((> 3 3) 'greater) ((< 3 3) 'less) (else 'equal))"
    |> rep builtin
    |> should equal "equal"

    "(cond ('((a 1) (b 2) (c 3)) => cdr) (else #f))"
    |> rep builtin
    |> should equal "((b 2) (c 3))"

[<Fact>]
let ``and`` () =
    "(and (= 2 2) (> 2 1))" |> rep builtin |> should equal "#t"
    "(and (= 2 2) (< 2 1))" |> rep builtin |> should equal "#f"
    "(and 1 2 'c '(f g))" |> rep builtin |> should equal "(f g)"
    "(and)" |> rep builtin |> should equal "#t"

[<Fact>]
let ``or`` () =
    "(or (= 2 2) (> 2 1))" |> rep builtin |> should equal "#t"
    "(or (= 2 2) (< 2 1))" |> rep builtin |> should equal "#t"
    "(or #f #f #f)" |> rep builtin |> should equal "#f"
    "(or)" |> rep builtin |> should equal "#f"

[<Fact>]
let ``let`` () =
    "(let ((x 2) (y 3))
      (* x y))"
    |> rep builtin
    |> should equal "6"

    "(let ((x 2) (y 3))
      (let ((x 7)
        (z (+ x y)))
       (* z x)))"
    |> rep builtin
    |> should equal "35"

    "(let
      ((x 2))
      x)"
    |> rep builtin
    |> should equal "2"

    "(let
      ((square (lambda (x) (* x x))))
      (square 4))"
    |> rep builtin
    |> should equal "16"

    "(let
      ((times3
        (let
         ((n 3))
         (lambda (x) (* n x)))))
      (times3 4))"
    |> rep builtin
    |> should equal "12"

    "(let
      ((times3
        (let
         ((makemultiplier
           (lambda (n) (lambda (x) (* n x)))))
        (makemultiplier 3))))
      (times3 5))"
    |> rep builtin
    |> should equal "15"

[<Fact>]
let ``let*`` () =
    "(let ((x 2) (y 3))
      (let* ((x 7)
        (z (+ x y)))
       (* z x)))"
    |> rep builtin
    |> should equal "70"

    "(let*
      ((a 5)
       (b (* a 2))
       (c (- b 3)))
      c)"
    |> rep builtin
    |> should equal "7"

[<Fact>]
let ``letrec`` () =
    "(letrec
      ((even?
        (lambda (n)
         (if (= n 0) #t (odd? (- n 1)))))
       (odd?
        (lambda (n)
         (if (= n 0) #f (even? (- n 1))))))
      (even? 88))"
    |> rep builtin
    |> should equal "#t"

    "(letrec
      ((factorial
        (lambda (n)
         (if (= n 0) 1 (* n (factorial (- n 1)))))))
      (factorial 4))"
    |> rep builtin
    |> should equal "24"

[<Fact>]
let ``letrec*`` () =
    "(letrec*
      ((p (lambda (x) (+ 1 (q (- x 1)))))
       (q (lambda (y) (if (= y 0) 0 (+ 1 (p (- y 1))))))
       (x (p 5))
       (y x))
      y)"
    |> rep builtin
    |> should equal "5"

    "(let
       ((x 5))
       (letrec*
         ((foo (lambda (y) (bar x y)))
          (bar (lambda (a b) (+ (* a b) a))))
         (foo (+ x 3))))"
    |> rep builtin
    |> should equal "45"

[<Fact>]
let ``begin`` () =
    let envs = newEnvs ()

    "(define x 0)" |> rep envs |> ignore

    "(and
      (= x 0)
      (begin
       (set! x 5)
       (+ x 1)))"
    |> rep envs
    |> should equal "6"

[<Fact>]
let ``quasiquote`` () =
    "`(list ,(+ 1 2) 4)" |> rep builtin |> should equal "(list 3 4)"

    "(let ((name 'a)) `(list ,name ',name))"
    |> rep builtin
    |> should equal "(list a 'a)"

    "`((foo ,(- 10 3)) ,@(cdr '(c d)) . ,(car '(cons)))"
    |> rep builtin
    |> should equal "((foo 7) d . cons)"

    "(let ((foo '(foo bar)) (baz 'baz)) `(list ,@foo ,baz))"
    |> rep builtin
    |> should equal "(list foo bar baz)"

    "`(a `(b ,(+ 1 2) ,(foo ,(+ 1 3) d) e) f)"
    |> rep builtin
    |> should equal "(a `(b ,(+ 1 2) ,(foo 4 d) e) f)"

    "(let ((name1 'x) (name2 'y)) `(a `(b ,,name1 ,',name2 d) e))"
    |> rep builtin
    |> should equal "(a `(b ,x ,'y d) e)"

[<Fact>]
let ``define`` () =
    let envs = newEnvs ()

    "(define add3
       (lambda (x) (+ x 3)))"
    |> rep envs
    |> ignore

    "(add3 3)" |> rep envs |> should equal "6"

    "(define first car)" |> rep envs |> ignore
    "(first '(1 2))" |> rep envs |> should equal "1"
