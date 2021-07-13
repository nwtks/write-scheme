module Tests

open Xunit
open FsUnit.Xunit
open Type
open Repl
open Builtin

[<Fact>]
let bool () =
    "#t" |> rep builtin |> should equal "#t"
    "#true" |> rep builtin |> should equal "#t"
    "#f" |> rep builtin |> should equal "#f"
    "#false" |> rep builtin |> should equal "#f"

[<Fact>]
let number () =
    "0" |> rep builtin |> should equal "0"
    "+12345" |> rep builtin |> should equal "12345"
    "-12345" |> rep builtin |> should equal "-12345"

[<Fact>]
let string () =
    "\"\"" |> rep builtin |> should equal "\"\""

    "\"12345\""
    |> rep builtin
    |> should equal "\"12345\""

    "\"1\\a2\\b3\\t4\\n5\\r6\\\"7\\\\8\\|90\""
    |> rep builtin
    |> should equal "\"1\a2\b3\t4\n5\r6\\\"7\\8|90\""

    "\"\\x3a;\""
    |> rep builtin
    |> should equal "\":\""

    "\"\\x003A;\""
    |> rep builtin
    |> should equal "\":\""

    "\"\\x3071;\""
    |> rep builtin
    |> should equal "\"ぱ\""

    "\"\\x1F600;\""
    |> rep builtin
    |> should equal "\"😀\""

[<Fact>]
let quote () =
    "(quote a)" |> rep builtin |> should equal "a"

    "(quote (+ 1 2))"
    |> rep builtin
    |> should equal "(+ 1 2)"

    "'a" |> rep builtin |> should equal "a"
    "'()" |> rep builtin |> should equal "()"

    "'(+ 1 2)"
    |> rep builtin
    |> should equal "(+ 1 2)"

    "'(quote a)"
    |> rep builtin
    |> should equal "(quote a)"

    "''a" |> rep builtin |> should equal "(quote a)"

    "'((a 1) (b 2) (c 3))"
    |> rep builtin
    |> should equal "((a 1) (b 2) (c 3))"

[<Fact>]
let symbol () =
    "'..." |> rep builtin |> should equal "..."
    "'+" |> rep builtin |> should equal "+"
    "'+soup+" |> rep builtin |> should equal "+soup+"

    "'->string"
    |> rep builtin
    |> should equal "->string"

    "'<=?" |> rep builtin |> should equal "<=?"

    "'a34kTMNs"
    |> rep builtin
    |> should equal "a34kTMNs"

    "'lambda" |> rep builtin |> should equal "lambda"

    "'list->vector"
    |> rep builtin
    |> should equal "list->vector"

    "'q" |> rep builtin |> should equal "q"
    "'V17a" |> rep builtin |> should equal "V17a"

    "'|two words|"
    |> rep builtin
    |> should equal "two words"

    "'|two\\x20;words|"
    |> rep builtin
    |> should equal "two words"

    "'the-word-recursion-has-many-meanings"
    |> rep builtin
    |> should equal "the-word-recursion-has-many-meanings"

[<Fact>]
let ``lambda`` () =
    "((lambda (x) (+ x x)) 4)"
    |> rep builtin
    |> should equal "8"

    "((lambda x x) 3 4 5 6)"
    |> rep builtin
    |> should equal "(3 4 5 6)"

    "((lambda (x y . z) z) 3 4 5 6)"
    |> rep builtin
    |> should equal "(5 6)"

    let envs = extendEnvs builtin []

    "(define reverse-subtract (lambda (x y) (- y x)))"
    |> rep envs
    |> ignore

    "(reverse-subtract 7 10)"
    |> rep envs
    |> should equal "3"

    "(define add4
      (let ((x 4))
       (lambda (y) (+ x y))))"
    |> rep envs
    |> ignore

    "(add4 6)" |> rep envs |> should equal "10"

[<Fact>]
let ``if`` () =
    "(if (> 3 2) 'yes)"
    |> rep builtin
    |> should equal "yes"

    "(if (> 2 3) 'yes 'no)"
    |> rep builtin
    |> should equal "no"

    "(if (> 3 2) (- 3 2) (+ 3 2))"
    |> rep builtin
    |> should equal "1"

    "((if #t + *) 3 4)"
    |> rep builtin
    |> should equal "7"

    "((if #f + *) 3 4)"
    |> rep builtin
    |> should equal "12"

[<Fact>]
let ``set!`` () =
    let envs = extendEnvs builtin []
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
    "(and (= 2 2) (> 2 1))"
    |> rep builtin
    |> should equal "#t"

    "(and (= 2 2) (< 2 1))"
    |> rep builtin
    |> should equal "#f"

    "(and 1 2 'c '(f g))"
    |> rep builtin
    |> should equal "(f g)"

    "(and)" |> rep builtin |> should equal "#t"

[<Fact>]
let ``or`` () =
    "(or (= 2 2) (> 2 1))"
    |> rep builtin
    |> should equal "#t"

    "(or (= 2 2) (< 2 1))"
    |> rep builtin
    |> should equal "#t"

    "(or #f #f #f)"
    |> rep builtin
    |> should equal "#f"

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

[<Fact>]
let ``begin`` () =
    let envs = extendEnvs builtin []
    "(define x 0)" |> rep envs |> ignore

    "(and
      (= x 0)
      (begin
       (set! x 5)
       (+ x 1)))"
    |> rep envs
    |> should equal "6"

[<Fact>]
let ``+`` () =
    "(+)" |> rep builtin |> should equal "0"
    "(+ 10)" |> rep builtin |> should equal "10"
    "(+ 10 2)" |> rep builtin |> should equal "12"
    "(+ 10 2 3)" |> rep builtin |> should equal "15"

[<Fact>]
let ``-`` () =
    "(-)" |> rep builtin |> should equal "0"
    "(- 10)" |> rep builtin |> should equal "-10"
    "(- 10 2)" |> rep builtin |> should equal "8"
    "(- 10 2 3)" |> rep builtin |> should equal "5"

[<Fact>]
let ``*`` () =
    "(*)" |> rep builtin |> should equal "1"
    "(* 10)" |> rep builtin |> should equal "10"
    "(* 10 2)" |> rep builtin |> should equal "20"
    "(* 10 2 3)" |> rep builtin |> should equal "60"

[<Fact>]
let ``/`` () =
    "(/)" |> rep builtin |> should equal "1"
    "(/ 10)" |> rep builtin |> should equal "0.1"
    "(/ 9 2)" |> rep builtin |> should equal "4.5"
    "(/ 12 2 3)" |> rep builtin |> should equal "2"

[<Fact>]
let ``%`` () =
    "(%)" |> rep builtin |> should equal "0"
    "(% 10)" |> rep builtin |> should equal "0"
    "(% 9 2)" |> rep builtin |> should equal "1"
    "(% 8 2)" |> rep builtin |> should equal "0"
    "(% 26 7 3)" |> rep builtin |> should equal "2"

[<Fact>]
let car () =
    "(car '(a b c))"
    |> rep builtin
    |> should equal "a"

    "(car '((a) b c d))"
    |> rep builtin
    |> should equal "(a)"

    "(car '(1 . 2))"
    |> rep builtin
    |> should equal "1"

    "(car '(1 2 . 3))"
    |> rep builtin
    |> should equal "1"

[<Fact>]
let cdr () =
    "(cdr '(a b c))"
    |> rep builtin
    |> should equal "(b c)"

    "(cdr '((a) b c d))"
    |> rep builtin
    |> should equal "(b c d)"

    "(cdr '(1 . 2))"
    |> rep builtin
    |> should equal "2"

    "(cdr '(1 2 . 3))"
    |> rep builtin
    |> should equal "(2 . 3)"

[<Fact>]
let cons () =
    "(cons 'a '())"
    |> rep builtin
    |> should equal "(a)"

    "(cons '(a) '(b c d))"
    |> rep builtin
    |> should equal "((a) b c d)"

    "(cons \"a\" '(b c))"
    |> rep builtin
    |> should equal "(\"a\" b c)"

    "(cons 'a 3)"
    |> rep builtin
    |> should equal "(a . 3)"

    "(cons '(a b) 'c)"
    |> rep builtin
    |> should equal "((a b) . c)"
