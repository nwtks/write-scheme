module WriteScheme.Tests.ConditionalTest

open Xunit
open FsUnit.Xunit

let rep = WriteScheme.Repl.rep WriteScheme.Builtin.builtinContext

let newRep () =
    WriteScheme.Repl.newContext [] |> WriteScheme.Repl.rep

[<Fact>]
let cond () =
    "(cond ((> 3 2) 'greater) ((< 3 2) 'less))" |> rep |> should equal "greater"

    "(cond ((> 3 3) 'greater) ((< 3 3) 'less) (else 'equal))"
    |> rep
    |> should equal "equal"

    "(cond ('((a 1) (b 2) (c 3)) => cdr) (else #f))"
    |> rep
    |> should equal "((b 2) (c 3))"

    "(cond ((> 1 2) 'a) ((> 1 3) 'b))" |> rep |> should equal "#<unspecified>"
    "(cond (#t))" |> rep |> should equal "#t"
    "(cond (1))" |> rep |> should equal "1"
    "(cond (#f) (else 42))" |> rep |> should equal "42"

[<Fact>]
let ``case`` () =
    "(case (* 2 3) ((2 3 5 7) 'prime) ((1 4 6 8 9) 'composite))"
    |> rep
    |> should equal "composite"

    "(case (car '(c d)) ((a e i o u) 'vowel) ((w y) 'semivowel) (else => (lambda (x) x)))"
    |> rep
    |> should equal "c"

    "(case (car '(c d)) ((a) 'a) ((b) 'b))" |> rep |> should equal "#<unspecified>"
    "(case (* 2 3) ((1 4 6 8 9) => (lambda (x) x)))" |> rep |> should equal "6"
    "(case 1)" |> rep |> should equal "#<unspecified>"
    "(case 1 (else => (lambda (x) x)))" |> rep |> should equal "1"
    "(case 1 (else 1 2 3))" |> rep |> should equal "3"
    "(case 1 ((1 2) => (lambda (x) x)))" |> rep |> should equal "1"
    "(case 3 ((1 2) => (lambda (x) x)) (else 0))" |> rep |> should equal "0"
    "(case 1 ((1 2) 42 99))" |> rep |> should equal "99"
    "(case 3 ((1 2) 42) (else 0))" |> rep |> should equal "0"
    "(case 1 (() 2) (else 3))" |> rep |> should equal "3"

[<Fact>]
let ``and`` () =
    "(and (= 2 2) (> 2 1))" |> rep |> should equal "#t"
    "(and (= 2 2) (< 2 1))" |> rep |> should equal "#f"
    "(and 1 2 'c '(f g))" |> rep |> should equal "(f g)"
    "(and)" |> rep |> should equal "#t"
    "(and 1)" |> rep |> should equal "1"

[<Fact>]
let ``or`` () =
    "(or (= 2 2) (> 2 1))" |> rep |> should equal "#t"
    "(or (= 2 2) (< 2 1))" |> rep |> should equal "#t"
    "(or #f #f #f)" |> rep |> should equal "#f"
    "(or)" |> rep |> should equal "#f"
    "(or 1)" |> rep |> should equal "1"

[<Fact>]
let ``when`` () =
    "(when (= 1 1) 'result)" |> rep |> should equal "result"
    "(when (= 1 1) 'first 'second)" |> rep |> should equal "second"
    "(when (= 1 2) 'result)" |> rep |> should equal "#<unspecified>"

[<Fact>]
let ``unless`` () =
    "(unless (= 1 2) 'result)" |> rep |> should equal "result"
    "(unless (= 1 2) 'first 'second)" |> rep |> should equal "second"
    "(unless (= 1 1) 'result)" |> rep |> should equal "#<unspecified>"

[<Fact>]
let ``cond-expand`` () =
    "(cond-expand (r7rs 'yes) (else 'no))" |> rep |> should equal "yes"
    "(cond-expand (exact-rational 'yes) (else 'no))" |> rep |> should equal "yes"
    "(cond-expand (ieee-float 'yes) (else 'no))" |> rep |> should equal "yes"

    "(cond-expand (unsupported-feature 'no) (else 'yes))"
    |> rep
    |> should equal "yes"

    "(cond-expand ((and r7rs exact-rational) 'yes) (else 'no))"
    |> rep
    |> should equal "yes"

    "(cond-expand ((and r7rs unsupported-feature) 'no) (else 'yes))"
    |> rep
    |> should equal "yes"

    "(cond-expand ((or unsupported-feature r7rs) 'yes) (else 'no))"
    |> rep
    |> should equal "yes"

    "(cond-expand ((not unsupported-feature) 'yes) (else 'no))"
    |> rep
    |> should equal "yes"

    "(cond-expand ((not r7rs) 'no) (else 'yes))" |> rep |> should equal "yes"

    "(cond-expand ((library (scheme base)) 'yes) (else 'no))"
    |> rep
    |> should equal "yes"

    "(cond-expand ((library (example unregistered)) 'no) (else 'yes))"
    |> rep
    |> should equal "yes"

    "(cond-expand (unsupported-feature 'no))"
    |> rep
    |> should startWith "No matching clause in cond-expand."

    "(cond-expand (else 'always))" |> rep |> should equal "always"

[<Fact>]
let ``do`` () =
    let rep = newRep ()

    "(do ((vec (make-vector 5))
          (i 0 (+ i 1)))
         ((= i 5) vec)
       (vector-set! vec i i))"
    |> rep
    |> should equal "#(0 1 2 3 4)"

    "(let ((x '(1 3 5 7 9)))
       (do ((x x (cdr x))
            (sum 0 (+ sum (car x))))
           ((null? x) sum)))"
    |> rep
    |> should equal "25"

    "(do ((i 0 (+ i 1))) ((= i 3)))" |> rep |> should equal "#<unspecified>"
    "(do ((i 0 (+ i 1)) (s 0)) ((= i 3) (list i s)))" |> rep |> should equal "(3 0)"

    "(do ((i 0 (+ i 2)) (j 10 (- j 1))) ((= i 10) (list i j)))"
    |> rep
    |> should equal "(10 5)"

    "(do ((i 0 (+ i 1))) ((= i 3) 'done) (if #f #f))" |> rep |> should equal "done"

[<Fact>]
let ``case-lambda`` () =
    let rep = newRep ()

    "(define f (case-lambda (() 0) ((x) 1) ((x y) 2) ((x y . z) 3)))"
    |> rep
    |> should equal "#<unspecified>"

    "(f)" |> rep |> should equal "0"
    "(f 1)" |> rep |> should equal "1"
    "(f 1 2)" |> rep |> should equal "2"
    "(f 1 2 3)" |> rep |> should equal "3"
    "(f 1 2 3 4)" |> rep |> should equal "3"

    "((case-lambda) 1)"
    |> rep
    |> should startWith "No matching clause in case-lambda."

    "((case-lambda ((x) x)) 1 2)"
    |> rep
    |> should startWith "No matching clause in case-lambda."
