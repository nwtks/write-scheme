module WriteScheme.Tests.BindingTest

open Xunit
open FsUnit.Xunit

let newRep () =
    WriteScheme.Repl.newContext [] |> WriteScheme.Repl.rep

[<Fact>]
let ``let`` () =
    let rep = newRep ()

    "(let ((a 1) (A 2)) (list a A))" |> rep |> should equal "(1 2)"
    "(let ((x 2) (y 3)) (* x y))" |> rep |> should equal "6"

    "(let ((x 2) (y 3)) (let ((x 7) (z (+ x y))) (* z x)))"
    |> rep
    |> should equal "35"

    "(let ((x 2)) x)" |> rep |> should equal "2"

    "(let
      ((square (lambda (x) (* x x))))
      (square 4))"
    |> rep
    |> should equal "16"

    "(let
      ((times3
        (let
         ((n 3))
         (lambda (x) (* n x)))))
      (times3 4))"
    |> rep
    |> should equal "12"

    "(let
      ((times3
        (let
         ((makemultiplier
           (lambda (n) (lambda (x) (* n x)))))
        (makemultiplier 3))))
      (times3 5))"
    |> rep
    |> should equal "15"

    "(let loop ((i 0)) (if (< i 5) (loop (+ i 1)) i))" |> rep |> should equal "5"

    "(let factorial ((n 5)) (if (= n 0) 1 (* n (factorial (- n 1)))))"
    |> rep
    |> should equal "120"

[<Fact>]
let ``let*`` () =
    let rep = newRep ()

    "(let ((x 2) (y 3)) (let* ((x 7) (z (+ x y))) (* z x)))"
    |> rep
    |> should equal "70"

    "(let* ((a 5) (b (* a 2)) (c (- b 3))) c)" |> rep |> should equal "7"

[<Fact>]
let ``letrec`` () =
    let rep = newRep ()

    "(letrec
      ((even?
        (lambda (n)
         (if (= n 0) #t (odd? (- n 1)))))
       (odd?
        (lambda (n)
         (if (= n 0) #f (even? (- n 1))))))
      (even? 88))"
    |> rep
    |> should equal "#t"

    "(letrec
      ((factorial
        (lambda (n)
         (if (= n 0) 1 (* n (factorial (- n 1)))))))
      (factorial 4))"
    |> rep
    |> should equal "24"

[<Fact>]
let ``letrec*`` () =
    let rep = newRep ()

    "(letrec*
      ((p (lambda (x) (+ 1 (q (- x 1)))))
       (q (lambda (y) (if (= y 0) 0 (+ 1 (p (- y 1))))))
       (x (p 5))
       (y x))
      y)"
    |> rep
    |> should equal "5"

    "(let
       ((x 5))
       (letrec*
         ((foo (lambda (y) (bar x y)))
          (bar (lambda (a b) (+ (* a b) a))))
         (foo (+ x 3))))"
    |> rep
    |> should equal "45"

[<Fact>]
let ``let-values`` () =
    let rep = newRep ()

    "(let-values (((a b) (values 1 2))) (+ a b))" |> rep |> should equal "3"

    "(let-values (((a b c) (values 1 2 3)) ((d) (values 4))) (+ a b c d))"
    |> rep
    |> should equal "10"

    "(let-values (((x) 42)) x)" |> rep |> should equal "42"

    "(let-values (((a b c) (values 1 2))) a)"
    |> rep
    |> should startWith "Values count mismatch in let-values."

[<Fact>]
let ``let*-values`` () =
    let rep = newRep ()

    "(let*-values (((a b) (values 1 2))
                   ((c d) (values a b)))
       (+ a b c d))"
    |> rep
    |> should equal "6"

    "(let*-values (((x y) (values 1 2))
                   ((x y) (values y x)))
       (list x y))"
    |> rep
    |> should equal "(2 1)"

    "(let*-values (((x) 42)) x)" |> rep |> should equal "42"
