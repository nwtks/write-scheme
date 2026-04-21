module WriteScheme.Tests.SpecialFormTest

open Xunit
open FsUnit.Xunit

let rep = WriteScheme.Repl.rep WriteScheme.Builtin.builtin

let repEnvs () =
    WriteScheme.Repl.newEnvs () |> WriteScheme.Repl.rep

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

[<Fact>]
let ``lambda`` () =
    "((lambda (x) (+ x x)) 4)" |> rep |> should equal "8"
    "((lambda x x))" |> rep |> should equal "()"
    "((lambda x x) 1)" |> rep |> should equal "(1)"
    "((lambda x x) 3 4 5 6)" |> rep |> should equal "(3 4 5 6)"
    "((lambda (x y . z) z) 3 4 5 6)" |> rep |> should equal "(5 6)"

    "(define reverse-subtract (lambda (x y) (- y x)))" |> rep |> ignore
    "(reverse-subtract 7 10)" |> rep |> should equal "3"

    "(define add4 (let ((x 4)) (lambda (y) (+ x y))))" |> rep |> ignore
    "(add4 6)" |> rep |> should equal "10"

[<Fact>]
let ``if`` () =
    "(if (> 3 2) 'yes)" |> rep |> should equal "yes"
    "(if (> 2 3) 'yes 'no)" |> rep |> should equal "no"
    "(if (> 3 2) (- 3 2) (+ 3 2))" |> rep |> should equal "1"
    "((if #t + *) 3 4)" |> rep |> should equal "7"
    "((if #f + *) 3 4)" |> rep |> should equal "12"
    "(if #t 42)" |> rep |> should equal "42"
    "(if #f 42 99)" |> rep |> should equal "99"

[<Fact>]
let ``set!`` () =
    "(define x 2)" |> rep |> ignore
    "(+ x 1)" |> rep |> should equal "3"
    "(set! x 4)" |> rep |> ignore
    "(+ x 1)" |> rep |> should equal "5"

[<Fact>]
let cond () =
    "(cond ((> 3 2) 'greater) ((< 3 2) 'less))" |> rep |> should equal "greater"

    "(cond ((> 3 3) 'greater) ((< 3 3) 'less) (else 'equal))"
    |> rep
    |> should equal "equal"

    "(cond ('((a 1) (b 2) (c 3)) => cdr) (else #f))"
    |> rep
    |> should equal "((b 2) (c 3))"

    "(cond ((> 1 2) 'a) ((> 1 3) 'b))" |> rep |> should equal "()"

[<Fact>]
let ``case`` () =
    "(case (* 2 3) ((2 3 5 7) 'prime) ((1 4 6 8 9) 'composite))"
    |> rep
    |> should equal "composite"

    "(case (car '(c d)) ((a e i o u) 'vowel) ((w y) 'semivowel) (else => (lambda (x) x)))"
    |> rep
    |> should equal "c"

    "(case (car '(c d)) ((a) 'a) ((b) 'b))" |> rep |> should equal "()"
    "(case (* 2 3) ((1 4 6 8 9) => (lambda (x) x)))" |> rep |> should equal "6"
    "(case 1)" |> rep |> should equal "()"
    "(case 1 (else => (lambda (x) x)))" |> rep |> should equal "1"
    "(case 1 (else 1 2 3))" |> rep |> should equal "3"
    "(case 1 ((1 2) => (lambda (x) x)))" |> rep |> should equal "1"
    "(case 3 ((1 2) => (lambda (x) x)) (else 0))" |> rep |> should equal "0"
    "(case 1 ((1 2) 42 99))" |> rep |> should equal "99"
    "(case 3 ((1 2) 42) (else 0))" |> rep |> should equal "0"

[<Fact>]
let ``and`` () =
    "(and (= 2 2) (> 2 1))" |> rep |> should equal "#t"
    "(and (= 2 2) (< 2 1))" |> rep |> should equal "#f"
    "(and 1 2 'c '(f g))" |> rep |> should equal "(f g)"
    "(and)" |> rep |> should equal "#t"

[<Fact>]
let ``or`` () =
    "(or (= 2 2) (> 2 1))" |> rep |> should equal "#t"
    "(or (= 2 2) (< 2 1))" |> rep |> should equal "#t"
    "(or #f #f #f)" |> rep |> should equal "#f"
    "(or)" |> rep |> should equal "#f"

[<Fact>]
let ``when`` () =
    "(when (= 1 1) 'result)" |> rep |> should equal "result"
    "(when (= 1 1) 'first 'second)" |> rep |> should equal "second"
    "(when (= 1 2) 'result)" |> rep |> should equal "()"

[<Fact>]
let ``unless`` () =
    "(unless (= 1 2) 'result)" |> rep |> should equal "result"
    "(unless (= 1 2) 'first 'second)" |> rep |> should equal "second"
    "(unless (= 1 1) 'result)" |> rep |> should equal "()"

[<Fact>]
let ``let`` () =
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
    "(let ((x 2) (y 3)) (let* ((x 7) (z (+ x y))) (* z x)))"
    |> rep
    |> should equal "70"

    "(let* ((a 5) (b (* a 2)) (c (- b 3))) c)" |> rep |> should equal "7"

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
    "(let-values (((a b) (values 1 2))) (+ a b))" |> rep |> should equal "3"

    "(let-values (((a b c) (values 1 2 3)) ((d) (values 4))) (+ a b c d))"
    |> rep
    |> should equal "10"

    "(let-values (((x) 42)) x)" |> rep |> should equal "42"

[<Fact>]
let ``let*-values`` () =
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

[<Fact>]
let ``values and call-with-values`` () =
    "(call-with-values (lambda () (values 1 2)) +)" |> rep |> should equal "3"

    "(call-with-values (lambda () (values 4 5)) (lambda (a b) b))"
    |> rep
    |> should equal "5"

    "(call-with-values (lambda () 42) (lambda (x) x))" |> rep |> should equal "42"

[<Fact>]
let ``begin`` () =
    let rep = repEnvs ()
    "(define x 0)" |> rep |> ignore
    "(and (= x 0) (begin (set! x 5) (+ x 1)))" |> rep |> should equal "6"

[<Fact>]
let ``do`` () =
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

    "(do ((i 0 (+ i 1))) ((= i 3)))" |> rep |> should equal "()"
    "(do ((i 0 (+ i 1)) (s 0)) ((= i 3) (list i s)))" |> rep |> should equal "(3 0)"

[<Fact>]
let ``delay`` () =
    "(force (delay (+ 1 2)))" |> rep |> should equal "3"
    "(force (make-promise 42))" |> rep |> should equal "42"

    "(define count 0)" |> rep |> ignore
    "(define p (delay (begin (set! count (+ count 1)) count)))" |> rep |> ignore
    "(force p)" |> rep |> should equal "1"
    "(force p)" |> rep |> should equal "1"
    "(force 1)" |> rep |> should equal "1"
    "count" |> rep |> should equal "1"

[<Fact>]
let ``delay-force`` () =
    "(force (delay-force (delay (delay-force (delay 10)))))"
    |> rep
    |> should equal "10"

[<Fact>]
let ``parameterize`` () =
    "(define radix (make-parameter 10))" |> rep |> ignore
    "(radix)" |> rep |> should equal "10"
    "(parameterize ((radix 16)) (radix))" |> rep |> should equal "16"
    "(radix)" |> rep |> should equal "10"

    "(define greet (make-parameter \"hello\"
                     (lambda (x) (if (string? x) x \"default\"))))"
    |> rep
    |> ignore

    "(parameterize ((greet 42)) (greet))" |> rep |> should equal "\"default\""
    "(greet)" |> rep |> should equal "\"hello\""

    "(parameterize ((radix 2)) (parameterize ((radix 8)) (radix)))"
    |> rep
    |> should equal "8"

    "(radix)" |> rep |> should equal "10"

    "(define p (make-parameter 0))" |> rep |> ignore
    "(define c #f)" |> rep |> ignore

    "(define (test)
        (parameterize ((p 1))
            (call/cc (lambda (k) (set! c k)))
            (p)))"
    |> rep
    |> ignore

    "(test)" |> rep |> should equal "1"
    "(p)" |> rep |> should equal "0"
    "(c #t)" |> rep |> should equal "1"
    "(p)" |> rep |> should equal "0"
    "(p 2)" |> rep |> ignore
    "(c #t)" |> rep |> should equal "1"
    "(p)" |> rep |> should equal "2"

[<Fact>]
let ``guard`` () =
    "(guard (condition
             (else 'caught))
       (+ 1 2))"
    |> rep
    |> should equal "3"

    "(guard (condition
             (else condition))
       (raise 'error-happened))"
    |> rep
    |> should equal "error-happened"

    "(guard (condition
             ((eq? condition 'foo) 'matched-foo)
             ((eq? condition 'bar) 'matched-bar)
             (else 'fallback))
       (raise 'bar))"
    |> rep
    |> should equal "matched-bar"

    (fun () ->
        "(guard (condition
                 ((eq? condition 'foo) 'matched))
           (raise 'bar))"
        |> rep
        |> ignore)
    |> should throw typeof<System.Exception>

[<Fact>]
let ``let-syntax`` () =
    "(let-syntax ((when (syntax-rules ()
                          ((when test stmt1 stmt2 ...)
                           (if test
                               (begin stmt1 stmt2 ...))))))
       (let ((x #t))
         (when x (set! x 'now))
         x))"
    |> rep
    |> should equal "now"

[<Fact>]
let ``letrec-syntax`` () =
    "(letrec-syntax ((my-or (syntax-rules ()
                              ((my-or) #f)
                              ((my-or e) e)
                              ((my-or e1 e2 ...)
                               (let ((temp e1))
                                 (if temp temp (my-or e2 ...)))))))
       (my-or #f #f 1 2))"
    |> rep
    |> should equal "1"

[<Fact>]
let ``quasiquote`` () =
    "`(list ,(+ 1 2) 4)" |> rep |> should equal "(list 3 4)"
    "`(a . ,(+ 1 2))" |> rep |> should equal "(a . 3)"
    "(let ((name 'a)) `(list ,name ',name))" |> rep |> should equal "(list a 'a)"

    "`((foo ,(- 10 3)) ,@(cdr '(c d)) . ,(car '(cons)))"
    |> rep
    |> should equal "((foo 7) d . cons)"

    "(let ((foo '(foo bar)) (baz 'baz)) `(list ,@foo ,baz))"
    |> rep
    |> should equal "(list foo bar baz)"

    "`(a `(b ,(+ 1 2) ,(foo ,(+ 1 3) d) e) f)"
    |> rep
    |> should equal "(a `(b ,(+ 1 2) ,(foo 4 d) e) f)"

    "(let ((name1 'x) (name2 'y)) `(a `(b ,,name1 ,',name2 d) e))"
    |> rep
    |> should equal "(a `(b ,x ,'y d) e)"

    "`1" |> rep |> should equal "1"
    "`\"abc\"" |> rep |> should equal "\"abc\""
    "`#t" |> rep |> should equal "#t"
    "`a" |> rep |> should equal "a"
    "`()" |> rep |> should equal "()"

    "`(,@'(1 2) ,@'(3 4))" |> rep |> should equal "(1 2 3 4)"
    "`(,@'())" |> rep |> should equal "()"
    "`(,@'() . ,@'())" |> rep |> should equal "()"
    "`(,@'() 1 2)" |> rep |> should equal "(1 2)"
    "`,@'(1 2)" |> rep |> should equal "(1 2)"
    "``(a . ,,@'(1 2))" |> rep |> should equal "`(a . ,(1 2))"
    "`(,@'(1 2) . 3)" |> rep |> should equal "(1 2 . 3)"
    "`(1 2 . ,(append '(3 4) 5))" |> rep |> should equal "(1 2 3 4 . 5)"
    "`(,@'(1 2) . ,(append '(3 4) 5))" |> rep |> should equal "(1 2 3 4 . 5)"
    "`(a . ,(values 1 2))" |> rep |> should equal "(a . (values 1 2))"
    "`(,@(values '(1 2)))" |> rep |> should equal "(1 2)"
    "`#()" |> rep |> should equal "#()"
    "`#(1 ,@(values '(2 3)) 4)" |> rep |> should equal "#(1 2 3 4)"

    "(let ((x 1)) `#(a ,x c))" |> rep |> should equal "#(a 1 c)"
    "(let ((x '(1 2))) `#(a ,@x c))" |> rep |> should equal "#(a 1 2 c)"
    "(let ((x '(1 2))) `#(a ,@x ,x))" |> rep |> should equal "#(a 1 2 (1 2))"
    "(let ((x '(1 2))) `(a . ,x))" |> rep |> should equal "(a 1 2)"
    "(let ((x '(1 2))) `(a ,@x . 3))" |> rep |> should equal "(a 1 2 . 3)"
    "(let ((x '(1 2)) (y '(3 4))) `(,@x . ,@y))" |> rep |> should equal "(1 2 3 4)"
    "(let ((x '(1 2))) `(a `(b . ,@x)))" |> rep |> should equal "(a `(b . ,@x))"
    "(let ((x '(1 2))) `(a `(b ,@,x)))" |> rep |> should equal "(a `(b ,@(1 2)))"
    "(let ((x '((1 2)))) `(a `(b ,,@x)))" |> rep |> should equal "(a `(b ,((1 2))))"

[<Fact>]
let ``define`` () =
    "(define add3 (lambda (x) (+ x 3)))" |> rep |> ignore
    "(add3 3)" |> rep |> should equal "6"

    "(define first car)" |> rep |> ignore
    "(first '(1 2))" |> rep |> should equal "1"

    "(define (square x) (* x x))" |> rep |> ignore
    "(square 5)" |> rep |> should equal "25"

    "(define (add . xs) (apply + xs))" |> rep |> ignore
    "(add 1 2 3)" |> rep |> should equal "6"

[<Fact>]
let ``define-values`` () =
    let rep = repEnvs ()

    "(define-values (x y) (values 1 2))" |> rep |> ignore
    "(+ x y)" |> rep |> should equal "3"

    "(define-values (a . b) (values 10 20 30))" |> rep |> ignore
    "a" |> rep |> should equal "10"
    "b" |> rep |> should equal "(20 30)"

    "(define-values (z) 42)" |> rep |> ignore
    "z" |> rep |> should equal "42"

[<Fact>]
let ``define-record-type`` () =
    "(define-record-type <p> (make-p x y) p? (x get-x) (y get-y set-y!))"
    |> rep
    |> ignore

    "(define p1 (make-p 1 2))" |> rep |> ignore
    "(p? p1)" |> rep |> should equal "#t"
    "(p? 1)" |> rep |> should equal "#f"
    "(get-x p1)" |> rep |> should equal "1"
    "(get-y p1)" |> rep |> should equal "2"
    "(set-y! p1 10)" |> rep |> ignore
    "(get-y p1)" |> rep |> should equal "10"

    "(define-record-type <q> (make-q) q?)" |> rep |> ignore
    "(define q1 (make-q))" |> rep |> ignore
    "(q? q1)" |> rep |> should equal "#t"
    "(p? q1)" |> rep |> should equal "#f"
    "(q? p1)" |> rep |> should equal "#f"

[<Fact>]
let ``promise?`` () =
    "(promise? 1)" |> rep |> should equal "#f"
    "(promise? (make-promise 1))" |> rep |> should equal "#t"
    "(make-promise (delay 1))" |> rep |> should equal "#<promise>"

    "(define p (delay 42))" |> rep |> ignore
    "(promise? p)" |> rep |> should equal "#t"

[<Fact>]
let ``make-parameter`` () =
    "(define p (make-parameter 0 (lambda (x) (* x 2))))" |> rep |> ignore
    "(p)" |> rep |> should equal "0"
    "(p 10)" |> rep |> should equal "20"
    "(p)" |> rep |> should equal "20"

    "(parameterize ((p 100)) (p))" |> rep |> should equal "200"
    "(p)" |> rep |> should equal "20"
