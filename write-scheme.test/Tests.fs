module Tests

open Xunit
open FsUnit.Xunit
open Builtin
open Repl

[<Fact>]
let number () =
    "0" |> rep builtin |> should equal "0"
    "1" |> rep builtin |> should equal "1"
    "+1" |> rep builtin |> should equal "1"
    "-1" |> rep builtin |> should equal "-1"
    "1.0" |> rep builtin |> should equal "1"
    "1." |> rep builtin |> should equal "1"
    "0.1" |> rep builtin |> should equal "0.1"
    ".1" |> rep builtin |> should equal "0.1"
    "-100.0e-3" |> rep builtin |> should equal "-0.1"
    "-0.1" |> rep builtin |> should equal "-0.1"
    "-.1" |> rep builtin |> should equal "-0.1"
    "100.0e-3" |> rep builtin |> should equal "0.1"
    "1e2" |> rep builtin |> should equal "100"
    "1.0E2" |> rep builtin |> should equal "100"
    "1.0e+2" |> rep builtin |> should equal "100"
    "+nan.0" |> rep builtin |> should equal "+nan.0"
    "+NaN.0" |> rep builtin |> should equal "+nan.0"
    "+inf.0" |> rep builtin |> should equal "+inf.0"
    "+Inf.0" |> rep builtin |> should equal "+inf.0"
    "-inf.0" |> rep builtin |> should equal "-inf.0"
    "-INF.0" |> rep builtin |> should equal "-inf.0"
    "1/2" |> rep builtin |> should equal "1/2"
    "10/2" |> rep builtin |> should equal "5"
    "-1/2" |> rep builtin |> should equal "-1/2"
    "0/2" |> rep builtin |> should equal "0"
    "#x11" |> rep builtin |> should equal "17"
    "#X11" |> rep builtin |> should equal "17"
    "#d11" |> rep builtin |> should equal "11"
    "#D11" |> rep builtin |> should equal "11"
    "#o11" |> rep builtin |> should equal "9"
    "#O11" |> rep builtin |> should equal "9"
    "#b11" |> rep builtin |> should equal "3"
    "#B11" |> rep builtin |> should equal "3"
    "#o7" |> rep builtin |> should equal "7"
    "#xA" |> rep builtin |> should equal "10"
    "#Xf" |> rep builtin |> should equal "15"
    "#x-10" |> rep builtin |> should equal "-16"
    "#d-10" |> rep builtin |> should equal "-10"
    "#o-10" |> rep builtin |> should equal "-8"
    "#b-10" |> rep builtin |> should equal "-2"
    "#d1." |> rep builtin |> should equal "1"
    "#d.1" |> rep builtin |> should equal "0.1"
    "#x10/2" |> rep builtin |> should equal "8"
    "#x11/2" |> rep builtin |> should equal "17/2"
    "#d11/2" |> rep builtin |> should equal "11/2"
    "#o11/2" |> rep builtin |> should equal "9/2"
    "#b11/10" |> rep builtin |> should equal "3/2"

[<Fact>]
let bool () =
    "#t" |> rep builtin |> should equal "#t"
    "#true" |> rep builtin |> should equal "#t"
    "#f" |> rep builtin |> should equal "#f"
    "#false" |> rep builtin |> should equal "#f"

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
let string () =
    "\"\"" |> rep builtin |> should equal "\"\""

    "\"12345\""
    |> rep builtin
    |> should equal "\"12345\""

    "\"1\\a2\\b3\\t4\\n5\\r6\\\"7\\\\8|90\""
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

    "''a" |> rep builtin |> should equal "'a"

    "'((a 1) (b 2) (c 3))"
    |> rep builtin
    |> should equal "((a 1) (b 2) (c 3))"

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

    let envs = newEnvs ()

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
    "`(list ,(+ 1 2) 4)"
    |> rep builtin
    |> should equal "(list 3 4)"

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

[<Fact>]
let ``not`` () =
    "(not #t)" |> rep builtin |> should equal "#f"
    "(not 3)" |> rep builtin |> should equal "#f"

    "(not (list 3))"
    |> rep builtin
    |> should equal "#f"

    "(not #f)" |> rep builtin |> should equal "#t"
    "(not '())" |> rep builtin |> should equal "#f"
    "(not (list))" |> rep builtin |> should equal "#f"
    "(not 'nil)" |> rep builtin |> should equal "#f"

[<Fact>]
let ``boolean?`` () =
    "(boolean? #f)"
    |> rep builtin
    |> should equal "#t"

    "(boolean? #t)"
    |> rep builtin
    |> should equal "#t"

    "(boolean? 0)" |> rep builtin |> should equal "#f"

    "(boolean? '())"
    |> rep builtin
    |> should equal "#f"

[<Fact>]
let ``pair?`` () =
    "(pair? '(a . b))"
    |> rep builtin
    |> should equal "#t"

    "(pair? '(a b c))"
    |> rep builtin
    |> should equal "#t"

    "(pair? '())" |> rep builtin |> should equal "#f"

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
let ``null?`` () =
    "(null? '(a . b))"
    |> rep builtin
    |> should equal "#f"

    "(null? '(a b c))"
    |> rep builtin
    |> should equal "#f"

    "(null? '())" |> rep builtin |> should equal "#t"

[<Fact>]
let ``list?`` () =
    "(list? '(a . b))"
    |> rep builtin
    |> should equal "#f"

    "(list? '(a b c))"
    |> rep builtin
    |> should equal "#t"

    "(list? '())" |> rep builtin |> should equal "#t"

[<Fact>]
let list () =
    "(list 'a (+ 3 4) 'c)"
    |> rep builtin
    |> should equal "(a 7 c)"

    "(list)" |> rep builtin |> should equal "()"

[<Fact>]
let append () =
    "(append '(x) '(y))"
    |> rep builtin
    |> should equal "(x y)"

    "(append '(a) '(b c d))"
    |> rep builtin
    |> should equal "(a b c d)"

    "(append '(a (b)) '((c)))"
    |> rep builtin
    |> should equal "(a (b) (c))"

    "(append '(a b) '(c . d))"
    |> rep builtin
    |> should equal "(a b c . d)"

    "(append '() 'a)"
    |> rep builtin
    |> should equal "a"

[<Fact>]
let ``symbol?`` () =
    "(symbol? 'foo)"
    |> rep builtin
    |> should equal "#t"

    "(symbol? (car '(a b)))"
    |> rep builtin
    |> should equal "#t"

    "(symbol? \"bar\")"
    |> rep builtin
    |> should equal "#f"

    "(symbol? 'nil)"
    |> rep builtin
    |> should equal "#t"

    "(symbol? '())"
    |> rep builtin
    |> should equal "#f"

    "(symbol? #f)" |> rep builtin |> should equal "#f"

[<Fact>]
let ``procedure?`` () =
    "(procedure? car)"
    |> rep builtin
    |> should equal "#t"

    "(procedure? 'car)"
    |> rep builtin
    |> should equal "#f"

    "(procedure? (lambda (x) (* x x)))"
    |> rep builtin
    |> should equal "#t"

    "(procedure? '(lambda (x) (* x x)))"
    |> rep builtin
    |> should equal "#f"

    "(call-with-current-continuation procedure?)"
    |> rep builtin
    |> should equal "#t"

[<Fact>]
let ``apply`` () =
    "(apply + (list 3 4))"
    |> rep builtin
    |> should equal "7"

[<Fact>]
let ``map`` () =
    "(map cdr '((a b) (d e) (g h)))"
    |> rep builtin
    |> should equal "((b) (e) (h))"

    "(map + '(1 2 3) '(4 5 6 7))"
    |> rep builtin
    |> should equal "(5 7 9)"

[<Fact>]
let ``for-each`` () =
    "(let
       ((v '()))
       (for-each
         (lambda (i) (set! v (cons (* i i) v)))
         '(0 1 2 3 4))
       v)"
    |> rep builtin
    |> should equal "(16 9 4 1 0)"

[<Fact>]
let ``call-with-current-continuation`` () =
    let envs = newEnvs ()

    "(define list-length
       (lambda (obj)
         (call-with-current-continuation
           (lambda (return)
             (letrec
               ((r
                 (lambda (o)
                   (cond
                     ((null? o) 0)
                     ((pair? o) (+ (r (cdr o)) 1))
                     (else (return #f))))))
               (r obj))))))"
    |> rep envs
    |> ignore

    "(list-length '(a b c d))"
    |> rep envs
    |> should equal "4"

    "(list-length '(a b . c))"
    |> rep envs
    |> should equal "#f"
