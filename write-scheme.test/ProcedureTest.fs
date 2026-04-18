module WriteScheme.Tests.ProcedureTest

open Xunit
open FsUnit.Xunit

let rep = WriteScheme.Repl.rep WriteScheme.Builtin.builtin

[<Fact>]
let ``procedure?`` () =
    "(procedure? car)" |> rep |> should equal "#t"
    "(procedure? 'car)" |> rep |> should equal "#f"
    "(procedure? (lambda (x) (* x x)))" |> rep |> should equal "#t"
    "(procedure? '(lambda (x) (* x x)))" |> rep |> should equal "#f"
    "(call-with-current-continuation procedure?)" |> rep |> should equal "#t"

[<Fact>]
let ``apply`` () =
    "(apply + (list 3 4))" |> rep |> should equal "7"
    "(apply + 1 2 '(3))" |> rep |> should equal "6"
    "(apply + '())" |> rep |> should equal "0"
    "(apply list 1 '() '(2))" |> rep |> should equal "(1 () 2)"

[<Fact>]
let ``map`` () =
    "(map cdr '((a b) (d e) (g h)))" |> rep |> should equal "((b) (e) (h))"
    "(map + '(1 2 3) '(4 5 6 7))" |> rep |> should equal "(5 7 9)"
    "(map car '())" |> rep |> should equal "()"
    "(map (lambda (x) (* x x)) '(1 2 3))" |> rep |> should equal "(1 4 9)"

[<Fact>]
let ``vector-map`` () =
    "(vector-map + '#(1 2 3) '#(4 5 6))" |> rep |> should equal "#(5 7 9)"
    "(vector-map (lambda (x) (* x x)) '#(1 2 3))" |> rep |> should equal "#(1 4 9)"

[<Fact>]
let ``for-each`` () =
    "(let
       ((v '()))
       (for-each
         (lambda (i) (set! v (cons (* i i) v)))
         '(0 1 2 3 4))
       v)"
    |> rep
    |> should equal "(16 9 4 1 0)"

    "(let ((v '())) (for-each (lambda (x) (set! v (cons x v))) '()) v)"
    |> rep
    |> should equal "()"

[<Fact>]
let ``vector-for-each`` () =
    "(let ((v (make-vector 3))) (vector-for-each (lambda (i x) (vector-set! v i (* x x))) '#(0 1 2) '#(1 2 3)) v)"
    |> rep
    |> should equal "#(1 4 9)"

[<Fact>]
let ``call-with-current-continuation`` () =
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
    |> rep
    |> ignore

    "(list-length '(a b c d))" |> rep |> should equal "4"
    "(list-length '(a b . c))" |> rep |> should equal "#f"

[<Fact>]
let ``values`` () =
    "(call-with-values (lambda () (values 1 2 3)) list)"
    |> rep
    |> should equal "(1 2 3)"

    "(values 42)" |> rep |> should equal "42"
