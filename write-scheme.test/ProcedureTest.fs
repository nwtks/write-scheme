module WriteScheme.Tests.ProcedureTest

open Xunit
open FsUnit.Xunit

let rep = WriteScheme.Repl.rep WriteScheme.Builtin.builtin

let repEnvs () =
    WriteScheme.Repl.newEnvs () |> WriteScheme.Repl.rep

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

[<Fact>]
let ``map`` () =
    "(map cdr '((a b) (d e) (g h)))" |> rep |> should equal "((b) (e) (h))"
    "(map + '(1 2 3) '(4 5 6 7))" |> rep |> should equal "(5 7 9)"

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

[<Fact>]
let ``call-with-current-continuation`` () =
    let rep = repEnvs ()

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
