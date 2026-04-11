module WriteScheme.Tests.CoreTest

open Xunit
open FsUnit.Xunit

let rep = WriteScheme.Repl.rep WriteScheme.Builtin.builtin

let repEnvs () =
    WriteScheme.Repl.newEnvs () |> WriteScheme.Repl.rep

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

[<Fact>]
let ``with-exception-handler basic`` () =
    "(with-exception-handler
       (lambda (e) (+ e 100))
       (lambda () (raise 1)))"
    |> rep
    |> should equal "101"

[<Fact>]
let ``with-exception-handler no exception`` () =
    "(with-exception-handler
       (lambda (e) 'error)
       (lambda () 42))"
    |> rep
    |> should equal "42"

[<Fact>]
let ``with-exception-handler nested`` () =
    "(with-exception-handler
       (lambda (e) (+ e 100))
       (lambda ()
         (with-exception-handler
           (lambda (e) (raise (+ e 10)))
           (lambda () (raise 1)))))"
    |> rep
    |> should equal "111"

[<Fact>]
let ``error and error-object`` () =
    "(with-exception-handler
       (lambda (e)
         (list (error-object? e)
               (error-object-message e)
               (error-object-irritants e)))
       (lambda () (error \"bad value\" 1 2)))"
    |> rep
    |> should equal "(#t \"bad value\" (1 2))"

[<Fact>]
let ``error-object? false for non-error`` () =
    "(with-exception-handler
       (lambda (e) (error-object? e))
       (lambda () (raise 42)))"
    |> rep
    |> should equal "#f"
