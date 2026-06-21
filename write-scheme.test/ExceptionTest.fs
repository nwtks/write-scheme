module WriteScheme.Tests.ExceptionTest

open Xunit
open FsUnit.Xunit

let newRep () =
    WriteScheme.Repl.newContext () |> WriteScheme.Repl.rep

[<Fact>]
let ``guard`` () =
    let rep = newRep ()

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

    "(guard (condition
             ((eq? condition 'foo) 'matched))
       (raise 'bar))"
    |> rep
    |> should equal "bar"

    "(guard (e ((eq? e 'not-found) 'caught))
       (guard (e ((eq? e 'foo) 'matched))
         (raise 'not-found)))"
    |> rep
    |> should equal "caught"

[<Fact>]
let ``with-exception-handler`` () =
    let rep = newRep ()

    "(with-exception-handler
       (lambda (e) (+ e 100))
       (lambda () (raise-continuable 1)))"
    |> rep
    |> should equal "101"

    "(with-exception-handler
       (lambda (e) (+ e 100))
       (lambda () 42))"
    |> rep
    |> should equal "42"

    "(with-exception-handler
        (lambda (e) (+ e 200))
        (lambda ()
          (+ 100
             (with-exception-handler
                (lambda (e) e)
                (lambda () 42)))))"
    |> rep
    |> should equal "142"

    "(with-exception-handler
       (lambda (e) (+ e 100))
       (lambda ()
         (with-exception-handler
           (lambda (e) (raise-continuable (+ e 10)))
           (lambda () (raise-continuable 1)))))"
    |> rep
    |> should equal "111"

    "(with-exception-handler 1)"
    |> rep
    |> should startWith "'(1)' invalid with-exception-handler parameter"

[<Fact>]
let ``error`` () =
    let rep = newRep ()

    "(guard (e (else (list (error-object? e)
                          (error-object-message e)
                          (error-object-irritants e))))
       (error \"bad value\" 1 2))"
    |> rep
    |> should equal "(#t \"bad value\" (1 2))"

    "(guard (e (else (list (error-object-message e)
                          (error-object-irritants e))))
       (error \"simple error\"))"
    |> rep
    |> should equal "(\"simple error\" ())"

    "(error)" |> rep |> should startWith "'()' invalid error parameter"
    "(error 1)" |> rep |> should startWith "'(1)' invalid error parameter"

[<Fact>]
let ``error-object?`` () =
    let rep = newRep ()

    "(guard (e (else (error-object? e)))
       (raise 42))"
    |> rep
    |> should equal "#f"

    "(error-object? 1 2)"
    |> rep
    |> should startWith "'(1 2)' invalid error-object? parameter"

    "(error-object-message 1)"
    |> rep
    |> should startWith "'(1)' invalid error-object-message parameter"

    "(error-object-irritants 1)"
    |> rep
    |> should startWith "'(1)' invalid error-object-irritants parameter"

[<Fact>]
let ``raise`` () =
    let rep = newRep ()

    "(with-exception-handler
       (lambda (e) (+ e 10))
       (lambda () (raise 1)))"
    |> rep
    |> should startWith "Exception handler returned."

    "(raise)" |> rep |> should startWith "'()' invalid raise parameter"
    "(raise 1 2)" |> rep |> should startWith "'(1 2)' invalid raise parameter"

[<Fact>]
let ``raise-continuable`` () =
    let rep = newRep ()

    "(with-exception-handler
       (lambda (e) (+ e 10))
       (lambda () (raise-continuable 1)))"
    |> rep
    |> should equal "11"

    "(call-with-values
       (lambda ()
         (with-exception-handler
           (lambda (e) (values 'a 'b))
           (lambda () (raise-continuable 'err))))
       list)"
    |> rep
    |> should equal "(a b)"

    "(raise-continuable)"
    |> rep
    |> should startWith "'()' invalid raise-continuable parameter"

    "(raise-continuable 1 2)"
    |> rep
    |> should startWith "'(1 2)' invalid raise-continuable parameter"
