module WriteScheme.Tests.ExceptionTest

open Xunit
open FsUnit.Xunit

let newRep () =
    WriteScheme.Repl.newContext () |> WriteScheme.Repl.rep

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

[<Fact>]
let ``error and error-object?`` () =
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

    "(guard (e (else (error-object? e)))
       (raise 42))"
    |> rep
    |> should equal "#f"

[<Fact>]
let ``raise vs raise-continuable`` () =
    let rep = newRep ()

    "(with-exception-handler
       (lambda (e) (+ e 10))
       (lambda () (raise-continuable 1)))"
    |> rep
    |> should equal "11"

    "(with-exception-handler
       (lambda (e) (+ e 10))
       (lambda () (raise 1)))"
    |> rep
    |> should startWith "Exception handler returned."

[<Fact>]
let ``raise-continuable multiple values`` () =
    let rep = newRep ()

    "(call-with-values
       (lambda ()
         (with-exception-handler
           (lambda (e) (values 'a 'b))
           (lambda () (raise-continuable 'err))))
       list)"
    |> rep
    |> should equal "(a b)"

[<Fact>]
let ``guard re-raise`` () =
    let rep = newRep ()

    "(guard (e ((eq? e 'not-found) 'caught))
       (guard (e ((eq? e 'foo) 'matched))
         (raise 'not-found)))"
    |> rep
    |> should equal "caught"
