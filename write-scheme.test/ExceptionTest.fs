module WriteScheme.Tests.ExceptionTest

open Xunit
open FsUnit.Xunit

let rep = WriteScheme.Repl.rep WriteScheme.Builtin.builtin

[<Fact>]
let ``with-exception-handler`` () =
    "(with-exception-handler
       (lambda (e) (+ e 100))
       (lambda () (raise 1)))"
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
           (lambda (e) (raise (+ e 10)))
           (lambda () (raise 1)))))"
    |> rep
    |> should equal "111"

[<Fact>]
let ``error and error-object?`` () =
    "(with-exception-handler
       (lambda (e)
         (list (error-object? e)
               (error-object-message e)
               (error-object-irritants e)))
       (lambda () (error \"bad value\" 1 2)))"
    |> rep
    |> should equal "(#t \"bad value\" (1 2))"

    "(with-exception-handler
        (lambda (e)
          (list (error-object-message e)
                (error-object-irritants e)))
        (lambda () (error \"simple error\")))"
    |> rep
    |> should equal "(\"simple error\" ())"

    "(with-exception-handler
       (lambda (e) (error-object? e))
       (lambda () (raise 42)))"
    |> rep
    |> should equal "#f"
