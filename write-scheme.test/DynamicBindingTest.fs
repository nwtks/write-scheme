module WriteScheme.Tests.DynamicBindingTest

open Xunit
open FsUnit.Xunit

let newRep () =
    WriteScheme.Repl.newContext () |> WriteScheme.Repl.rep

[<Fact>]
let ``parameterize`` () =
    let rep = newRep ()

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
let ``make-parameter`` () =
    let rep = newRep ()

    "(define p (make-parameter 0 (lambda (x) (* x 2))))" |> rep |> ignore
    "(p)" |> rep |> should equal "0"
    "(p 10)" |> rep |> should equal "20"
    "(p)" |> rep |> should equal "20"

    "(parameterize ((p 100)) (p))" |> rep |> should equal "200"
    "(p)" |> rep |> should equal "20"
