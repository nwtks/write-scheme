module WriteScheme.Tests.RecordTest

open Xunit
open FsUnit.Xunit

let newRep () =
    WriteScheme.Repl.newContext () |> WriteScheme.Repl.rep

[<Fact>]
let ``define-record-type`` () =
    let rep = newRep ()

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
    "(list (make-p 1 2) (make-p 3 4))" |> rep |> should equal "(#<<p>> #<<p>>)"

    "(define-record-type <q> (make-q) q?)" |> rep |> ignore
    "(define q1 (make-q))" |> rep |> ignore
    "(q? q1)" |> rep |> should equal "#t"
    "(p? q1)" |> rep |> should equal "#f"
    "(q? p1)" |> rep |> should equal "#f"
