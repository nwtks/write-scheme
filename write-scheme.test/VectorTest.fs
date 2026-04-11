namespace WriteScheme.Tests

open Xunit
open FsUnit.Xunit

module VectorTest =
    let rep = WriteScheme.Repl.rep WriteScheme.Builtin.builtin

    [<Fact>]
    let ``vector?`` () =
        rep "(vector? #(1 2 3))" |> should equal "#t"
        rep "(vector? '#(1 2 3))" |> should equal "#t"
        rep "(vector? '())" |> should equal "#f"
        rep "(vector? 1)" |> should equal "#f"

    [<Fact>]
    let ``make-vector`` () =
        rep "(vector-length (make-vector 5))" |> should equal "5"
        rep "(make-vector 3 0)" |> should equal "#(0 0 0)"
        rep "(make-vector 0)" |> should equal "#()"

    [<Fact>]
    let ``vector`` () =
        rep "(vector 1 2 3)" |> should equal "#(1 2 3)"
        rep "(vector)" |> should equal "#()"
        rep "(vector 'a 'b 'c)" |> should equal "#(a b c)"

    [<Fact>]
    let ``vector-length`` () =
        rep "(vector-length #(1 2 3))" |> should equal "3"
        rep "(vector-length #())" |> should equal "0"

    [<Fact>]
    let ``vector-ref`` () =
        rep "(vector-ref #(1 2 3) 0)" |> should equal "1"
        rep "(vector-ref #(1 2 3) 2)" |> should equal "3"

    [<Fact>]
    let ``vector-set!`` () =
        rep "(let ((v (vector 1 2 3))) (vector-set! v 0 10) v)"
        |> should equal "#(10 2 3)"

        rep "(let ((v (vector 1 2 3))) (vector-set! v 2 20) v)"
        |> should equal "#(1 2 20)"

    [<Fact>]
    let ``vector->list`` () =
        rep "(vector->list #(1 2 3))" |> should equal "(1 2 3)"
        rep "(vector->list #())" |> should equal "()"

    [<Fact>]
    let ``list->vector`` () =
        rep "(list->vector '(1 2 3))" |> should equal "#(1 2 3)"
        rep "(list->vector '())" |> should equal "#()"

    let ``vector-fill!`` () =
        rep "(let ((v (vector 1 2 3))) (vector-fill! v 5) v)" |> should equal "#(5 5 5)"
