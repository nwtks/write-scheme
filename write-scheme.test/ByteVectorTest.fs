namespace WriteScheme.Tests

open Xunit
open FsUnit.Xunit

module ByteVectorTest =
    let rep = WriteScheme.Repl.rep WriteScheme.Builtin.builtin

    [<Fact>]
    let ``bytevector?`` () =
        rep "(bytevector? #u8(1 2 3))" |> should equal "#t"
        rep "(bytevector? '#u8(1 2 3))" |> should equal "#t"
        rep "(bytevector? #u8())" |> should equal "#t"
        rep "(bytevector? '#u8())" |> should equal "#t"
        rep "(bytevector? '())" |> should equal "#f"
        rep "(bytevector? 1)" |> should equal "#f"

    [<Fact>]
    let ``make-bytevector`` () =
        rep "(bytevector-length (make-bytevector 5))" |> should equal "5"
        rep "(make-bytevector 3 0)" |> should equal "#u8(0 0 0)"
        rep "(make-bytevector 0)" |> should equal "#u8()"
        rep "(make-bytevector 3 255)" |> should equal "#u8(255 255 255)"

    [<Fact>]
    let ``bytevector`` () =
        rep "(bytevector 1 2 3)" |> should equal "#u8(1 2 3)"
        rep "(bytevector)" |> should equal "#u8()"

    [<Fact>]
    let ``bytevector-length`` () =
        rep "(bytevector-length #u8(1 2 3))" |> should equal "3"
        rep "(bytevector-length #u8())" |> should equal "0"

    [<Fact>]
    let ``bytevector-u8-ref`` () =
        rep "(bytevector-u8-ref #u8(10 20 30) 0)" |> should equal "10"
        rep "(bytevector-u8-ref #u8(10 20 30) 2)" |> should equal "30"

    [<Fact>]
    let ``bytevector-u8-set!`` () =
        rep "(let ((v (bytevector 1 2 3))) (bytevector-u8-set! v 0 10) v)"
        |> should equal "#u8(10 2 3)"

        rep "(let ((v (bytevector 1 2 3))) (bytevector-u8-set! v 2 255) v)"
        |> should equal "#u8(1 2 255)"

    [<Fact>]
    let ``bytevector-copy`` () =
        rep "(let* ((a (bytevector 1 2 3)) (b (bytevector-copy a))) (bytevector-u8-set! a 0 10) b)"
        |> should equal "#u8(1 2 3)"
