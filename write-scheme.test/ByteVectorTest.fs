namespace WriteScheme.Tests

open Xunit
open FsUnit.Xunit

module ByteVectorTest =
    let rep = WriteScheme.Repl.rep WriteScheme.Builtin.builtin

    [<Fact>]
    let ``bytevector?`` () =
        "(bytevector? #u8(1 2 3))" |> rep |> should equal "#t"
        "(bytevector? '#u8(1 2 3))" |> rep |> should equal "#t"
        "(bytevector? #u8())" |> rep |> should equal "#t"
        "(bytevector? '#u8())" |> rep |> should equal "#t"
        "(bytevector? '())" |> rep |> should equal "#f"
        "(bytevector? 1)" |> rep |> should equal "#f"

    [<Fact>]
    let ``make-bytevector`` () =
        "(bytevector-length (make-bytevector 5))" |> rep |> should equal "5"
        "(make-bytevector 3 0)" |> rep |> should equal "#u8(0 0 0)"
        "(make-bytevector 0)" |> rep |> should equal "#u8()"
        "(make-bytevector 3 255)" |> rep |> should equal "#u8(255 255 255)"

    [<Fact>]
    let ``bytevector`` () =
        "(bytevector 1 2 3)" |> rep |> should equal "#u8(1 2 3)"
        "(bytevector)" |> rep |> should equal "#u8()"

    [<Fact>]
    let ``bytevector-length`` () =
        "(bytevector-length #u8(1 2 3))" |> rep |> should equal "3"
        "(bytevector-length #u8())" |> rep |> should equal "0"

    [<Fact>]
    let ``bytevector-u8-ref`` () =
        "(bytevector-u8-ref #u8(10 20 30) 0)" |> rep |> should equal "10"
        "(bytevector-u8-ref #u8(10 20 30) 2)" |> rep |> should equal "30"

    [<Fact>]
    let ``bytevector-u8-set!`` () =
        "(let ((v (bytevector 1 2 3))) (bytevector-u8-set! v 0 10) v)"
        |> rep
        |> should equal "#u8(10 2 3)"

        "(let ((v (bytevector 1 2 3))) (bytevector-u8-set! v 2 255) v)"
        |> rep
        |> should equal "#u8(1 2 255)"

    [<Fact>]
    let ``bytevector-copy`` () =
        "(let* ((a (bytevector 1 2 3)) (b (bytevector-copy a))) (bytevector-u8-set! a 0 10) b)"
        |> rep
        |> should equal "#u8(1 2 3)"
