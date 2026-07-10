module WriteScheme.Tests.LibraryTest

open Xunit
open FsUnit.Xunit

let evalAll input =
    match input |> WriteScheme.Read.readAll false with
    | Ok exprs ->
        exprs
        |> WriteScheme.Eval.eachEval (WriteScheme.Repl.newContext ()) id (Ok(WriteScheme.Type.SUnspecified, None))
    | Error _ -> failwith "Parse failed"

let check input expected =
    match input |> evalAll with
    | Ok res -> res |> WriteScheme.Print.print |> should equal expected
    | Error(WriteScheme.Type.EvalError(msg, _)) -> failwithf "Eval failed: %s" msg
    | Error e -> failwithf "Eval failed: %A" e

[<Fact>]
let ``define-library exports and imports correctly`` () =
    let input =
        """
    (define-library (example hello)
        (export hello)
        (import (scheme base))
        (begin
            (define (hello) "hello world")))
    (begin
        (import (example hello))
        (hello))
    """

    match evalAll input with
    | Ok(WriteScheme.Type.SString str, _) -> str.runes |> WriteScheme.Type.runesToString |> should equal "hello world"
    | _ -> failwith "Expected string 'hello world'"

[<Fact>]
let ``library isolated environment`` () =
    let input =
        """
    (define-library (example hidden)
        (export get-hidden)
        (import (scheme base))
        (begin
            (define hidden-val 42)
            (define (get-hidden) hidden-val)))
    (import (example hidden))
    (get-hidden)
    """

    match evalAll input with
    | Ok(WriteScheme.Type.SRational(n, d), _) ->
        n |> should equal 42I
        d |> should equal 1I
    | _ -> failwith "Expected 42"

[<Fact>]
let ``define-library handles include, include-ci, include-library-declarations, cond-expand`` () =
    let tempFile1 = System.IO.Path.GetTempFileName()
    let tempFile2 = System.IO.Path.GetTempFileName()
    let tempFile3 = System.IO.Path.GetTempFileName()
    System.IO.File.WriteAllText(tempFile1, "(define (helper1) 10)")
    System.IO.File.WriteAllText(tempFile2, "(DEFINE (HELPER2) 20)")
    System.IO.File.WriteAllText(tempFile3, "(export helper1 helper2 cond-val)")

    let input =
        $"""
    (define-library (example complex)
        (import (scheme base))
        (include "{tempFile1}")
        (include-ci "{tempFile2}")
        (include-library-declarations "{tempFile3}")
        (cond-expand
            (r7rs (begin (define cond-val 12)))
            (else (begin (define cond-val 0)))))
    (import (example complex))
    (+ (helper1) (+ (helper2) cond-val))
    """

    try
        match evalAll input with
        | Ok(WriteScheme.Type.SRational(n, d), _) ->
            n |> should equal 42I
            d |> should equal 1I
        | _ -> failwith "Expected 42"
    finally
        System.IO.File.Delete tempFile1
        System.IO.File.Delete tempFile2
        System.IO.File.Delete tempFile3

[<Fact>]
let ``import sets: only`` () =
    let input =
        """
    (define-library (lib) (export a b c) (import (scheme base)) (begin (define a 1) (define b 2) (define c 3)))
    (import (only (lib) a c))
    (list a c)
    """

    check input "(1 3)"

[<Fact>]
let ``import sets: except`` () =
    let input =
        """
    (define-library (lib) (export a b c) (import (scheme base)) (begin (define a 1) (define b 2) (define c 3)))
    (import (except (lib) b))
    (list a c)
    """

    check input "(1 3)"

[<Fact>]
let ``import sets: prefix`` () =
    let input =
        """
    (define-library (lib) (export a b) (import (scheme base)) (begin (define a 1) (define b 2)))
    (import (prefix (lib) pre:))
    (list pre:a pre:b)
    """

    check input "(1 2)"

[<Fact>]
let ``import sets: rename`` () =
    let input =
        """
    (define-library (lib) (export a b) (import (scheme base)) (begin (define a 1) (define b 2)))
    (import (rename (lib) (a x) (b y)))
    (list x y)
    """

    check input "(1 2)"

[<Fact>]
let ``import sets: nested`` () =
    let input =
        """
    (define-library (lib) (export a b c) (import (scheme base)) (begin (define a 1) (define b 2) (define c 3)))
    (import (prefix (only (lib) a b) p:))
    (list p:a p:b)
    """

    check input "(1 2)"

[<Fact>]
let ``import sets: error cases`` () =
    let lib =
        "(define-library (lib) (export a) (import (scheme base)) (begin (define a 1)))"

    $"{lib} (import (only (lib) b))"
    |> evalAll
    |> function
        | Error(WriteScheme.Type.EvalError(msg, _)) -> msg |> should startWith "only: identifier 'b' not exported."
        | _ -> failwith "Expected error"

    $"{lib} (import (except (lib) b))"
    |> evalAll
    |> function
        | Error(WriteScheme.Type.EvalError(msg, _)) -> msg |> should startWith "except: identifier 'b' not exported."
        | _ -> failwith "Expected error"

    $"{lib} (import (rename (lib) (b x)))"
    |> evalAll
    |> function
        | Error(WriteScheme.Type.EvalError(msg, _)) -> msg |> should startWith "rename: identifier 'b' not exported."
        | _ -> failwith "Expected error"

[<Fact>]
let ``export: rename`` () =
    let input =
        """
    (define-library (lib)
        (export (rename a x) (rename b y))
        (import (scheme base))
        (begin (define a 1) (define b 2)))
    (import (lib))
    (list x y)
    """

    check input "(1 2)"

[<Fact>]
let ``transitive imports`` () =
    let input =
        """
    (define-library (l1)
        (export a)
        (import (scheme base))
        (begin (define a 10)))
    (define-library (l2)
        (export b)
        (import (scheme base) (l1))
        (begin (define b (+ a 5))))
    (import (l2))
    b
    """

    check input "15"

[<Fact>]
let ``multiple imports in define-library`` () =
    let input =
        """
    (define-library (l1) (export a) (import (scheme base)) (begin (define a 1)))
    (define-library (lib)
        (export val)
        (import (scheme base) (l1))
        (begin (define val (+ a 10))))
    (import (lib))
    val
    """

    check input "11"

[<Fact>]
let ``top-level cond-expand with import`` () =
    let input =
        """
    (cond-expand
        (r7rs (import (scheme base))))
    (list 1 2)
    """

    check input "(1 2)"

[<Fact>]
let ``empty export list`` () =
    let input =
        """
    (define-library (lib)
        (export)
        (import (scheme base))
        (begin (define a 1)))
    (import (lib))
    (list 1)
    """

    check input "(1)"

[<Fact>]
let ``redefining library`` () =
    let input =
        """
    (define-library (lib) (export a) (import (scheme base)) (begin (define a 1)))
    (define-library (lib) (export a) (import (scheme base)) (begin (define a 2)))
    (import (lib))
    a
    """

    check input "2"
