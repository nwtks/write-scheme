namespace WriteScheme.Test

open Xunit
open FsUnit.Xunit
open WriteScheme
open WriteScheme.Type

module LibraryTest =
    let evalAll input =
        match input |> Read.readAll false with
        | Ok exprs -> exprs |> Eval.eachEval (Repl.newContext ()) id (Ok(SUnspecified, None))
        | Error _ -> failwith "Parse failed"

    let check input expected =
        match input |> evalAll with
        | Ok res -> res |> Print.print |> should equal expected
        | Error(EvalError(msg, _)) -> failwithf "Eval failed: %s" msg
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
        | Ok(SString str, _) -> str.runes |> runesToString |> should equal "hello world"
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
        | Ok(SRational(n, d), _) ->
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
            sprintf
                """
        (define-library (example complex)
            (import (scheme base))
            (include "%s")
            (include-ci "%s")
            (include-library-declarations "%s")
            (cond-expand
                (r7rs (begin (define cond-val 12)))
                (else (begin (define cond-val 0)))))
        (import (example complex))
        (+ (helper1) (+ (helper2) cond-val))
        """
                tempFile1
                tempFile2
                tempFile3

        try
            match evalAll input with
            | Ok(SRational(n, d), _) ->
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

        sprintf "%s (import (only (lib) b))" lib
        |> evalAll
        |> function
            | Error(EvalError(msg, _)) -> msg |> should startWith "only: identifier 'b' not exported."
            | _ -> failwith "Expected error"

        sprintf "%s (import (except (lib) b))" lib
        |> evalAll
        |> function
            | Error(EvalError(msg, _)) -> msg |> should startWith "except: identifier 'b' not exported."
            | _ -> failwith "Expected error"

        sprintf "%s (import (rename (lib) (b x)))" lib
        |> evalAll
        |> function
            | Error(EvalError(msg, _)) -> msg |> should startWith "rename: identifier 'b' not exported."
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
