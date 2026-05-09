namespace WriteScheme.Test

open Xunit
open FsUnit.Xunit
open WriteScheme
open WriteScheme.Type

module LibraryTest =
    let evalAll input =
        let envs = Context.extendEnvs Builtin.builtin []

        match Read.readAll false input with
        | Ok exprs -> exprs |> Eval.eachEval envs id (Ok(SUnspecified, None))
        | Error _ -> failwith "Parse failed"

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
        | Ok(SString str, _) -> runesToString str.runes |> should equal "hello world"
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
