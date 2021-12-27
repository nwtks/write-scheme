module Repl

open Read
open Print
open Eval
open Builtin

let rep envs = read >> eval envs id >> print

let newEnvs () = extendEnvs builtin []

let rec repl () =
    let envs = newEnvs ()

    let rec repl' output =
        try
            printf "%s\n> " output
            System.Console.ReadLine() |> rep envs |> repl'
        with
        | ex -> ex.Message |> repl'

    repl' "Welcome"
