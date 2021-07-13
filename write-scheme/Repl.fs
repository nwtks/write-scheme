module Repl

open Type
open Read
open Print
open Eval
open Builtin

let rep envs = read >> eval envs id >> print

let rec repl output =
    let envs = extendEnvs builtin []
    printf "%s\n> " output

    try
        System.Console.ReadLine() |> rep envs |> repl
    with
    | ex -> ex.Message |> repl
