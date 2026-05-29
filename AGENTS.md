# AGENTS.md

This file provides the information an AI agent needs to understand and work with this codebase. See README.md for architecture overview, type reference, and dependency list.

## Commands

```bash
# Build
dotnet build

# Run tests (with coverage report)
dotnet test

# Start REPL
dotnet run --project write-scheme
```

After any code change, run `dotnet test` and confirm **all tests pass**.

Maintain high unit test coverage (at least line ~80%).

## CPS Pattern

The entire evaluator is implemented in Continuation-Passing Style (CPS).

- **Always pass results to `cont`** — either `Ok value |> cont` or `Error e |> cont`.
- **Call `cont` in tail position** to guarantee stack safety.
- To abort evaluation early, return `Error e` directly (bypassing `cont`).

```fsharp
// Typical built-in procedure implementation pattern
let myProc context pos cont args =
    match args with
    | [ x ] ->
        // Process and pass to continuation
        doSomething x |> Ok |> cont
    | _ ->
        // Use the invalidParameter helper for argument errors
        args |> invalidParameter pos "'%s' invalid my-proc parameter."
```

## Adding a New Built-in Procedure

1. **Choose (or create) an implementation file** — pick the appropriate `Builtin/*.fs` for the feature.
2. **Implement the function** — follow the `SProcedureKind` signature.
3. **Register it in `builtinBindings` in `Builtin.fs`**:
   ```fsharp
   "my-proc", (SProcedure myProc, None) |> ref
   // For special forms:
   "my-syntax", (SSyntax mySyntax, None) |> ref
   ```
4. **Add tests** — add `[<Fact>]` entries to the appropriate `write-scheme.test/*Test.fs`.

## Writing Tests

```fsharp
module WriteScheme.Tests.MyTest

open Xunit
open FsUnit.Xunit

// rep evaluates a Scheme expression string and returns its printed result
let rep = WriteScheme.Repl.rep WriteScheme.Builtin.builtinContext

[<Fact>]
let ``my-proc`` () =
    "(my-proc 1 2)" |> rep |> should equal "3"
    // Check error messages with startWith (position info is appended at the end)
    "(my-proc)" |> rep |> should startWith "'()' invalid my-proc parameter"
```

- `rep` evaluates one Scheme expression and returns the result via `Print.print`.
- Error messages are verified with `should startWith "..."` because source position is appended at the end.

## Helper Functions (Builtin/Helper.fs)

| Function | Purpose |
|---|---|
| `invalid pos fmt expr` | Build and return an `EvalError` |
| `invalidParameter pos fmt args` | Print the argument list and build an `EvalError` |
| `mapResult f list` | Apply a `Result`-returning `f` to a list, stopping at the first error |
| `getRange length args` | Parse optional `start` / `stop` arguments and return an index range |
| `doWind context cont savedWinders arg` | Execute `dynamic-wind` winders during continuation invocation |
| `doAroundProc context cont before thunk after` | Execute `dynamic-wind` before/thunk/after sequence |
| `tryReadAll foldCase filename pos` | Read a file and call `Read.readAll` |

## Numeric Type Notes

- Integers and rationals are represented as `SRational(numerator: bigint, denominator: bigint)` — integers have denominator `1I`.
- Use `newInteger n` / `newSRational n d` to construct values (GCD-normalized automatically).
- `SZero = SRational(0I, 1I)` is a cached singleton.
- Strings are stored as `System.Text.Rune` arrays (Unicode code-point level operations).
