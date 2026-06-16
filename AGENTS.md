# AGENTS.md

This file provides guidance for AI agents working in this repository.

## AGENTS.md Editing Rules

- **Don't write what's in the codebase** — information that can be obtained by reading source code or project files must not be written in AGENTS.md.
- **Don't duplicate README.md** — content already described in README.md should only be referenced by a link (`See [README.md](...)`).

### Documentation Location Rules

| Topic | Destination |
|-------|-------------|
| The language specification | `docs/language-reference.md` |
| Architecture and design discussions | `docs/architecture.md` |
| Design trade-offs | `docs/trade-off.md` |
| Common mistakes / gotchas | `docs/gotchas.md` |

- **When a design decision, trade-off, bug fix, or known issue occurs, update `docs/trade-off.md` or `docs/gotchas.md` immediately (in the same session) — do not defer.**
- When a new trade-off or gotcha arises, first consider appending to the relevant `docs/` file. Only add to AGENTS.md if it's an "implicit rule not obvious from the codebase."
- Only keep project-specific implicit rules in AGENTS.md. The topics above belong in their corresponding `docs/*.md` files.

---

## Documentation

| Document | Description |
|----------|-------------|
| [Language Reference](docs/language-reference.md) | Full language specification |
| [Architecture](docs/architecture.md) | Internal design (CPS, types, pipeline) |
| [Design Trade-offs](docs/trade-off.md) | Rationale for key decisions |
| [Recurring Gotchas](docs/gotchas.md) | Common pitfalls and non-obvious behaviors |

---

## Cross-Platform Compatibility

All code — including test code — must work on **both Windows and Linux**.

---

## Coding Conventions

- Prefer functional programming idioms over imperative ones throughout the codebase — including test code.
- **Favor expressions over statements** — Use `match` expressions, `if`/`then`/`else`, and pattern matching instead of imperative control flow.
- **Leverage discriminated unions** — Model domain concepts (`SExpressionKind`, `SkipResult`, `SBinding`) with DUs for exhaustiveness checking.
- **Use `[<TailCall>]` on recursive functions** that loop to prevent stack overflows.
- Do not introduce new external NuGet packages without checking existing dependencies in the `.fsproj` files first.
- **Cyclomatic complexity** — Every function/method must keep its Coverlet complexity ≤ 15 (hard limit). Keep it ≤ 10 where practical.
- **Naming conventions** —
  - Built-in procedures: `s` prefix (e.g., `sCons`, `sMap`, `sStringRef`).
  - Predicates: `is` prefix (e.g., `isPair`, `isChar`, `isProperList`).
- **Constructor helpers** — Use the normalization helpers instead of directly constructing types:
  - `newInteger n` / `newSRational num den` for numbers
  - `SZero = SRational(0I, 1I)` is a cached singleton.
  - `newSString isImmutable str` / `runesToString runes` for strings
  - `toSBool` / `fromSBool` for boolean conversions

---

## Builtin Module Structure

1. **Choose (or create) an implementation file** — pick the appropriate `Builtin/*.fs` for the feature.
2. **Implement the function** — every `Builtin/*.fs` file must follow this template:

   ```fsharp
   namespace WriteScheme.Builtins

   open WriteScheme
   open Type

   [<AutoOpen>]
   module ModuleName =
       let fnName context pos cont =
           function
           | [ args ] -> ... |> cont
           | x -> x |> invalidParameter pos "invalid parameter." |> cont
   ```

3. **Register it in `builtinBindings`** in `Builtin.fs` by adding an entry to the `builtinBindings` list with the `SProcedureKind`.
4. **Add the new file to `write-scheme.fsproj`** **before** `Builtin.fs` (the aggregator), following the existing alphabetical order.
5. **Add tests** — add `[<Fact>]` entries to the appropriate `write-scheme.test/*Test.fs`.

---

## Testing Conventions

- After any code change, run `dotnet test` and confirm **all tests pass**.
- The `dotnet test` output includes a **Cyclomatic Complexity Report** (from coverage data). Check that no function exceeds complexity 15 (error threshold). Warnings above 10 should be addressed where practical.
- Maintain high unit test coverage (current: ~85%, target: ≥ 90% line coverage). If line coverage falls below 85%, add test code to restore it.

---

## Test Module Structure

- Use `module WriteScheme.Tests.TestName` (module declaration), not `namespace`.
- Open `Xunit` and `FsUnit.Xunit` at the top of each test file.
- Test names use backtick syntax: ``let ``test name`` () =``
- Use `[<Fact>]` attributes for individual test cases.
- **Test context setup** —
  - `let rep = WriteScheme.Repl.rep WriteScheme.Builtin.builtinContext` — shared context, suitable for most tests.
  - `let newRep () = ...` (using `Repl.newContext()`) — fresh context per call, for tests that need isolation.

```fsharp
module WriteScheme.Tests.ListTest

open Xunit
open FsUnit.Xunit

let rep = WriteScheme.Repl.rep WriteScheme.Builtin.builtinContext

[<Fact>]
let ``pair?`` () =
    "(pair? '(a . b))" |> rep |> should equal "#t"
```

### Evaluation helpers

- `rep` evaluates one Scheme expression and returns the result via `Print.print`.
  Suitable for most tests. Uses `Builtin.builtinContext` (shared state).
- `newRep ()` creates a fresh context per call. Use when tests need isolation
  (e.g., modifying global state, `define`, `set!`, macros).
- Use `|> rep |> ignore` for expressions evaluated only for side effects.

```fsharp
let newRep () =
    WriteScheme.Repl.newContext () |> WriteScheme.Repl.rep

[<Fact>]
let ``define`` () =
    let rep = newRep ()
    "(define x 42)" |> rep |> ignore
    "x" |> rep |> should equal "42"
```

### Assertion patterns

- Use `should equal` for result comparison (string output from the printer).
- Error messages are verified with `should startWith "..."` because source position is appended at the end.
- Results are always compared as strings — the REPL returns `Print.print` output.
