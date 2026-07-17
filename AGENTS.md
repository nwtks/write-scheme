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

See [README.md](README.md) for the full documentation index.

---

## Cross-Platform Compatibility

All code — including test code — must work on **both Windows and Linux**.

### Path handling

- **Scheme-level paths** (strings passed to `include`, `include-ci`, `load`, etc.) use forward slashes. They are passed directly to `System.IO.File.ReadAllText`, which handles both `/` and `\` on Windows automatically.
- **Temporary files in tests** — use `System.IO.Path.GetTempFileName()` and always clean up with `System.IO.File.Delete` inside a `try`/`finally` block:

  ```fsharp
  let tmp = System.IO.Path.GetTempFileName()
  try
      System.IO.File.WriteAllText(tmp, content)
      // ... test logic ...
  finally
      System.IO.File.Delete tmp
  ```

### File system assumptions

- **Case sensitivity** — Linux file systems are case-sensitive, Windows is not. When creating or comparing file paths in tests, use the exact case. Do not assume two paths differing only in case refer to the same file.
- **Clean up temporary files** — tests that create files must delete them in a `finally` block to avoid leaving artifacts on any platform.
- **Only use `FileNotFoundException`** for I/O error handling. Avoid catching other filesystem exceptions that may differ across platforms.

### Line endings

- **Scheme `newline`** prints a single `\n` character (`U+000A`). This is consistent across platforms.
- **F# string literals** — use `\n` for newlines.
- **The parser** handles `\r\n`, `\n`, and `\r` line endings uniformly.

### Platform-specific APIs

- **No P/Invoke** and no Windows-only / Linux-only native APIs.
- **Managed platform queries are allowed** where R7RS requires them — e.g. `OperatingSystem.IsWindows()` / `IsLinux()`, `BitConverter.IsLittleEndian`, and `RuntimeInformation.ProcessArchitecture` for `features` / `cond-expand`.
- **Process execution** — the interpreter runs single-threaded; do not introduce threading or process APIs beyond intentional exit (`Environment.Exit` for `exit` / `emergency-exit`).
- **Console** — use `System.Console.ReadLine()` / `System.Console.Write` only; avoid console color or terminal-specific APIs.

---

## Coding Conventions

- **Functional-first** — Prefer functional programming idioms over imperative ones throughout the codebase — including test code. Use recursion, immutability, and composition over loops, mutation, and statements.
- **Favor expressions over statements** — Use `match` expressions, `if`/`then`/`else`, and pattern matching instead of imperative control flow. Every branch should produce a value.
- **No exception-based error flow** — Expected errors (type errors, invalid arguments, file not found) are returned as `Result` values, never thrown. See [`docs/gotchas.md`](docs/gotchas.md) for the `failwith "unreachable."` convention.
- **Discriminated unions** — Model domain concepts with DUs for exhaustiveness checking. Use named fields when a case carries multiple values of the same type.
- **`[<TailCall>]` attribute** — Every recursive function that performs iteration must have `[<TailCall>]`.
- **Pattern matching over functions** — Prefer pattern matching (`function` keyword) over explicit `match` with a named parameter.
- **Pipeline style** — Use `|>` for value-forwarding and `>>` for function composition. Break long pipelines with `|>` at the start of continuation lines.
- **Mutability** — Use `mutable` fields and `ref` cells sparingly, only where Scheme semantics require it (pairs: `set-car!`/`set-cdr!`; environment mutation via `define`/`set!`). Prefer immutable data for everything else.
- Do not introduce new external NuGet packages without checking existing dependencies in the `.fsproj` files first.

---

## Builtin Module Structure

All Builtin files **except `Number.fs`** use `[<AutoOpen>]` modules to avoid explicit import boilerplate in `Builtin.fs`. `Number.fs` defines the `SNumber` DU type at namespace level (without `[<AutoOpen>]`), with its arithmetic functions inside `module SNumber`.

See [`docs/trade-off.md#14-autoopen-modules-in-builtin-vs-explicit-imports`](docs/trade-off.md#14-autoopen-modules-in-builtin-vs-explicit-imports).

1. **Choose (or create) an implementation file** — pick the appropriate `Builtin/*.fs` for the feature.
2. **Implement the function** — follow the existing patterns: `[<AutoOpen>]` module, `open WriteScheme`, `open Type`, and the `SProcedureKind` signature (`context -> pos -> cont -> args`).
3. **Register it** in the `builtinBindings` list in `Builtin.fs`.
4. **Add the new file to `write-scheme.fsproj`** **before** `Builtin.fs` (the aggregator), following the existing dependency order.
5. **Add tests** — add test cases to the appropriate test file.

---

## Testing Conventions

### Running Tests

- After any code change, run `dotnet test` and confirm **all tests pass**.
- The test run automatically checks cyclomatic complexity (via `Directory.Build.targets`). No function may exceed complexity 15; warnings above 10 should be addressed where practical. See [`docs/architecture.md#14-cyclomatic-complexity-guidelines`](docs/architecture.md#14-cyclomatic-complexity-guidelines).
- For standalone complexity analysis: `dotnet fsi scripts/check-complexity.fsx`
- Maintain high unit test coverage (target: ≥ 90% line coverage). If line coverage falls below 90%, add test code to restore it.

### What to Test

- **Happy path** — Normal usage with typical inputs.
- **Edge cases** — Empty lists, single-element lists, improper lists, zero/negative numbers, NaN, infinity, Unicode (emoji, supplementary characters).
- **Immutability guards** — Ensure mutable operations (`string-set!`, `vector-set!`) reject immutable inputs.
- **Error messages** — Verify that invalid arguments, missing arguments, and type errors produce informative messages (see [`docs/gotchas.md`](docs/gotchas.md)).
- **Cycle safety** — Test circular pairs/lists with `equal?`, the printer, and list operations.
- **Arity boundaries** — Zero arguments, one argument, many arguments, wrong number of arguments.
- **`call/cc` interactions** — Test first-class continuations with procedures that capture/restore state.

### Assertion patterns

- Use `should equal` for result comparison (string output from the printer).
- **EvalError messages** are verified with `should startWith "..."` because source position is appended at the end.
- **SError-based errors** (from `read-error?`/`file-error?` etc.) print as `#<error "...">` objects — use `should haveSubstring "..."` for these.
- Results are always compared as strings — the REPL returns `Print.print` output.
