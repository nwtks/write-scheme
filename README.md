# R7RS Scheme Interpreter in F#

A R7RS (Small) Scheme interpreter written in F#. Features a Continuation-Passing Style (CPS) evaluator with first-class continuations (`call/cc`), hygienic `syntax-rules` macros, and the full numeric tower.

## Features

- **Full R7RS syntax**: lists, vectors, bytevectors, strings with Unicode scalar values, characters, rationals, reals, complex numbers, datum labels (`#N=`/`#N#`) for cyclic and shared structure
- **CPS evaluator**: stack-safe evaluation with tail-call optimization
- **First-class continuations**: `call/cc` and `dynamic-wind`
- **Hygienic macros**: R7RS `syntax-rules` with ellipsis, literals, custom ellipsis symbols, pattern wildcards, template escaping, and `syntax-error`; local macros with `let-syntax` / `letrec-syntax`
- **Numeric tower**: exact integers and rationals (`bigint`-based), inexact reals (IEEE 754 double), complex numbers
- **Library system**: R7RS `define-library` / `import` with set operations (`only`, `except`, `prefix`, `rename`)
- **Record types**: R7RS `define-record-type`
- **Exception handling**: `raise`, `raise-continuable`, `with-exception-handler`, `guard`, `error`, `error-object?` / `error-object-message` / `error-object-irritants`, `read-error?`, `file-error?`
- **Lazy evaluation**: `delay`, `delay-force`, `force`, `make-promise`
- **I/O ports**: string ports, bytevector ports, file ports; complete R7RS I/O primitives
- **Multiple values**: `values`, `call-with-values`, `define-values`, `let-values`, `let*-values`
- **Quasiquotation**: `` ` ``, `,`, `,@` with full nesting support
- **Dynamic binding**: `parameterize` / `make-parameter`
- **Conditional expansion**: `cond-expand` with feature-based macro-time dispatch
- **File inclusion**: `include`, `include-ci` at expansion time
- **Iteration**: `do`, `when`, `unless`
- **Argument-count dispatch**: `case-lambda`
- **First-class environments**: `environment`, `eval`, `interaction-environment`, `null-environment`, `scheme-report-environment`
- **File system**: `file-exists?`, `delete-file`
- **System interface**: `load`, `command-line`, `exit`, `emergency-exit`, `get-environment-variable`, `get-environment-variables`, `current-second`, `current-jiffy`, `jiffies-per-second`, `features`
- **Standard libraries**: `(scheme base)`, `(scheme case-lambda)`, `(scheme char)`, `(scheme complex)`, `(scheme cxr)`, `(scheme eval)`, `(scheme file)`, `(scheme inexact)`, `(scheme lazy)`, `(scheme load)`, `(scheme process-context)`, `(scheme r5rs)`, `(scheme read)`, `(scheme repl)`, `(scheme time)`, `(scheme write)`

## Requirements

- .NET 10.0 SDK

## Build & Run

```bash
# Start the REPL
dotnet run --project write-scheme

# Run a Scheme file
echo '(+ 1 2)' | dotnet run --project write-scheme

# Run tests (with coverage)
dotnet test

# Format code
dotnet fantomas write-scheme/ write-scheme.test/
```

## REPL Examples

```
> (+ 1 2)
3
> (map (lambda (x) (+ x 1)) '(1 2 3))
(2 3 4)
> (call/cc (lambda (k) (k 42)))
42
> (define-syntax my-or (syntax-rules () ((my-or) #f) ((my-or a) a) ((my-or a b ...) (let ((t a)) (if t t (my-or b ...))))))
my-or
> (my-or #f (+ 1 2))
3
```

## Documentation

| Document | Description |
|----------|-------------|
| [Language Reference](docs/language-reference.md) | Full language specification |
| [Architecture](docs/architecture.md) | Internal design (CPS, types, pipeline) |
| [Design Trade-offs](docs/trade-off.md) | Rationale for key decisions |
| [Recurring Gotchas](docs/gotchas.md) | Common pitfalls and non-obvious behaviors |
| [AGENTS.md](AGENTS.md) | Guidelines for AI agents working on this repo |
