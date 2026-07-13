# R7RS Scheme Interpreter in F#

A R7RS (Small) Scheme interpreter written in F#. Features a Continuation-Passing Style (CPS) evaluator with first-class continuations (`call/cc`), hygienic `syntax-rules` macros, and the full numeric tower.

## Features

- **Full R7RS syntax**: lists, vectors, bytevectors, strings with Unicode scalar values, characters, rationals, reals, complex numbers
- **CPS evaluator**: stack-safe evaluation with tail-call optimization
- **First-class continuations**: `call/cc` and `dynamic-wind`
- **Hygienic macros**: R7RS `syntax-rules` with ellipsis, literals, custom ellipsis symbols, and `syntax-error`
- **Numeric tower**: exact integers and rationals (`bigint`-based), inexact reals (IEEE 754 double), complex numbers
- **Library system**: R7RS `define-library` / `import` with set operations (`only`, `except`, `prefix`, `rename`)
- **Record types**: R7RS `define-record-type`
- **Exception system**: `raise`, `raise-continuable`, `with-exception-handler`, `guard`, `error`
- **Lazy evaluation**: `delay`, `delay-force`, `force`, `make-promise`
- **Datum labels**: `#N=` / `#N#` for cyclic and shared structure
- **I/O ports**: string ports, bytevector ports, file ports; R7RS I/O primitives

## Requirements

- .NET 10.0 SDK

## Build & Run

```bash
# Start the REPL
dotnet run --project write-scheme

# Run a Scheme file
echo '(+ 1 2)' | dotnet run --project write-scheme

# Run tests (with coverage via coverlet)
dotnet test
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
