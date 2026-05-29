# R7RS Scheme Interpreter in F#

A R7RS Scheme interpreter written in F#. Features a Continuation-Passing Style (CPS) evaluator with first-class continuations, hygienic macros, and a stack-safe execution model.

## Requirements

- .NET 8.0 SDK

## Build & Run

```bash
# Start the REPL
dotnet run --project write-scheme

# Run tests
dotnet test
```

## Implemented Features

### Data Types

| Type | Examples |
|------|----------|
| Boolean | `#t`, `#f`, `#true`, `#false` |
| Integer / Rational | `42`, `-1`, `1/2`, `10/3` |
| Real | `3.14`, `1e2`, `+inf.0`, `-inf.0`, `+nan.0` |
| Complex | `1+2i`, `1@1.57` (polar) |
| Numeric radix | `#x1F` (hex), `#o17` (octal), `#b1010` (binary) |
| Exactness | `#e1.0` (exact), `#i1/2` (inexact) |
| String | `"hello"`, `"\n"`, `"\x3071;"` (ぱ) |
| Character | `#\a`, `#\space`, `#\newline`, `#\x3071` |
| Symbol | `foo`, `+`, `list->vector`, `|two words|` |
| Pair / List | `(1 2 3)`, `(a . b)` |
| Vector | `#(1 2 3)` |
| Bytevector | `#u8(0 10 255)` |

### Special Forms

| Syntax | Description |
|--------|-------------|
| `quote`, `'` | Quotation |
| `lambda` | Closure creation (variadic `. rest` supported) |
| `if` | Conditional branching |
| `set!` | Variable assignment |
| `include`, `include-ci` | File inclusion (ci: case-insensitive) |
| `cond`, `case` | Multi-way conditional (`else`, `=>` supported) |
| `and`, `or` | Short-circuit evaluation |
| `when`, `unless` | Conditional execution |
| `cond-expand` | Feature-based conditional expansion |
| `let`, `let*`, `letrec`, `letrec*` | Local bindings |
| `let-values`, `let*-values` | Binding multiple values in a local scope |
| `begin` | Sequential execution |
| `do` | Iteration with variable updates |
| `delay`, `delay-force` | Lazy evaluation (promises) |
| `parameterize` | Dynamic binding of parameters |
| `guard` | Exception handling with condition matching |
| `quasiquote`, `` ` `` | Quasiquotation (`unquote` `,` / `unquote-splicing` `,@`) |
| `case-lambda` | Case-based lambda creation |
| `let-syntax`, `letrec-syntax` | Local macro bindings |
| `syntax-rules` | Hygienic macros (R7RS compliant: flexible ellipsis positions, custom ellipsis symbols, and escaping support) |
| `syntax-error` | Signalling a syntax error at expansion time |
| `import` | R7RS import sets (`only`, `except`, `prefix`, `rename` supported) |
| `define` | Variable / procedure definition (Tail-recursive internal definitions supported) |
| `define-values` | Binding multiple values returned by an expression |
| `define-syntax` | Syntax definition |
| `define-record-type` | R7RS record type definition |
| `define-library` | R7RS library definition |

### Performance & Reliability

- **Stack-Safe Evaluator**: Systematic use of Jump-based CPS and tail-recursive accumulators across the evaluator, macro engine, and list builtins ensures that deep recursion and large list processing never cause stack overflows.
- **Tail-recursive Internal Definitions**: Collection of internal definitions is performed tail-recursively, ensuring stack safety for large blocks of definitions.
- **First-Class Continuations**: Full support for `call/cc` and `dynamic-wind` enabled by the CPS architecture.
- **Hygienic Macros**: R7RS-compliant `syntax-rules` engine with support for flexible ellipsis positions, custom ellipsis symbols, and hygiene via automatic renaming.
- **Robust Exception Handling**: R7RS `guard` and `with-exception-handler` integrated with the CPS flow for predictable and safe error management.
- **Source-Mapped Errors**: Runtime errors include line and column information from the source.
- **R7RS Compliant Unicode Support**: Codepoint-aware string operations and full Unicode character support (Runes).
- **Cycle Detection**: Robust handling of cyclic lists using Floyd's cycle-finding algorithm for predicates (`list?`, `length`) and visited-set tracking for the printer to prevent infinite loops.
- **Numeric Tower**: Full support for integers, rationals, reals, and complex numbers.
- **Exactness**: Integers and rationals are exact; reals and complex numbers are inexact. `exact` and `inexact` procedures handle conversions.

### Built-in Procedures

#### Equivalence
`eqv?`,
`eq?`,
`equal?`

#### Numeric
`number?`,
`complex?`,
`real?`,
`rational?`,
`integer?`,
`exact?`,
`inexact?`,
`exact-integer?`,
`finite?`,
`infinite?`,
`nan?`,
`=`,
`<`,
`>`,
`<=`,
`>=`,
`zero?`,
`positive?`,
`negative?`,
`odd?`,
`even?`,
`max`,
`min`,
`+`,
`*`,
`-`,
`/`,
`abs`,
`floor/`,
`floor-quotient`,
`floor-remainder`,
`truncate/`,
`truncate-quotient`,
`truncate-remainder`,
`quotient`,
`remainder`,
`modulo`,
`gcd`,
`lcm`,
`numerator`,
`denominator`,
`floor`,
`ceiling`,
`truncate`,
`round`,
`rationalize`,
`exp`,
`log`,
`sin`,
`cos`,
`tan`,
`asin`,
`acos`,
`atan`,
`square`,
`sqrt`,
`exact-integer-sqrt`,
`expt`,
`make-rectangular`,
`make-polar`,
`real-part`,
`imag-part`,
`magnitude`,
`angle`,
`inexact`,
`exact`,
`number->string`,
`string->number`

#### Boolean
`not`,
`boolean?`,
`boolean=?`

#### List Operations
`pair?`,
`cons`,
`car`,
`cdr`,
`caar`...`cddr`,
`set-car!`,
`set-cdr!`,
`null?`,
`list?`,
`make-list`,
`list`,
`length`,
`append`,
`reverse`,
`list-tail`,
`list-ref`,
`list-set!`,
`memq`,
`memv`,
`member`,
`assq`,
`assv`,
`assoc`,
`list-copy`

#### Symbol Operations
`symbol?`,
`symbol=?`,
`symbol->string`,
`string->symbol`

#### Character Operations
`char?`,
`char=?`,
`char<?`,
`char>?`,
`char<=?`,
`char>=?`,
`char-ci=?`,
`char-ci<?`,
`char-ci>?`,
`char-ci<=?`,
`char-ci>=?`,
`char-alphabetic?`,
`char-numeric?`,
`char-whitespace?`,
`char-upper-case?`,
`char-lower-case?`,
`digit-value`,
`char->integer`,
`integer->char`,
`char-upcase`,
`char-downcase`,
`char-foldcase`

#### String Operations
`string?`,
`make-string`,
`string`,
`string-length`,
`string-ref`,
`string-set!`,
`string=?`,
`string<?`,
`string>?`,
`string<=?`,
`string>=?`,
`string-ci=?`,
`string-ci<?`,
`string-ci>?`,
`string-ci<=?`,
`string-ci>=?`,
`string-upcase`,
`string-downcase`,
`string-foldcase`,
`substring`,
`string-append`,
`string->list`,
`list->string`,
`string-copy`,
`string-copy!`,
`string-fill!`

#### Vector Operations
`vector?`,
`make-vector`,
`vector`,
`vector-length`,
`vector-ref`,
`vector-set!`,
`vector->list`,
`list->vector`,
`vector->string`,
`string->vector`,
`vector-copy`,
`vector-copy!`,
`vector-append`,
`vector-fill!`

#### Bytevector Operations
`bytevector?`,
`make-bytevector`,
`bytevector`,
`bytevector-length`,
`bytevector-u8-ref`,
`bytevector-u8-set!`,
`bytevector-copy`,
`bytevector-copy!`,
`bytevector-append`,
`utf8->string`,
`string->utf8`

#### Higher-Order Functions
`procedure?`,
`apply`,
`map`,
`string-map`,
`vector-map`,
`for-each`,
`string-for-each`,
`vector-for-each`

#### Continuations & Control
`call-with-current-continuation`,
`call/cc`,
`values`,
`call-with-values`,
`dynamic-wind`

#### Exception Handling
`with-exception-handler`,
`raise`,
`raise-continuable`,
`error`,
`error-object?`,
`error-object-message`,
`error-object-irritants`

#### Lazy Evaluation
`delay`,
`delay-force`,
`force`,
`promise?`,
`make-promise`

#### Parameters
`make-parameter`,
`parameterize`

#### I/O
`display`,
`load`

## R7RS Compliance & Known Issues

This project aims for R7RS (Small) compliance.
Current limitations and pending features include:
- **I/O**: File I/O procedures (ports) beyond `load` and `display` are not yet fully implemented.

## Architecture

### Project Layout

```
write-scheme/           # Interpreter core (F# executable)
  Type.fs               # SExpression type definitions and common utilities
  Read.fs               # FParsec-based parser
  Print.fs              # S-expression serializer
  DatumLabel.fs         # #N= / #N# datum label resolution
  Context.fs            # Execution context (environments, libraries, winders, handlers)
  Eval.fs               # CPS evaluator (eval / apply / eachEval)
  Builtin/              # Built-in procedures and special forms
    Helper.fs           # Shared helpers (invalid, mapResult, doWind, getRange, …)
    SpecialForm.fs      # lambda, define, let, if, cond, … special forms
    Macro.fs            # syntax-rules hygienic macro engine
    Math.fs             # Numeric operations (Numeric Tower: integer/rational/real/complex)
    List.fs, Str.fs, Char.fs, Vector.fs, ByteVector.fs
    Bool.fs, Symbol.fs, Core.fs
    Procedure.fs        # apply, map, for-each, call/cc, dynamic-wind, …
    Promise.fs          # delay / force
    SavedParameter.fs   # make-parameter / parameterize
    Exception.fs        # with-exception-handler, raise, guard
  Builtin.fs            # builtinBindings list and builtinContext construction
  Repl.fs               # rep function (string → evaluated result string)
  Program.fs            # Entry point (REPL loop)
write-scheme.test/      # xUnit test project
  *Test.fs              # Per-feature test modules
```

### Core Types

#### SExpression

Every value is represented as `SExpressionKind * Position option`. `Position option` carries source location for error messages.

| Case | Description |
|---|---|
| `SBool of bool` | `#t` / `#f` |
| `SRational of bigint * bigint` | Integer or rational (integers have denominator `1I`) |
| `SReal of float` | Floating-point (includes +inf.0, -inf.0, +nan.0) |
| `SComplex of Complex` | Complex number |
| `SString of SStringData` | Unicode string (array of `Rune`) |
| `SChar of Rune` | Character |
| `SSymbol of string` | Symbol |
| `SPair of SPairData` | Pair (mutable car/cdr) |
| `SVector of SExpression array` | Vector |
| `SByteVector of byte array` | Bytevector |
| `SEmpty` | `()` |
| `SUnspecified` | Unspecified return value |
| `SValues of SExpression list` | Multiple values |
| `SRecord` | Created by `define-record-type` |
| `SPromise` | `delay` / `force` |
| `SParameter` | `make-parameter` |
| `SProcedure of SProcedureKind` | Procedure |
| `SSyntax of SProcedureKind` | Special form (arguments passed unevaluated) |
| `SContinuation of SContinuation` | First-class continuation from `call/cc` |
| `SError` | Error object |

#### Procedure Signature

All built-in procedures and special forms share the `SProcedureKind` type:

```fsharp
type SProcedureKind =
    Context -> Position option -> SContinuation -> SExpression list -> Result<SExpression, SkipResult>
```

The evaluator is implemented in **Continuation-Passing Style (CPS)**. Every procedure receives a `SContinuation` and must pass its result to it rather than returning directly, which guarantees stack safety for arbitrarily deep recursion.

#### Error Variants

```fsharp
type SkipResult =
    | EvalError of string * Position option   // evaluation error (type errors, etc.)
    | ParseError of string * Position option  // parse error
    | SchemeRaise of SExpression * Position option // Scheme-level raise
```

## Dependencies

| Package | Version | Purpose |
|---|---|---|
| FParsec | 1.* | S-expression parser (`Read.fs`) |
| xUnit | (test project) | Test framework |
| FsUnit.Xunit | (test project) | `should equal` / `should startWith` assertion DSL |
| coverlet | (test project) | Code coverage measurement |
