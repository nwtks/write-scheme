# R7RS Scheme Interpreter in F#

A subset R7RS Scheme interpreter written in F#. Features a CPS (Continuation-Passing Style) based evaluator with first-class continuations, hygienic macros, and a stack-safe execution model.

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
| String | `"hello"`, `"\n"`, `"\x3071;"` (ぱ) |
| Character | `#\a`, `#\space`, `#\newline`, `#\x3071` |
| Symbol | `foo`, `+`, `list->vector`, `|two words|` |
| Pair / List | `(1 2 3)`, `(a . b)` |
| Vector | `#(1 2 3)` |
| Bytevector | `#u8(0 10 255)` |
| Numeric radix | `#x1F` (hex), `#o17` (octal), `#b1010` (binary) |
| Exactness | `#e1.0` (exact), `#i1/2` (inexact) |

### Special Forms

| Syntax | Description |
|--------|-------------|
| `quote`, `'` | Quotation |
| `quasiquote`, `` ` `` | Quasiquotation (`unquote` `,` / `unquote-splicing` `,@`) |
| `lambda` | Closure creation (variadic `. rest` supported) |
| `set!` | Variable assignment |
| `if` | Conditional branching |
| `cond`, `case` | Multi-way conditional (`else`, `=>` supported) |
| `and`, `or` | Short-circuit evaluation |
| `when`, `unless` | Conditional execution |
| `let`, `let*`, `letrec`, `letrec*` | Local bindings |
| `let-values`, `let*-values` | Binding multiple values in a local scope |
| `begin` | Sequential execution |
| `do` | Iteration with variable updates |
| `delay`, `delay-force` | Lazy evaluation (promises) |
| `parameterize` | Dynamic binding of parameters |
| `guard` | Exception handling with condition matching |
| `let-syntax`, `letrec-syntax` | Local macro bindings |
| `syntax-rules` | Hygienic macros (R7RS compliant: flexible ellipsis positions, custom ellipsis symbols, and escaping support) |
| `syntax-error` | Signalling a syntax error at expansion time |
| `define` | Variable / procedure definition |
| `define-values` | Binding multiple values returned by an expression |
| `define-syntax` | Syntax definition |
| `define-record-type` | R7RS record type definition |

### Performance & Reliability

- **Stack-Safe Evaluator**: The core evaluator uses a Jump-based Continuation-Passing Style (CPS) to ensure that Deep recursion (including tail calls) works indefinitely without consuming stack frames.
- **First-Class Continuations**: Full support for `call/cc` enabled by the CPS architecture.
- **Robust Exception Handling**: R7RS `guard` and `with-exception-handler` integrated with the CPS flow for predictable and safe error management.
- **Source-Mapped Errors**: Runtime errors include line and column information from the source.
- **R7RS Compliant Unicode Support**: Full support for Unicode scalar values (Runes). Procedures like `string-ref`, `string-length`, and `string-map` are codepoint-aware and handle surrogate pairs (e.g., 🍎) correctly.

### Built-in Procedures

#### Equivalence
`eqv?`, `eq?`, `equal?`

#### Numeric
`+`, `-`, `*`, `/`, `=`, `<`, `>`, `<=`, `>=`, `number?`, `complex?`, `real?`, `rational?`, `integer?`, `exact?`, `inexact?`, `exact-integer?`, `finite?`, `infinite?`, `nan?`, `zero?`, `positive?`, `negative?`, `odd?`, `even?`, `make-rectangular`, `make-polar`, `real-part`, `imag-part`, `magnitude`, `angle`, `number->string`, `string->number`

#### Boolean
`not`, `boolean?`, `boolean=?`

#### List Operations
`cons`, `car`, `cdr`, `caar`...`cddr`, `pair?`, `null?`, `list?`, `make-list`, `list`, `length`, `append`, `reverse`, `list-tail`, `list-ref`, `memq`, `memv`, `member`, `assq`, `assv`, `assoc`, `list-copy`

#### Symbol Operations
`symbol?`, `symbol=?`, `symbol->string`, `string->symbol`

#### Character Operations
`char?`, `char=?`, `char<?`, `char>?`, `char<=?`, `char>=?`, `char-ci=?`, `char-ci<?`, `char-ci>?`, `char-ci<=?`, `char-ci>=?`, `char-alphabetic?`, `char-numeric?`, `char-whitespace?`, `char-upper-case?`, `char-lower-case?`, `digit-value`, `char->integer`, `integer->char`, `char-upcase`, `char-downcase`, `char-foldcase`

#### String Operations
`string?`, `make-string`, `string`, `string-length`, `string-ref`, `string-set!`, `string=?`, `string<?`, `string>?`, `string<=?`, `string>=?`, `string-ci=?`, `string-ci<?`, `string-ci>?`, `string-ci<=?`, `string-ci>=?`, `string-upcase`, `string-downcase`, `string-foldcase`, `substring`, `string-append`, `string->list`, `list->string`, `string-copy`, `string-copy!`, `string-fill!`

#### Vector Operations
`vector?`, `make-vector`, `vector`, `vector-length`, `vector-ref`, `vector-set!`, `vector->list`, `list->vector`, `vector->string`, `string->vector`, `vector-copy`, `vector-copy!`, `vector-append`, `vector-fill!`

#### Bytevector Operations
`bytevector?`, `make-bytevector`, `bytevector`, `bytevector-length`, `bytevector-u8-ref`, `bytevector-u8-set!`, `bytevector-copy`, `bytevector-copy!`, `bytevector-append`, `utf8->string`, `string->utf8`

#### Higher-Order Functions
`apply`, `map`, `string-map`, `vector-map`, `for-each`, `string-for-each`, `vector-for-each`

#### Continuations & Control
`call/cc`, `call-with-current-continuation`, `values`, `call-with-values`, `dynamic-wind`

#### Lazy Evaluation
`delay`, `delay-force`, `force`, `promise?`, `make-promise`

#### Exception Handling
`with-exception-handler`, `raise`, `error`, `error-object?`, `error-object-message`, `error-object-irritants`

#### Parameters
`make-parameter`, `parameterize`

#### I/O
`display`, `load`

## R7RS Compliance & Known Issues

This project aims for R7RS (Small) compliance, but some features are currently limited:

### Mutability
- **Strings**: Fully mutable and codepoint-aware. Procedures like `string-set!`, `string-copy!`, and `string-fill!` are supported.
- **Pairs**: The internal representation uses optimized list structures, making `set-car!` and `set-cdr!` challenging. Full support for mutable pairs is a work-in-progress.

### Other Limitations
- **Number Tower**: Limited support for complex numbers and inexact reals.
- **Macros**: `syntax-rules` support is limited in some edge cases.
- **Libraries**: The `define-library` system is not yet fully implemented.

## Architecture

```
Read.fs    — Parser built with FParsec (tokenizing and parsing S-expressions)
Type.fs    — S-expression type definitions (SExpression discriminated union)
Eval.fs    — CPS-based evaluator (eval / apply)
Print.fs   — S-expression serialization
Builtin.fs — Built-in procedures registration
Builtin/   - Implementation of built-in procedures and special forms
Repl.fs    — Read-Eval-Print loop
```
