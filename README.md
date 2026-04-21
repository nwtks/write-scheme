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

- **Stack-Safe Evaluator**: Systematic use of Jump-based CPS and tail-recursive accumulators across the evaluator, macro engine, and list builtins ensures that deep recursion and large list processing never cause stack overflows.
- **First-Class Continuations**: Full support for `call/cc` and `dynamic-wind` enabled by the CPS architecture.
- **Hygienic Macros**: R7RS-compliant `syntax-rules` engine with support for flexible ellipsis positions, custom ellipsis symbols, and hygiene via automatic renaming.
- **Robust Exception Handling**: R7RS `guard` and `with-exception-handler` integrated with the CPS flow for predictable and safe error management.
- **Source-Mapped Errors**: Runtime errors include line and column information from the source.
- **R7RS Compliant Unicode Support**: Codepoint-aware string operations and full Unicode character support (Runes).
- **Cycle Detection**: Robust handling of cyclic lists using Floyd's cycle-finding algorithm for predicates (`list?`, `length`) and visited-set tracking for the printer to prevent infinite loops.

### Built-in Procedures

#### Equivalence
`eqv?`, `eq?`, `equal?`

#### Numeric
`+`, `-`, `*`, `/`, `=`, `<`, `>`, `<=`, `>=`, `number?`, `complex?`, `real?`, `rational?`, `integer?`, `exact?`, `inexact?`, `exact-integer?`, `finite?`, `infinite?`, `nan?`, `zero?`, `positive?`, `negative?`, `odd?`, `even?`, `make-rectangular`, `make-polar`, `real-part`, `imag-part`, `magnitude`, `angle`, `number->string`, `string->number`

#### Boolean
`not`, `boolean?`, `boolean=?`

#### List Operations
`cons`, `car`, `cdr`, `caar`...`cddr`, `set-car!`, `set-cdr!`, `pair?`, `null?`, `list?`, `make-list`, `list`, `length`, `append`, `reverse`, `list-tail`, `list-ref`, `memq`, `memv`, `member`, `assq`, `assv`, `assoc`, `list-copy`

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

This project aims for R7RS (Small) compliance. Current limitations and pending features include:

- **Libraries**: The `define-library` system (modules) is not yet implemented.
- **I/O**: File I/O procedures (ports) beyond `load` and `display` are not yet fully implemented.
- **Macros**: While the `syntax-rules` engine is robust, some advanced R7RS edge cases in pattern matching may still be limited.

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
