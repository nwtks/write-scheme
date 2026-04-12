
# R7RS Scheme Interpreter in F#

A subset R7RS Scheme interpreter written in F#. Features a CPS (Continuation-Passing Style) based evaluator with first-class continuations and macros.

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
| String | `"hello"`, `"\n"`, `"\x3071;"` (ぱ) |
| Character | `#\a`, `#\space`, `#\newline`, `#\x3071` |
| Symbol | `foo`, `+`, `list->vector`, `\|two words\|` |
| Pair / List | `(1 2 3)`, `(a . b)` |
| Numeric radix | `#x1F` (hex), `#o17` (octal), `#b1010` (binary) |

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

### Built-in Procedures

#### Equivalence
`eqv?`, `eq?`, `equal?`

#### Numeric
`+`, `-`, `*`, `/`, `=`, `<`, `>`, `<=`, `>=`, `number?`, `zero?`, `positive?`, `negative?`

#### Boolean
`not`, `boolean?`

#### List Operations
`cons`, `car`, `cdr`, `pair?`, `null?`, `list?`, `list`, `append`

#### Vector Operations
`vector?`, `make-vector`, `vector`, `vector-length`, `vector-ref`, `vector-set!`, `vector->list`, `list->vector`, `vector-fill!`

#### Type Predicates
`boolean?`, `number?`, `symbol?`, `char?`, `string?`, `procedure?`

#### Higher-Order Functions
`apply`, `map`, `for-each`

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

## Architecture

```
Read.fs    — Parser built with FParsec (tokenizing and parsing S-expressions)
Type.fs    — S-expression type definitions (SExpression discriminated union)
Eval.fs    — CPS-based evaluator (eval / apply)
Print.fs   — S-expression serialization
Builtin.fs — Built-in procedures and special forms
Repl.fs    — Read-Eval-Print loop
```
