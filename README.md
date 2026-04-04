
# R7RS Scheme Interpreter in F#

A subset R7RS Scheme interpreter written in F#. Features a CPS (Continuation-Passing Style) based evaluator with first-class continuations and hygienic macros.

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
| `define` | Variable / procedure definition |
| `define-syntax` | Syntax definition |
| `syntax-rules` | Hygienic macros (pattern matching with ellipsis `...`) |
| `set!` | Variable assignment |
| `if` | Conditional branching |
| `cond` | Multi-way conditional (`else`, `=>` supported) |
| `and`, `or` | Short-circuit evaluation |
| `when`, `unless` | Conditional execution |
| `let`, `let*`, `letrec`, `letrec*` | Local bindings |
| `begin` | Sequential execution |

### Built-in Procedures

#### Equivalence
`eqv?`, `eq?`, `equal?`

#### Numeric
`+`, `-`, `*`, `/`, `=`, `<`, `>`, `<=`, `>=`, `number?`, `zero?`, `positive?`, `negative?`

#### Boolean
`not`, `boolean?`

#### List Operations
`cons`, `car`, `cdr`, `pair?`, `null?`, `list?`, `list`, `append`

#### Type Predicates
`symbol?`, `char?`, `string?`, `procedure?`

#### Higher-Order Functions
`apply`, `map`, `for-each`

#### Continuations
`call/cc`, `call-with-current-continuation`

#### Exception Handling
`with-exception-handler`, `raise`, `error`, `error-object?`, `error-object-message`, `error-object-irritants`

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
