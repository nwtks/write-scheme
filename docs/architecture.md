# Architecture

This document describes the internal architecture of the Scheme interpreter.

---

## Table of Contents

- [1. Project Layout](#1-project-layout)
- [2. Core Design: Continuation-Passing Style (CPS)](#2-core-design-continuation-passing-style-cps)
- [3. Core Types (`Type.fs`)](#3-core-types-typefs)
- [4. Pipeline Overview](#4-pipeline-overview)
- [5. Reading Phase (`Read.fs`)](#5-reading-phase-readfs)
- [6. Datum Label Resolution (`DatumLabel.fs`)](#6-datum-label-resolution-datumlabelfs)
- [7. Evaluation Phase (`Eval.fs`)](#7-evaluation-phase-evalfs)
- [8. Context System (`Context.fs`)](#8-context-system-contextfs)
- [9. Built-in Procedures and Special Forms](#9-built-in-procedures-and-special-forms)
- [10. Macro System (`Builtin/Macro.fs`)](#10-macro-system-builtinmacrofs)
- [11. Print Phase (`Print.fs`)](#11-print-phase-printfs)
- [12. REPL Loop (`Repl.fs` / `Program.fs`)](#12-repl-loop-replfs--programfs)
- [13. Data Flow Summary](#13-data-flow-summary)
- [14. Cyclomatic Complexity Guidelines](#14-cyclomatic-complexity-guidelines)

---

## 1. Project Layout

```
write-scheme/                     # Interpreter core (F# executable)
├── Type.fs                       # SExpression type definitions, constructors, utilities
├── Read.fs                       # FParsec-based parser
├── DatumLabel.fs                 # #N= / #N# datum label resolution
├── Print.fs                      # S-expression serializer
├── Context.fs                    # Execution context: environments, libraries, winders, handlers
├── Eval.fs                       # CPS evaluator: eval / apply / eachEval
├── Builtin/
│   ├── Helper.fs                 # Shared helpers: invalid, invalidParameter, mapResult
│   ├── SpecialForm.fs            # Special forms: quote, lambda, if, set!, begin, define, include, include-ci
│   ├── Binding.fs                # let/let*/letrec/let-values binding helpers
│   ├── Conditional.fs            # cond/case helper functions (isElseClause, normalizeCaseClause)
│   ├── Lazy.fs                   # delay / delay-force (syntax); force / promise? / make-promise (procedures)
│   ├── DynamicBinding.fs         # make-parameter / parameterize
│   ├── Exception.fs              # with-exception-handler, raise, guard
│   ├── Quasiquote.fs             # quasiquote special form; QqKeyword DU and expansion helpers
│   ├── Macro.fs                  # syntax-rules hygienic macro engine
│   ├── Record.fs                 # define-record-type implementation
│   ├── Library.fs                # define-library / import set operations
│   ├── Core.fs                   # eqv?, equal?
│   ├── Port.fs                   # String / bytevector / file ports; read/write/display I/O
│   ├── Number.fs                 # SNumber type (NRational|NReal|NComplex) and unified arithmetic
│   ├── Math.fs                   # Numeric tower operations
│   ├── Bool.fs                   # Boolean operations
│   ├── List.fs                   # Pair/list operations
│   ├── Symbol.fs                 # Symbol operations
│   ├── Char.fs                   # Character operations
│   ├── Str.fs                    # String operations
│   ├── Vector.fs                 # Vector operations
│   ├── ByteVector.fs             # Bytevector operations
│   ├── Procedure.fs              # apply, map, for-each, call/cc, dynamic-wind, values
│   ├── Environment.fs            # environment, eval, interaction-environment, null-environment, scheme-report-environment
│   └── SystemInterface.fs        # load, file-exists?, delete-file, command-line, exit, emergency-exit, get-environment-variable, get-environment-variables, current-second, current-jiffy, jiffies-per-second, features
├── Builtin.fs                    # builtinBindings registry + builtinContext
├── Repl.fs                       # rep function + REPL loop
├── Program.fs                    # Entry point
└── write-scheme.fsproj           # Project file (depends on FParsec only)

write-scheme.test/                # xUnit test project
└── *Test.fs                      # Per-feature test modules
```

Compilation order is declared in `write-scheme.fsproj`:

```
Type.fs → Print.fs → Read.fs → DatumLabel.fs → Context.fs → Eval.fs
→ Builtin/Helper.fs → Builtin/SpecialForm.fs → Builtin/Binding.fs
→ Builtin/Conditional.fs → Builtin/Lazy.fs → Builtin/DynamicBinding.fs
→ Builtin/Exception.fs → Builtin/Quasiquote.fs → Builtin/Macro.fs
→ Builtin/Record.fs → Builtin/Library.fs → Builtin/Core.fs → Builtin/Port.fs
→ Builtin/Number.fs → Builtin/Math.fs → Builtin/Bool.fs → Builtin/List.fs
→ Builtin/Symbol.fs → Builtin/Char.fs → Builtin/Str.fs → Builtin/Vector.fs
→ Builtin/ByteVector.fs → Builtin/Procedure.fs → Builtin/Environment.fs
→ Builtin/SystemInterface.fs → Builtin.fs → Repl.fs → Program.fs
```

---

## 2. Core Design: Continuation-Passing Style (CPS)

The entire evaluator is implemented in **Continuation-Passing Style (CPS)**. This is the single most important architectural decision.

### 2.1 What CPS Means Here

Every evaluation function receives a **continuation** (`cont`) — a function that represents "what to do next" — and **never returns directly**. Instead, it passes its result to the continuation:

```fsharp
// Instead of:
let eval expr = compute expr  // returns directly

// We have:
let eval context cont expr =  // receives a continuation
    compute expr |> Ok |> cont // passes result to continuation
```

### 2.2 Continuation Type

```fsharp
type SContinuation = Result<SExpression, SkipResult> -> Result<SExpression, SkipResult>
```

A continuation takes a `Result<SExpression, SkipResult>` and returns the same. This makes continuations composable — they can be chained.

### 2.3 Why CPS?

- **Stack safety**: By calling `cont` in tail position, arbitrarily deep recursion never overflows the stack.
- **First-class continuations (`call/cc`)**: The entire evaluator state is captured in the continuation chain, making `call/cc` straightforward to implement — just capture the current continuation as a value.
- **`dynamic-wind`**: Winders are naturally integrated into continuation invocations via the `doWind` helper.
- **Exception handling**: Error results (`Error e`) bypass the normal continuation chain, propagating directly to the nearest exception handler.

### 2.4 Rules

- **Always pass results to `cont`** — either `Ok value |> cont` or `Error e |> cont`.
- **Call `cont` in tail position** to guarantee stack safety.
- To abort evaluation early, return `Error e` directly (bypassing `cont`).

### 2.5 Procedure Signature

All built-in procedures and special forms share the same signature:

```fsharp
type SProcedureKind =
    Context -> Position option -> SContinuation -> SExpression list -> Result<SExpression, SkipResult>
```

Parameters:
1. `Context` — the execution context (environments, libraries, handlers)
2. `Position option` — source position for error reporting
3. `SContinuation` — the continuation to call with the result
4. `SExpression list` — the evaluated argument list (for procedures) or unevaluated forms (for special forms)

---

## 3. Core Types (`Type.fs`)

### 3.1 SExpression

Every value in the interpreter is represented as a pair of `SExpressionKind` and an optional `Position`:

```fsharp
type SExpression = SExpressionKind * Position option
```

The `Position option` carries source location information for error messages.

### 3.2 SExpressionKind Discriminated Union

| Case | Description | Carries |
|------|-------------|---------|
| `SUnspecified` | Unspecified return value | — |
| `SEmpty` | The empty list `()` | — |
| `SEof` | End-of-file object | — |
| `SBool of bool` | Boolean | `true` / `false` |
| `SRational of bigint * bigint` | Integer or rational | numerator, denominator |
| `SReal of float` | Floating-point number | IEEE 754 double |
| `SComplex of Complex` | Complex number | `System.Numerics.Complex` |
| `SString of SStringData` | Unicode string | `{ runes: Rune[]; isImmutable: bool }` |
| `SChar of Rune` | Character | `System.Text.Rune` (Unicode scalar) |
| `SSymbol of string` | Symbol | interned name |
| `SPair of SPairData` | Pair | mutable `car` / `cdr` |
| `SVector of SExpression array` | Vector | mutable array |
| `SByteVector of byte array` | Bytevector | mutable byte array |
| `SValues of SExpression list` | Multiple values | value list |
| `SRecord of int * string * SExpression ref array` | Record type | typeId, typeName, fields |
| `SError of ErrorType * SStringData * SExpression list` | Error object | error category (`GenericError` / `ReadError` / `FileError`), message, irritants |
| `SQuote of SExpression` | Quotation (reader syntax) | quoted expression |
| `SQuasiquote of SExpression` | Quasiquotation | template |
| `SUnquote of SExpression` | Unquote | expression |
| `SUnquoteSplicing of SExpression` | Unquote-splicing | expression |
| `SDatumLabel of int * SExpression` | Datum label (reader) | label ID, expression |
| `SDatumRef of int` | Back-reference (reader) | label ID |
| `SPromise of (bool * SExpression) ref` | Promise | evaluated flag, value/thunk |
| `SParameter of SExpression ref * SExpression option` | Parameter | current value, optional converter |
| `SPort of SPortData` | I/O port | `{ direction; isTextual; isOpen; inputReader; outputWriter; fileStream; filePath }` |
| `SSyntax of SProcedureKind` | Special form | procedure |
| `SProcedure of SProcedureKind` | Procedure | procedure |
| `SContinuation of SContinuation` | First-class continuation | continuation function |
| `SEnvironment of Environment` | First-class environment | used by `(scheme eval)` `environment` |

### 3.3 Context

The execution context carries all mutable state:

```fsharp
type Context =
    { environments: Environment list        // lexical environment stack
      libraries: Map<string, Library> ref   // registered libraries
      mutable nextExpansionId: int          // counter for macro expansion
      mutable nextRecordTypeId: int         // counter for record types
      mutable ports: PortSet                // current input/output/error ports
      winders: Winder list ref              // dynamic-wind stack
      nextWinderId: int ref                 // counter for winder IDs
      handlers: SExpression list ref      // exception handler stack
      commandLineArgs: string list }      // command-line arguments (`(command-line)` procedure)
```

`PortSet` groups three ports (`input`, `output`, `error`), each an `SPortData` record (`direction`, `isTextual`, mutable `isOpen`, optional `inputReader: TextReader`/`outputWriter: TextWriter`/`fileStream: Stream`/`filePath`). String ports back onto `System.IO.StringReader`/`StringWriter`, bytevector ports onto `MemoryStream`, and file ports onto `FileStream`.

Default ports in `Context.defaultPorts` are in-memory (`StringReader("")` / `StringWriter`), not the process console. The REPL reads user input via `Console.ReadLine` and only uses ports for Scheme I/O procedures. See [`docs/trade-off.md`](trade-off.md) for the record-vs-class-hierarchy trade-off.

### 3.4 Environment

```fsharp
type Environment = Map<string, SExpression ref> ref
```

Each environment is a mutable map from symbol names to mutable references (`ref` cells). The `ref` cell indirection enables `set!` to mutate bindings from parent environments (e.g., parameters whose values change via `parameterize`).

### 3.5 SkipResult (Error Types)

```fsharp
type SkipResult =
    | EvalError of string * Position option    // evaluation error (type errors, etc.)
    | ParseError of string * Position option   // parse error
    | SchemeRaise of SExpression * Position option  // R7RS raise
```

### 3.6 Key Constructors and Utilities

| Function | Purpose |
|----------|---------|
| `toSPair list` | Build a proper list from `SExpression list` |
| `toList expr` | Convert a proper list to `SExpression list` (with cycle detection) |
| `isProperList expr` | Check if expression is a proper list (Floyd's algorithm) |
| `loopListInfo tortoise hare accLength accList` | Floyd's cycle-finding algorithm; returns `Ok(list, length)` |
| `newInteger n` | Create `SRational(n, 1I)` (GCD-normalized) |
| `newSRational n d` | Create normalized rational (GCD-reduced) |
| `SZero` | Cached singleton `SRational(0I, 1I)` |
| `runesToString runes` | Convert `Rune[]` to `string` |
| `normalizeRational n d` | Reduce rational to lowest terms |

---

## 4. Pipeline Overview

The evaluation pipeline has four stages:

```
Source text
    │
    ▼
┌──────────────┐
│   Read.fs    │  FParsec parser: text → SExpression
│  (FParsec)   │  Handles reader macros: ', `, ,, ,@, #N=, #N#
└──────────────┘
    │
    ▼
┌──────────────────┐
│ DatumLabel.fs    │  Resolve #N= / #N# labels for shared/circular structure
└──────────────────┘
    │
    ▼
┌──────────┐
│ Eval.fs  │  CPS evaluator: SExpression → SExpression
│          │  Macro expansion, special forms, procedure application
└──────────┘
    │
    ▼
┌───────────┐
│ Print.fs  │  Serialize: SExpression → string
└───────────┘
    │
    ▼
  Output text
```

The `Repl.rep` function wires these together:

```fsharp
let rep context =
    Read.read false           // 1. Parse
    >> Result.bind DatumLabel.resolveLabels  // 2. Resolve datum labels
    >> Result.bind (Eval.eval context id)    // 3. Evaluate (id = identity continuation)
    >> Result.map Print.print                // 4. Serialize
    >> Result.defaultWith (...)              // 5. Handle errors
```

---

## 5. Reading Phase (`Read.fs`)

### 5.1 Parser Architecture

The parser uses **FParsec** (F# port of Parsec). It is organized as a set of mutually recursive parser functions operating on a `SState` (which is just `bool`, indicating case-folding mode for `include-ci`).

### 5.2 Parsing Flow

```
pExpression
├── pDatumLabel       #N=label
├── pDatumRef         #N# back-ref
├── pBoolean          #t, #f, #true, #false
├── pNumber           numeric literals
├── pCharacter        #\a, #\space, #\x3071, etc.
├── pString           "..." with escapes
├── pByteVector       #u8(...)
├── pVector           #(...)
├── pQuotation        'expr
├── pQuasiquotation   `expr
├── pUnquote          ,expr
├── pUnquoteSplicing  ,@expr
├── pIdentifier       symbols
└── pList             (expr ...)
    └── pDot           (a . b) dotted notation
```

### 5.3 Reader Expansions During Parsing

The parser converts reader syntax into internal forms:

| Input | Internal Representation |
|-------|------------------------|
| `'expr` | `(quote expr)` |
| `` `expr `` | `(quasiquote expr)` |
| `,expr` | `(unquote expr)` |
| `,@expr` | `(unquote-splicing expr)` |
| `#N=expr` | `SDatumLabel(N, expr)` |
| `#N#` | `SDatumRef(N)` |

### 5.4 Key Details

- **Unicode strings**: Parsed via `System.Text.Rune` for proper Unicode scalar value handling.
- **Numeric parsing**: Supports radix prefixes (`#b`, `#o`, `#x`, `#d`), exactness prefixes (`#e`, `#i`), rationals (`n/d`), scientific notation, and complex numbers (rectangular `a+bi`, polar `a@b`).
- **Case-folding**: Library `include-ci` passes case-folding mode through the parser state.
- **Line comments**: `;` to end-of-line.
- **Block comments**: `#| ... |#` (nestable).

---

## 6. Datum Label Resolution (`DatumLabel.fs`)

### 6.1 Purpose

Datum labels (`#N=expr` / `#N#`) enable representation of shared and circular structure in the textual format. The parser produces `SDatumLabel` and `SDatumRef` nodes; this phase resolves them into the actual graph structure.

### 6.2 Algorithm

1. **`collectDatum`**: Walk the parsed tree and collect all `SDatumLabel(N, expr)` bindings into a `Map<int, SExpression>`.
2. **`resolveDatumRef`**: Walk the tree a second time, replacing each `SDatumRef(N)` with the corresponding expression from the collected map.
   - Validates no duplicate label definitions.
   - Validates no forward references (labels must be defined before use).
   - Detects invalid circular references.
   - Propagates resolution through pairs, vectors, records, and error objects.

---

## 7. Evaluation Phase (`Eval.fs`)

### 7.1 Entry Points

| Function | Purpose |
|----------|---------|
| `eval context cont expr` | Evaluate a single expression |
| `apply context cont args proc` | Apply a procedure to arguments |
| `eachEval context cont acc exprs` | Evaluate a sequence, returning the last result |
| `evalBody context cont acc body` | Evaluate a lambda body (handles internal definitions) |
| `evalArgs context cont fn acc args` | Evaluate all arguments and apply the function |
| `evalPair context cont pair` | Dispatch a pair (operator expression) |

### 7.2 Evaluation Rules

**Self-evaluating forms**: Booleans, numbers, strings, characters, vectors, bytevectors, procedures, promises, parameters, error objects, records, `SUnspecified`, `SEmpty` — all evaluate to themselves.

**Symbols**: Looked up in the environment chain via `Context.lookupEnvironments`.

**Pairs** (the general case):

```
(pair) → eval car → is it a syntax? → apply syntax with cdr (unevaluated)
               ↓
         is it a procedure? → eval all args → apply procedure with evaluated args
```

### 7.3 Procedure Application

`apply` dispatches on the operator type:

```fsharp
match proc with
| SParameter(param, converter) → applyParameter  (read/write parameter value)
| SSyntax fn | SProcedure fn   → fn context pos cont args  (F# function call)
| SContinuation fn             → match args with
                                 | [ arg ] → Ok arg |> fn
                                 | _       → (SValues args, pos) |> Ok |> fn
                                 // multiple values passed as a single SValues
| _                            → error "not an operator"
```

### 7.4 Macro Expansion

When the evaluator encounters a symbol bound to a syntax definition (via `define-syntax`), it expands it at the point of evaluation. The macro engine in `Builtin/Macro.fs` handles pattern matching, ellipsis substitution, and hygiene.

### 7.5 Internal Definitions

`evalBody` handles the R7RS semantics for internal definitions:

1. **`expandBeginInBody`** pre-processes the body by flattening `begin` forms before definition collection (see [`docs/gotchas.md#7-expandbegininbody-pre-processes-begin-blocks-before-definition-collection`](gotchas.md#7-expandbegininbody-pre-processes-begin-blocks-before-definition-collection)).
2. **`collectInternalDefinitions`** classifies `define` / `define-values` forms at the beginning of the body (it no longer handles `begin` expansion itself).
3. **`validateBodyStructure`** ensures no definitions appear after non-definition expressions, and that internal definitions are followed by at least one expression.
4. **`prepareDefinitionContext`** creates a new environment with all defined variables initialized to `SUnspecified`.
5. Evaluates the definitions in that environment (allowing recursion via `eachEval`).
6. Evaluates the remaining expressions in order.

### 7.6 Stack Safety

- **`eval` and `apply`**: Both marked with `[<TailCall>]` and mutually recursive — all calls to continuations are in tail position.
- **`eachEval`**: Tail-recursive with an accumulator.
- **`evalArgs`**: Tail-recursive, builds accumulated list of evaluated arguments.
- **`loopListInfo`**: Floyd's cycle detection, using tail recursion for unbounded list traversal.
- **Helper recursion**: All `loop*` helpers in Builtin files ensure tail position calls.

---

## 8. Context System (`Context.fs`)

### 8.1 Environment Management

```fsharp
extendEnvironments context bindings       // push a new lexical scope
mergeEnvironments context captureContext // merge captured closure environments
defineEnvironmentVariable context sym value  // define/rebind in current scope
tryLookupEnvironments context sym         // search up the chain (returns SExpression ref option)
lookupEnvironments context pos symbol    // search up the chain (returns Result)
```

### 8.2 Library Management

```fsharp
registerLibrary context name libEnvironment exports   // register a define-library
lookupLibrary context pos name                       // resolve a library import
```

### 8.3 Dynamic Wind Management

```fsharp
pushWinder context winder    // enter a dynamic-wind guard
popWinder context id         // leave a dynamic-wind guard
```

Winders are stored as a list with unique IDs (`getNextWinderId`), allowing the evaluator to compute the difference between the current winder state and the saved state when invoking a continuation.

### 8.4 Exception Handler Stack

```fsharp
pushHandler context handler   // install a handler (for with-exception-handler)
popHandler context            // restore previous handler
```

### 8.5 Context Reset

`Context.reset` clears winders, restores `handlers` to `initialHandlers`, and resets `ports` to `defaultPorts`. It is called only from `Repl.rep` when the pipeline returns `Error` (`ParseError`, `EvalError`, or `SchemeRaise`) — not from `raise` / `error` themselves.

---

## 9. Built-in Procedures and Special Forms

### 9.1 Registration

All built-ins are registered in `Builtin.fs` in the `builtinBindings` list:

```fsharp
let builtinBindings: (string * SExpression ref) list =
    [ "quote", (SSyntax sQuote, None) |> ref
      "cons", (SProcedure sCons, None) |> ref
      ... ]
```

The list maps symbol names to mutable `SExpression ref` cells. Each cell holds an `(SExpressionKind * Position option)` tuple — the `Position` (set to `None` for built-ins) carries source location info, while mutability for `set!` comes from the `ref` cell itself.

### 9.2 Special Forms

Special forms are registered as `SSyntax` in `builtinBindings`. Implementations are split across Builtin modules (not only `SpecialForm.fs`):

| Special Form | Implementation | Key Behavior |
|---|---|---|
| `quote` | `sQuote` (SpecialForm.fs) | Returns datum as-is |
| `lambda` | `sLambda` (SpecialForm.fs) | Captures lexical environment, creates procedure |
| `if` | `sIf` (SpecialForm.fs) | Conditional with optional alternate |
| `set!` | `sSetBang` (SpecialForm.fs) | Mutates variable reference |
| `include` | `sInclude` (SpecialForm.fs) | File inclusion at expansion time |
| `include-ci` | `sIncludeCi` (SpecialForm.fs) | Case-insensitive file inclusion |
| `begin` | `sBegin` (SpecialForm.fs) | Sequential evaluation |
| `define` | `sDefine` (SpecialForm.fs) | Variable/procedure definition |
| `define-values` | `sDefineValues` (SpecialForm.fs) | Multi-value definition |
| `cond` | `sCond` (Conditional.fs) | Multi-way conditional with `=>` support |
| `case` | `sCase` (Conditional.fs) | Pattern matching with keys |
| `and` | `sAnd` (Conditional.fs) | Short-circuit AND |
| `or` | `sOr` (Conditional.fs) | Short-circuit OR |
| `when` | `sWhen` (Conditional.fs) | Conditional execution if truthy |
| `unless` | `sUnless` (Conditional.fs) | Conditional execution if false |
| `do` | `sDo` (Conditional.fs) | Iteration with variable updates |
| `case-lambda` | `sCaseLambda` (Conditional.fs) | Arity-based dispatch |
| `cond-expand` | `sCondExpand` (Conditional.fs) | Feature-based conditional expansion |
| `let`, `let*` | `sLet`, `sLetStar` (Binding.fs) | Parallel/sequential bindings |
| `letrec`, `letrec*` | `sLetRec`, `sLetRecStar` (Binding.fs) | Recursive bindings |
| `let-values`, `let*-values` | `sLetValues`, `sLetStarValues` (Binding.fs) | Multi-value bindings |
| `delay` | `sDelay` (Lazy.fs) | Lazy promise creation |
| `delay-force` | `sDelayForce` (Lazy.fs) | Lazy promise (thunk returns promise) |
| `parameterize` | `sParameterize` (DynamicBinding.fs) | Dynamic binding |
| `guard` | `sGuard` (Exception.fs) | Exception with condition matching |
| `quasiquote` | `sQuasiquote` (Quasiquote.fs) | Template with unquote/unquote-splicing; uses `QqKeyword` DU, `normalizeQqKeyword`, `consQq`, `joinQq` |
| `let-syntax`, `letrec-syntax` | `sLetSyntax`, `sLetRecSyntax` (Macro.fs) | Local macro bindings |
| `syntax-rules` | `sSyntaxRules` (Macro.fs) | Macro pattern definition |
| `syntax-error` | `sSyntaxError` (Macro.fs) | Expansion-time error signaling |
| `define-syntax` | `sDefineSyntax` (Macro.fs) | Macro definition |
| `define-record-type` | `sDefineRecordType` (Record.fs) | Record type definition (R7RS) |
| `define-library` | `sDefineLibrary` (Library.fs) | Library definition (R7RS) |
| `import` | `sImport` (Library.fs) | Library import with set operations |
| `environment` | `sEnvironment` (Environment.fs) | R7RS `(scheme eval)` environment constructor |

### 9.3 Procedure Categories

| Category | File | Examples |
|---|---|---|
| Equivalence | `Core.fs`, `Helper.fs` | `eqv?`, `equal?` (Core); `invalid`, `invalidParameter`, `mapResult`, `wrapUnary`, `eqv`, `doWind` (Helper) |
| Numeric | `Number.fs`, `Math.fs` | `SNumber` type and unified arithmetic in `Number.fs`; `+`, `-`, `*`, `/`, `sin`, `cos`, `gcd`, `quotient` in `Math.fs` |
| Boolean | `Bool.fs` | `not`, `boolean?`, `boolean=?` |
| List/Pair | `List.fs` | `cons`, `car`, `cdr`, `append`, `member`, `assoc` |
| Symbol | `Symbol.fs` | `symbol?`, `symbol->string` |
| Character | `Char.fs` | `char?`, `char=?`, `char-upcase` |
| String | `Str.fs` | `string?`, `string-length`, `string-append` |
| Vector | `Vector.fs` | `vector?`, `vector-ref`, `vector->list` |
| Bytevector | `ByteVector.fs` | `bytevector?`, `bytevector-u8-ref` |
| Higher-order / control | `Procedure.fs` | `apply`, `map`, `for-each`, `call/cc`, `values`, `call-with-values`, `dynamic-wind` |
| Exception | `Exception.fs` | `with-exception-handler`, `raise`, `error`, `error-object?`, `error-object-message`, `error-object-irritants`, `read-error?`, `file-error?` |
| Lazy evaluation | `Lazy.fs` | `force`, `promise?`, `make-promise` |
| Dynamic binding | `DynamicBinding.fs` | `make-parameter` (`parameterize` is syntax in the same file) |
| I/O ports | `Port.fs` | `read`, `write`, `display`, `open-input-string`, `open-input-file`, `current-input-port`, `eof-object` |
| Environment / eval | `Environment.fs` | `environment`, `eval`, `interaction-environment`, `null-environment`, `scheme-report-environment` |
| System interface | `SystemInterface.fs` | `load`, `file-exists?`, `delete-file`, `command-line`, `exit`, `emergency-exit`, `get-environment-variable`, `get-environment-variables`, `current-second`, `current-jiffy`, `jiffies-per-second`, `features` |

### 9.4 Implementation Pattern

Every built-in follows a consistent pattern:

```fsharp
let myProc context pos cont args =
    match args with
    | [ x ] ->
        // Process and pass to continuation
        doSomething x |> Ok |> cont
    | _ ->
        // Error case
        args |> invalidParameter pos "'%s' invalid my-proc parameter."
```

---

## 10. Macro System (`Builtin/Macro.fs`)

### 10.1 Architecture

The macro system implements R7RS `syntax-rules` with full hygiene via automatic renaming.

### 10.2 Key Components

| Component | Purpose |
|---|---|
| `SBinding` (DU) | `SingleB` / `EllipsisB` — represents pattern variable bindings |
| `collectPatternVariables` | Walks a pattern and collects all pattern variables |
| `loopPatternVars` | Recursive pattern variable collection |
| `freeIdentifierEquals` | Hygiene check — compares identifiers by their binding references |
| `buildEllipsisBindings` | Groups repeated pattern matches for `...` |
| `matchOne` | Core matcher (pattern value → `SBinding option`, uses `matchAtom` for atomic types) |
| `decodePair` | Converts pair tree into prefix + optional tail |
| `matchPatternListWithEllipsisParts` | Recursive list matching with `...` (CPS) |
| Template instantiation | Substitutes pattern variables in the template |

### 10.3 Hygiene

Hygiene is achieved by tracking the lexical environment where the macro was defined. When a pattern variable matches an identifier, the macro compares the identifier's original binding (from the macro's definition environment) against the template context, renaming as needed to prevent unintended capture.

### 10.4 Pattern Matching

The pattern matcher (`matchOne`) supports matching against all atomic and compound types. `matchAtom` handles the 6 atomic cases (`SEmpty`, `SBool`, `SRational`, `SReal`, `SString`, `SChar`), while pairs, vectors, and symbols (`_`, ellipsis, literals) are matched directly by `matchOne`.

Supported features:

- Literal symbols (must match exactly, not as pattern variables)
- `_` wildcard (matches anything)
- Ellipsis (`...`) in various positions (nested ellipsis, etc.)
- Custom ellipsis symbols
- Improper list tails

---

## 11. Print Phase (`Print.fs`)

### 11.1 CPS Printer

The printer is implemented in CPS to handle cyclic and shared data structures safely:

```fsharp
printCPS labelMap emitted visited next expr
```

- `labelMap` (`IDictionary<obj, int>`) assigns label IDs to shared objects, enabling `#N=` / `#N#` notation for shared structure (used by `printShared`).
- `emitted` (`Set<int>`) tracks which labels have already been emitted, so the first encounter prints `#N=` and later references print `#N#`.
- `visited` tracks already-printed objects for cycle detection (identity comparison via `obj.ReferenceEquals`).
- `next` is the continuation to call with the formatted string and the updated `emitted` set.

Two entry points:

- `print` — uses an empty `labelMap`, so cycles are printed as `...` (no shared-structure labels).
- `printShared` — pre-computes a `labelMap` via `buildSharedLabelMap`, so shared and cyclic structure is printed using `#N=` / `#N#` reader notation.

### 11.2 Cycle Detection

The `visited` list tracks previously printed objects. When a cycle is detected, the printer prints `...` (or `#N#` if a label was emitted) to avoid infinite loops:

```fsharp
let isVisited visited x =
    visited |> List.exists (fun v -> obj.ReferenceEquals(v, x))
```

Cycle detection covers pairs, vectors, values, and error irritants (see [`docs/gotchas.md#11-printer-cycle-detection-tracks-pairs-vectors-values-and-error-irritants`](gotchas.md#11-printer-cycle-detection-tracks-pairs-vectors-values-and-error-irritants)).

### 11.3 Formatting Rules

| Type | Format |
|------|--------|
| Boolean | `#t`, `#f` |
| Integer | `42`, `-17` |
| Rational | `1/2`, `-3/4` |
| Real | `3.14`, `+inf.0`, `-inf.0`, `+nan.0` |
| Complex | `1+2i`, `0-3i` |
| String | `"hello"` with escape sequences |
| Character | `#\a`, `#\space`, `#\x3071` |
| Symbol | `foo`, `\|two words\|` (pipe-escaped if needed) |
| Pair/List | `(1 2 3)`, `(1 . 2)` |
| Vector | `#(1 2 3)` |
| Bytevector | `#u8(0 10 255)` |
| Unspecified | `#<unspecified>` |
| Record | `#<record-type-name>` |
| Port | `#<input textual port open>`, `#<output binary port closed>` |
| Promise | `#<promise>` |
| Parameter | `#<parameter>` |
| Procedure | `#<procedure>` |
| Syntax | `#<syntax>` |
| Continuation | `#<continuation>` |
| Environment | `#<environment>` |
| Error | `#<error "message" irritant ...>` |
| EOF | `#!eof` |

---

## 12. REPL Loop (`Repl.fs` / `Program.fs`)

### 12.1 Lifecycle

```fsharp
// Program.fs — entry point
let main argv =
    "Welcome" |> repl (newContext (argv |> Array.toList))
    0

// Repl.fs — recursive REPL loop
let rec repl context output =
    printf "%s\n> " output
    let line = System.Console.ReadLine()
    if isNull line then ()
    else line |> rep context |> repl context   // tail call

// Repl.fs — single expression pipeline
let rep context =
    Read.read false                                      // parse
    >> Result.bind DatumLabel.resolveLabels              // resolve datum labels
    >> Result.bind (Eval.eval context id)                // evaluate (id continuation)
    >> Result.map Print.print                            // serialize
    >> Result.defaultWith (fun e ->
        context |> Context.reset
        match e with
        | ParseError(msg, pos) -> ...
        | EvalError(msg, pos) -> ...
        | SchemeRaise(expr, pos) -> ...)
```

### 12.2 Context Freshness

`newContext argv` creates a fresh context for each REPL session:

```fsharp
let newContext argv =
    let context = Builtin.builtinContext
    { context with
        commandLineArgs = argv
        environments = (Map.empty |> ref) :: context.environments
        winders = ref []
        handlers = ref Context.initialHandlers
        nextWinderId = ref 0 }
```

This starts with a fresh user environment on top of the built-in bindings, and forwards `argv` to the `(command-line)` procedure. Note that `libraries` is **not** copied — it is inherited from `builtinContext` (all standard `(scheme ...)` libraries are registered there) and shared via the same `ref` cell.

### 12.3 Error Recovery

When an error occurs (`ParseError`, `EvalError`, or `SchemeRaise`), `Context.reset` is called to clear winders, restore the default exception handlers, and reset `ports` to `defaultPorts` (in-memory string ports), before formatting the error message (with source position appended) and continuing the REPL.

---

## 13. Data Flow Summary

```
User input (string)
    │
    ▼
┌──────────────────────────────────────────────────────┐
│  Read.fs                                             │
│  FParsec parser                                      │
│  Output: SExpression with SDatumLabel / SDatumRef    │
│  Reader macros expanded: ' → (quote ...)             │
└──────────────────────────────────────────────────────┘
    │
    ▼
┌──────────────────────────────────────────────────────┐
│  DatumLabel.fs                                       │
│  Resolve #N= / #N# labels                            │
│  Output: SExpression with all labels resolved        │
└──────────────────────────────────────────────────────┘
    │
    ▼
┌──────────────────────────────────────────────────────┐
│  Eval.fs                                             │
│  CPS evaluator                                       │
│  ├─ Self-evaluating? → return as-is                  │
│  ├─ Symbol? → lookup in environment chain            │
│  ├─ Pair? →                                          │
│  │  ├─ car is syntax? → apply syntax (unevaled args) │
│  │  ├─ car is procedure? → eval args → apply         │
│  │  └─ car is macro? → expand → re-evaluate          │
│  └─ ...                                              │
│  Output: SExpression (evaluated result)              │
└──────────────────────────────────────────────────────┘
    │
    ▼
┌──────────────────────────────────────────────────────┐
│  Print.fs                                            │
│  CPS printer with cycle detection                    │
│  Output: string                                      │
└──────────────────────────────────────────────────────┘
    │
    ▼
Displayed to user
```

---

## 14. Cyclomatic Complexity Guidelines

Cyclomatic complexity is measured by Coverlet. The current project-wide thresholds:
- **Error threshold**: complexity > 15 — must be refactored.
- **Warning threshold**: complexity > 10 — should be addressed where practical.
- **Target**: ≤ 10 for most functions.

The `dotnet test` output includes coverage data — check after every change.

Functions with large `match` expressions on `SExpressionKind` historically exceeded the threshold and are refactored incrementally by extracting helpers, using or-patterns, or introducing intermediate types. Special forms are spread across Builtin modules; prefer adding new ones as separate functions in the appropriate module rather than extending large multi-purpose matchers.
