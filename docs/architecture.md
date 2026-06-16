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
├── Read.fs                       # FParsec-based parser (418 lines)
├── DatumLabel.fs                 # #N= / #N# datum label resolution (128 lines)
├── Print.fs                      # S-expression serializer (164 lines)
├── Context.fs                    # Execution context: environments, libraries, winders, handlers (111 lines)
├── Eval.fs                       # CPS evaluator: eval / apply / eachEval (193 lines)
├── Builtin/
│   ├── Helper.fs                 # Shared helpers: invalid, mapResult, doWind, getRange, eqv (174 lines)
│   ├── SpecialForm.fs            # All special forms: lambda, if, cond, let, define, ... (1437 lines)
│   ├── Macro.fs                  # syntax-rules hygienic macro engine (562 lines)
│   ├── Procedure.fs              # apply, map, for-each, call/cc, dynamic-wind (221 lines)
│   ├── Core.fs                   # eqv?, equal? (105 lines)
│   ├── Number.fs                 # SNumber type (NRational|NReal|NComplex) and unified arithmetic (159 lines)
│   ├── Math.fs                   # Numeric tower operations (737 lines)
│   ├── List.fs                   # Pair/list operations (290 lines)
│   ├── Str.fs                    # String operations (212 lines)
│   ├── Char.fs                   # Character operations (97 lines)
│   ├── Vector.fs                 # Vector operations (122 lines)
│   ├── ByteVector.fs             # Bytevector operations (102 lines)
│   ├── Bool.fs                   # Boolean operations (30 lines)
│   ├── Symbol.fs                 # Symbol operations (36 lines)
│   ├── Promise.fs                # delay / force (39 lines)
│   ├── SavedParameter.fs         # make-parameter / parameterize (90 lines)
│   └── Exception.fs              # with-exception-handler, raise, guard (74 lines)
├── Builtin.fs                    # builtinBindings registry + builtinContext (268 lines)
├── Repl.fs                       # rep function + REPL loop
├── Program.fs                    # Entry point
└── write-scheme.fsproj           # Project file (depends on FParsec only)

write-scheme.test/                # xUnit test project
└── *Test.fs                      # Per-feature test modules
```

Compilation order is declared in `write-scheme.fsproj`:

```
Type.fs → Print.fs → Read.fs → DatumLabel.fs → Context.fs → Eval.fs
→ Builtin/Helper.fs → Builtin/Promise.fs → Builtin/SavedParameter.fs
→ Builtin/SpecialForm.fs → Builtin/Procedure.fs → Builtin/Macro.fs
→ Builtin/Core.fs → Builtin/Number.fs → Builtin/Math.fs
→ Builtin/Bool.fs → Builtin/List.fs → Builtin/Symbol.fs
→ Builtin/Char.fs → Builtin/Str.fs → Builtin/Vector.fs
→ Builtin/ByteVector.fs → Builtin/Exception.fs → Builtin.fs → Repl.fs → Program.fs
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
| `SError of SStringData * SExpression list` | Error object | message, irritants |
| `SQuote of SExpression` | Quotation (reader syntax) | quoted expression |
| `SQuasiquote of SExpression` | Quasiquotation | template |
| `SUnquote of SExpression` | Unquote | expression |
| `SUnquoteSplicing of SExpression` | Unquote-splicing | expression |
| `SDatumLabel of int * SExpression` | Datum label (reader) | label ID, expression |
| `SDatumRef of int` | Back-reference (reader) | label ID |
| `SPromise of (bool * SExpression) ref` | Promise | evaluated flag, value/thunk |
| `SParameter of SExpression ref * SExpression option` | Parameter | current value, optional converter |
| `SSyntax of SProcedureKind` | Special form | procedure |
| `SProcedure of SProcedureKind` | Procedure | procedure |
| `SContinuation of SContinuation` | First-class continuation | continuation function |

### 3.3 Context

The execution context carries all mutable state:

```fsharp
type Context =
    { environments: Environment list        // lexical environment stack
      libraries: Map<string, Library> ref   // registered libraries
      mutable nextExpansionId: int          // counter for macro expansion
      mutable nextRecordTypeId: int         // counter for record types
      winders: Winder list ref              // dynamic-wind stack
      nextWinderId: int ref                 // counter for winder IDs
      handlers: SExpression list ref }      // exception handler stack
```

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
| SContinuation fn             → fn (Ok arg)  (invoke captured continuation)
| _                            → error "not an operator"
```

### 7.4 Macro Expansion

When the evaluator encounters a symbol bound to a syntax definition (via `define-syntax`), it expands it at the point of evaluation. The macro engine in `Builtin/Macro.fs` handles pattern matching, ellipsis substitution, and hygiene.

### 7.5 Internal Definitions

`evalBody` handles the R7RS semantics for internal definitions:

1. Collects all `define` / `define-values` forms at the beginning of the body.
2. Creates a new environment with all defined variables initialized to unspecified.
3. Evaluates the definitions in that environment (allowing recursion).
4. Evaluates the remaining expressions in order.

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
extendEnvironments context bindings    // push a new lexical scope
mergeEnvironments context capture      // merge captured closure env
defineEnvironmentVariable context sym  // define in current scope
lookupEnvironments context sym         // search up the chain
```

### 8.2 Library Management

```fsharp
registerLibrary context name env exports   // register a define-library
lookupLibrary context pos name             // resolve a library import
```

### 8.3 Dynamic Wind Management

```fsharp
pushWinder context winder    // enter a dynamic-wind guard
popWinder context id         // leave a dynamic-wind guard
```

Winders are stored as a list with unique IDs, allowing the evaluator to compute the difference between the current winder state and the saved state when invoking a continuation.

### 8.4 Exception Handler Stack

```fsharp
pushHandler context handler   // install a handler (for with-exception-handler)
popHandler context            // restore previous handler
```

### 8.5 Context Reset

`Context.reset` clears winders and restores the default exception handlers. This is called after an error in the REPL to ensure a clean state for the next input.

---

## 9. Built-in Procedures and Special Forms

### 9.1 Registration

All built-ins are registered in `Builtin.fs` in the `builtinBindings` list:

```fsharp
let builtinBindings =
    [ "procedure-name", (SProcedure implementationFn, None) |> ref
      "syntax-name",    (SSyntax implementationFn, None) |> ref
      ... ]
```

The second element of the tuple is an optional `SExpression option` that can hold a mutable reference for `set!` — `None` means the binding is immutable.

### 9.2 Special Forms (`Builtin/SpecialForm.fs`)

This is the largest file (1416 lines) containing the implementation of all special forms:

| Special Form | Implementation | Key Behavior |
|---|---|---|
| `quote` | `sQuote` | Returns datum as-is |
| `lambda` | `sLambda` / `closure` | Captures lexical environment, creates procedure |
| `if` | `sIf` | Conditional with optional alternate |
| `set!` | `sSetBang` | Mutates variable reference |
| `cond` | `sCond` | Multi-way conditional with `=>` support |
| `case` | `sCase` | Pattern matching with keys (`normalizeCaseClause`/`isElseClause` helpers) |
| `and`, `or` | `sAnd`, `sOr` | Short-circuit evaluation |
| `when`, `unless` | `sWhen`, `sUnless` | Conditional execution |
| `begin` | `sBegin` | Sequential evaluation |
| `let`, `let*` | `sLet`, `sLetStar` | Parallel/sequential bindings |
| `letrec`, `letrec*` | `sLetRec`, `sLetRecStar` | Recursive bindings |
| `let-values`, `let*-values` | `sLetValues`, `sLetStarValues` | Multi-value bindings |
| `do` | `sDo` | Iteration with variable updates |
| `delay` | `sDelay` | Lazy promise creation |
| `delay-force` | `sDelayForce` | Lazy promise (thunk returns promise) |
| `parameterize` | `sParameterize` | Dynamic binding (delegates to SavedParameter.fs) |
| `guard` | `sGuard` | Exception with condition matching |
| `quasiquote` | `sQuasiquote` | Template with unquote/unquote-splicing; uses `QqKeyword` DU, `normalizeQqKeyword`, `consQq`, `joinQq` (supports nested quasiquotation) |
| `case-lambda` | `sCaseLambda` | Arity-based dispatch |
| `let-syntax`, `letrec-syntax` | `sLetSyntax`, `sLetRecSyntax` | Local macro bindings |
| `syntax-rules` | `sSyntaxRules` | Macro pattern definition |
| `syntax-error` | `sSyntaxError` | Expansion-time error signaling |
| `import` | `sImport` | Library import with set operations |
| `define` | `sDefine` | Variable/procedure definition |
| `define-values` | `sDefineValues` | Multi-value definition |
| `define-syntax` | `sDefineSyntax` | Macro definition |
| `define-record-type` | `sDefineRecordType` | Record type definition (R7RS) |
| `define-library` | `sDefineLibrary` | Library definition (R7RS) |
| `include`, `include-ci` | `sInclude`, `sIncludeCi` | File inclusion |

### 9.3 Procedure Categories

| Category | File | Examples |
|---|---|---|
| Equivalence | `Core.fs`, `Helper.fs` | `equal?` (Core), `eqv?` (Helper) |
| Numeric | `Number.fs`, `Math.fs` | `SNumber` type and unified arithmetic in `Number.fs`; `+`, `-`, `*`, `/`, `sin`, `cos`, `gcd`, `quotient` in `Math.fs` |
| Boolean | `Bool.fs` | `not`, `boolean?`, `boolean=?` |
| List/Pair | `List.fs` | `cons`, `car`, `cdr`, `map`, `append`, `assoc` |
| Symbol | `Symbol.fs` | `symbol?`, `symbol->string` |
| Character | `Char.fs` | `char?`, `char=?`, `char-upcase` |
| String | `Str.fs` | `string?`, `string-length`, `string-append` |
| Vector | `Vector.fs` | `vector?`, `vector-ref`, `vector->list` |
| Bytevector | `ByteVector.fs` | `bytevector?`, `bytevector-u8-ref` |
| Higher-order | `Procedure.fs` | `apply`, `map`, `for-each`, `call/cc`, `dynamic-wind` |
| Exception | `Exception.fs` | `with-exception-handler`, `raise`, `error` |
| Promise | `Promise.fs` | `force`, `promise?`, `make-promise` |
| Parameter | `SavedParameter.fs` | `make-parameter` |

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

The printer itself is implemented in CPS to handle cyclic data structures safely:

```fsharp
printCPS visited next expr
```

- `visited` tracks already-printed objects (identity comparison via `obj.ReferenceEquality`).
- `next` is the continuation to call with the formatted string.

### 11.2 Cycle Detection

The printer maintains a `visited` list of previously printed pair/vector objects. When it encounters a cycle, it prints `...` to avoid infinite loops.

```fsharp
let isVisited visited x =
    visited |> List.exists (fun v -> obj.ReferenceEquals(v, x))
```

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
| Record | `#[record-type-name ...]` |
| Procedure | `#<procedure>` |
| Continuation | `#<continuation>` |

---

## 12. REPL Loop (`Repl.fs` / `Program.fs`)

### 12.1 Lifecycle

```fsharp
// Program.fs — entry point
main = "Welcome" |> repl (newContext ())

// Repl.fs — REPL loop
repl context output =
    printf "%s\n> " output    // print previous result
    readLine ()               // read input
    |> rep context            // evaluate
    |> repl context           // recurse

// Repl.fs — single expression pipeline
rep context =
    Read.read               → parse
    >> DatumLabel.resolve   → resolve datum labels
    >> Eval.eval context id → evaluate
    >> Print.print          → serialize
    >> error formatting     → handle errors
```

### 12.2 Context Freshness

`newContext()` creates a fresh context for each REPL session:

```fsharp
let newContext () =
    let context = Builtin.builtinContext
    { context with
        environments = (Map.empty |> ref) :: context.environments
        winders = ref []
        handlers = ref Context.initialHandlers
        nextWinderId = ref 0 }
```

This starts with a fresh user environment on top of the built-in bindings.

### 12.3 Error Recovery

When an error occurs (`ParseError`, `EvalError`, or `SchemeRaise`), `Context.reset` is called to clean up winders and handlers before printing the error message and continuing the REPL.

---

## 13. Data Flow Summary

```
User input (string)
    │
    ▼
┌──────────────────────────────────────────────────────┐
│  Read.fs                                              │
│  FParsec parser                                       │
│  Output: SExpression with SDatumLabel / SDatumRef     │
│  Reader macros expanded: ' → (quote ...)              │
└──────────────────────────────────────────────────────┘
    │
    ▼
┌──────────────────────────────────────────────────────┐
│  DatumLabel.fs                                        │
│  Resolve #N= / #N# labels                             │
│  Output: SExpression with all labels resolved          │
└──────────────────────────────────────────────────────┘
    │
    ▼
┌──────────────────────────────────────────────────────┐
│  Eval.fs                                               │
│  CPS evaluator                                         │
│  ├─ Self-evaluating? → return as-is                   │
│  ├─ Symbol? → lookup in environment chain             │
│  ├─ Pair? →                                            │
│  │   ├─ car is syntax? → apply syntax (unevaled args) │
│  │   ├─ car is procedure? → eval args → apply         │
│  │   └─ car is macro? → expand → re-evaluate          │
│  └─ ...                                                │
│  Output: SExpression (evaluated result)                │
└──────────────────────────────────────────────────────┘
    │
    ▼
┌──────────────────────────────────────────────────────┐
│  Print.fs                                              │
│  CPS printer with cycle detection                      │
│  Output: string                                        │
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

Functions exceeding 15 complexity are typically those with large `match` expressions on `SExpressionKind` (e.g., `eqv`, `loopEqual`). These are refactored incrementally by extracting helpers, using or-patterns, or introducing intermediate types.

`SpecialForm.fs` (1400+ lines, 30+ special forms) is the most complexity-sensitive area. New special forms should be added as separate functions rather than extending existing ones.
