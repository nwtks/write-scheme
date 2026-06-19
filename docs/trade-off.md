# Design Trade-offs

This document records design decisions, trade-offs, and their rationale.

---

## Table of Contents

- [1. CPS vs Direct-Style Evaluation](#1-cps-vs-direct-style-evaluation)
- [2. SExpression as Tuple vs Record](#2-sexpression-as-tuple-vs-record)
- [3. Mutable Pairs vs Immutable Pairs](#3-mutable-pairs-vs-immutable-pairs)
- [4. Bigint-Based Rationals vs Native Integer Types](#4-bigint-based-rationals-vs-native-integer-types)
- [5. Rune-Based Strings vs .NET char/string](#5-rune-based-strings-vs-net-charstring)
- [6. FParsec Parser Combinators vs Hand-Written Parser](#6-fparsec-parser-combinators-vs-hand-written-parser)
- [7. Unified SExpressionKind DU vs Separate Types](#7-unified-sexpressionkind-du-vs-separate-types)
- [8. Single Executable vs Library + Executable](#8-single-executable-vs-library--executable)
- [9. Mutable Environments vs Persistent Environments](#9-mutable-environments-vs-persistent-environments)
- [10. Floyd's Cycle Detection vs Visited-Set](#10-floyds-cycle-detection-vs-visited-set)
- [11. Result<SExpression, SkipResult> vs Exceptions](#11-resultsexpression-skipresult-vs-exceptions)
- [12. Separate Datum Label Resolution Pass vs Inline During Parsing](#12-separate-datum-label-resolution-pass-vs-inline-during-parsing)
- [13. Centralized Builtin Binding List vs Distributed Registration](#13-centralized-builtin-binding-list-vs-distributed-registration)
- [14. AutoOpen Modules in Builtin/ vs Explicit Imports](#14-autoopen-modules-in-builtin-vs-explicit-imports)
- [15. CPS Printer with Visited-Set vs Simple Recursive Printer](#15-cps-printer-with-visited-set-vs-simple-recursive-printer)
- [16. Reader Macro Expansion During Parsing vs Post-Parse Transformation](#16-reader-macro-expansion-during-parsing-vs-post-parse-transformation)
- [17. SNumber Unified Type vs Separate SExpressionKind Cases for Numeric Operations](#17-snumber-unified-type-vs-separate-sexpressionkind-cases-for-numeric-operations)
- [18. QqKeyword DU vs Raw Symbol Matching in Quasiquote Expansion](#18-qqkeyword-du-vs-raw-symbol-matching-in-quasiquote-expansion)
- [19. CPS Incompatibility with Ref Cells for Accumulation](#19-cps-incompatibility-with-ref-cells-for-accumulation)
- [20. Option List Accumulator vs Plain List in `loopListInfo`](#20-option-list-accumulator-vs-plain-list-in-looplistinfo)

---

## 1. CPS vs Direct-Style Evaluation

### Context

The entire evaluator — `eval`, `apply`, `evalArgs`, `Eval.evalBody` — is written in Continuation-Passing Style. Every function receives a continuation (`cont`) and passes its result to it rather than returning.

### Trade-off

| Aspect | CPS | Direct Style |
|--------|-----|-------------|
| Stack safety | ✅ Guaranteed via tail calls | ❌ Can overflow on deep recursion |
| First-class continuations | ✅ `call/cc` captures the continuation chain naturally | ❌ Requires explicit stack capture/copying |
| `dynamic-wind` | ✅ Integrated naturally into continuation invocation | ❌ Requires special handling |
| Exception handling | ✅ Error results bypass continuation chain | ✅ Exceptions work naturally |
| Readability | ❌ Continuation threading obscures control flow | ✅ Familiar, straightforward |
| Debugging | ❌ Stack traces are continuation chains | ✅ Stack traces reflect call structure |
| Boilerplate | ❌ Every function must take and call `cont` | ✅ Functions return directly |

### Rationale

CPS was chosen because the Scheme language requires first-class continuations (`call/cc`) and `dynamic-wind`. In a direct-style evaluator, implementing `call/cc` would require capturing and copying the entire call stack, which is difficult in .NET without platform-specific code. CPS makes these features **free** — `call/cc` is simply wrapping the current continuation in a value, and `dynamic-wind` is a matter of running winders when the continuation chain is traversed.

### Mitigations

- Keep individual continuation functions small.
- Use named continuations (e.g., `cont`) consistently.
- Document the CPS pattern in AGENTS.md for new contributors.

---

## 2. SExpression as Tuple vs Record

### Context

SExpression is defined as:

```fsharp
type SExpression = SExpressionKind * Position option
```

rather than:

```fsharp
type SExpression = { kind: SExpressionKind; pos: Position option }
```

### Trade-off

| Aspect | Tuple | Record |
|--------|-------|--------|
| Pattern matching | ✅ Destructuring via `case, pos` is concise | ❌ Requires `{ kind = ...; pos = _ }` |
| Field access | ❌ `fst`/`snd` or destructuring needed | ✅ Named access `expr.pos` |
| Extensibility | ❌ Adding fields breaks all match sites | ✅ Adding fields is backward-compatible |
| Conciseness | ✅ `(value, None)` is very compact | ❌ `({ kind = value; pos = None })` is verbose |

### Rationale

Tuple was chosen for conciseness in pattern matching, which is the dominant operation on SExpressions. Every evaluation function, printer, and built-in matches on `SExpressionKind` and propagates position — the tuple syntax `(x, pos)` keeps this ergonomic. Adding extra fields (unlikely) would be a very rare event.

---

## 3. Mutable Pairs vs Immutable Pairs

### Context

`SPairData` uses mutable fields:

```fsharp
type SPairData =
    { mutable car: SExpression
      mutable cdr: SExpression }
```

### Trade-off

| Aspect | Mutable | Immutable |
|--------|---------|-----------|
| Scheme compliance | ✅ Required for `set-car!`/`set-cdr!` | ❌ Violates R7RS |
| Cycle detection needed | ✅ Printer uses visited-set, `toList` uses Floyd's | ❌ No cycles possible with immutable structure |
| `equal?` complexity | ✅ Structural equality must handle cycles | ❌ Straightforward recursive comparison |
| Thread safety | ❌ Mutation is not thread-safe | ✅ Immutable by default |

### Rationale

Scheme `set-car!` and `set-cdr!` require mutable pairs — this is non-negotiable for R7RS compliance. The trade-off is that cycle detection becomes necessary in both the printer and list operations, adding complexity. The interpreter is single-threaded (Scheme's `set!`/`set-car!` semantics assume sequential evaluation), so thread safety is not a concern.

---

## 4. Bigint-Based Rationals vs Native Integer Types

### Context

Integers and rationals are stored as `SRational(bigint, bigint)` using `System.Numerics.BigInteger`, rather than using `int`, `long`, or `decimal`.

### Trade-off

| Aspect | `bigint` Rationals | Native Integer Types |
|--------|-------------------|---------------------|
| Range | ✅ Arbitrarily large integers | ❌ Limited to 32/64 bits |
| Exactness | ✅ Exact arithmetic per R7RS | ❌ Overflow or precision loss |
| Performance | ❌ Heap-allocated, slower arithmetic | ✅ Stack-allocated, CPU-native |
| Memory | ❌ Larger memory footprint | ✅ Compact representation |
| Rational support | ✅ Natural: `SRational(num, den)` | ❌ Requires separate rational type |

### Rationale

R7RS requires exact integer arithmetic over arbitrarily large values. `bigint` is the only reasonable choice in .NET. The rational representation is a natural extension — integers are simply rationals with denominator `1I`. The performance cost is acceptable for an interpreter.

---

## 5. Rune-Based Strings vs .NET char/string

### Context

Strings are stored as `System.Text.Rune` arrays, not as .NET `string` or `char[]`.

```fsharp
type SStringData =
    { runes: System.Text.Rune array
      isImmutable: bool }
```

### Trade-off

| Aspect | `Rune[]` | `string` / `char[]` |
|--------|----------|---------------------|
| Unicode compliance | ✅ Full scalar value awareness | ❌ `char` is UTF-16 code unit, not codepoint |
| R7RS compliance | ✅ `string-ref` is codepoint-based | ❌ Wrong semantics for supplementary chars |
| Performance | ❌ Higher memory use, slower index | ✅ Compact, fast index |
| Interop | ❌ Must convert to/from `string` for I/O | ✅ Native .NET representation |

### Rationale

R7RS specifies that `string-ref` operates on **characters** (as defined by the Scheme report), which are Unicode scalar values. Using .NET `char` (a UTF-16 code unit) would give wrong results for supplementary characters (emoji, CJK extension B, etc.). `System.Text.Rune` is .NET's standard representation for Unicode scalar values. The conversion cost on I/O boundaries is acceptable.

---

## 6. FParsec Parser Combinators vs Hand-Written Parser

### Context

The parser uses the **FParsec** library (F# port of the Parsec parser combinator library).

### Trade-off

| Aspect | FParsec | Hand-Written |
|--------|---------|--------------|
| Development speed | ✅ Combinators compose naturally | ❌ Must implement all parsing logic |
| Error messages | ✅ Automatic position tracking | ❌ Must be manually implemented |
| Correctness | ✅ Combinator composition reduces bugs | ❌ Easy to miss edge cases |
| Dependency | ❌ External NuGet package | ✅ No dependencies |
| Build time | ❌ Adds compilation overhead | ✅ Minimal |
| Control | ❌ Limited by library's capabilities | ✅ Full control over performance/behavior |

### Rationale

FParsec was chosen because Scheme has a complex reader syntax (radix prefixes, character names, string escapes, datum labels, nested block comments, etc.). A parser combinator library provides high-quality error messages (line/column positions) and composable parsing at a fraction of the development cost of a hand-written parser. The dependency on FParsec is minimal and stable.

---

## 7. Unified SExpressionKind DU vs Separate Types

### Context

All value types are represented in a single discriminated union:

```fsharp
type SExpressionKind =
    | SBool | SRational | SReal | SComplex | SString | SChar
    | SSymbol | SPair | SVector | SByteVector
    | SValues | SRecord | SError
    | SQuote | SQuasiquote | SUnquote | SUnquoteSplicing
    | SDatumLabel | SDatumRef
    | SPromise | SParameter
    | SSyntax | SProcedure | SContinuation
    | SUnspecified | SEmpty
```

rather than having separate types for each category.

### Trade-off

| Aspect | Single DU | Separate Types |
|--------|-----------|----------------|
| Exhaustiveness | ✅ Single match covers all cases | ❌ Must handle each type separately |
| Simplicity | ✅ One type to pass everywhere | ❌ Need generic wrappers or interfaces |
| Function signatures | ✅ Simple: `SExpression` | ❌ Complex: type parameters or unions |
| File size | ❌ One very large type definition | ✅ Each type defined near its operations |
| Match verbosity | ❌ Unrelated cases clutter matches | ✅ Only relevant cases matched |

### Rationale

A single DU is simpler for an interpreter because:

- All values flow through the same `eval`/`apply` functions.
- Procedures accept and return a single value type.
- Exhaustiveness checking catches incomplete matches (useful when adding new types).
- Printer and parser can handle all types uniformly.

The cognitive load of one large DU is mitigated by F#'s pattern matching — each function only matches the cases it cares about.

---

## 8. Single Executable vs Library + Executable

### Context

The project is a single executable (`OutputType: Exe`) rather than a library + executable split.

### Trade-off

| Aspect | Single Exe | Library + Exe |
|--------|-----------|---------------|
| Testability | ❌ Tests must reference the exe | ✅ Library is naturally testable |
| Reuse | ❌ Cannot easily embed in other apps | ✅ Library can be consumed as a package |
| Simplicity | ✅ Single project, single output | ❌ Two projects, coordination overhead |

### Rationale

The project started as a single executable for simplicity. The test project (`write-scheme.test`) references the main project directly, which works fine with `ReferenceOutputAssembly = true`. If the interpreter ever needs to be embedded in other applications (e.g., a GUI, a web service), extracting a library project would be straightforward — the core modules (`Eval`, `Read`, `Print`, `Builtin`) already have no dependency on the console REPL.

---

## 9. Mutable Environments vs Persistent Environments

### Context

Environments use mutable ref cells inside a mutable map:

```fsharp
type Environment = Map<string, SExpression ref> ref
```

### Trade-off

| Aspect | Mutable (`ref` + `ref`) | Persistent (Immutable Map) |
|--------|------------------------|----------------------------|
| `set!` semantics | ✅ `ref` cells allow mutation from parent scopes | ❌ Would need to copy the environment chain |
| `parameterize` | ✅ Parameter values are `ref` cells, easy to save/restore | ❌ Requires threading environment through `parameterize` body |
| `define` mutation | ✅ `ref` on the outer map enables existing-define rebinding | ❌ Would need structural sharing with mutation |
| Performance | ✅ Simple pointer operations | ❌ Copy-on-write overhead |
| Thread safety | ❌ Mutation visible across all references | ✅ Immutable by default |

### Rationale

R7RS `set!` mutates an existing binding in the environment where it was defined, not the innermost scope. This means `ref` cells are needed so that a `set!` in a nested scope can modify a binding from a parent scope. Without `ref` cells, the entire environment chain would need to be searched and a new map constructed at each `set!`.

Similarly, `define` at the top level rebinds existing names (but shadows in internal definitions), which requires mutating the environment map itself (`Environment` is `Map<string, SExpression ref> ref`).

---

## 10. Floyd's Cycle Detection vs Visited-Set

### Context

`toList` and `isProperList` use **Floyd's cycle-finding algorithm** (tortoise and hare), while the printer uses a **visited-set** (identity comparison list).

### Trade-off

| Aspect | Floyd's Algorithm | Visited-Set |
|--------|------------------|-------------|
| Memory | ✅ O(1) — two pointers only | ❌ O(n) — stores all visited nodes |
| Speed | ❌ Multiple traversals for large lists | ✅ Single pass |
| Detect which node is cyclic | ❌ Only detects presence of cycle | ✅ Can identify the specific cycle point |
| Use in `toList` | ✅ Natural fit for length detection | ❌ Would need to track separately |
| Use in printer | ❌ Cannot determine print boundaries easily | ✅ Natural fit: "have I printed this?" |

### Rationale

Using both approaches is the right call:

- **`toList`/`isProperList`**: Floyd's is ideal because these functions only care about whether a list is proper and need to find its end. The O(1) memory is a nice bonus for list operations that may be called on very long lists.
- **Printer (`Print.fs`)**: The visited-set is better because the printer needs to know *which specific objects* have been printed to avoid infinite loops, and it must print `...` at the exact cycle point. Identity comparison (`obj.ReferenceEquals`) on pairs and vectors is the most reliable cycle detection for the printer.

This hybrid approach gets the best of both: low memory overhead for list operations, and precise cycle reporting for printing.

---

## 11. Result\<SExpression, SkipResult\> vs Exceptions

### Context

Errors are returned as `Result<SExpression, SkipResult>` values, not thrown as .NET exceptions.

```fsharp
type SkipResult =
    | EvalError of string * Position option
    | ParseError of string * Position option
    | SchemeRaise of SExpression * Position option
```

### Trade-off

| Aspect | `Result<_, SkipResult>` | .NET Exceptions |
|--------|------------------------|-----------------|
| CPS integration | ✅ Composes naturally with continuations | ❌ Exception handler must be installed on stack |
| `call/cc` | ✅ Continuation captures `Ok`/`Error` state | ❌ Exception state is in the runtime, not captured |
| `raise`/`with-exception-handler` | ✅ Handlers are explicit CPS functions | ❌ Must map to/from .NET exceptions |
| Performance in success path | ✅ No exception overhead | ❌ `try` blocks have some cost |
| Boilerplate | ❌ All functions return `Result` | ✅ Exceptions propagate automatically |

### Rationale

CPS and `Result` are a natural fit — the continuation receives a `Result<SExpression, SkipResult>`, so `call/cc` correctly captures the error state. If .NET exceptions were used, `call/cc` would capture the exception handler state from the .NET runtime, which doesn't compose cleanly with Scheme's continuation semantics.

Additionally, Scheme's `with-exception-handler` and `raise` implement a custom exception system that doesn't map directly to .NET exceptions. Using `Result` keeps the control flow explicit and composable with CPS.

---

## 12. Separate Datum Label Resolution Pass vs Inline During Parsing

### Context

Datum labels (`#N=expr` / `#N#`) are resolved in a **separate pass** (`DatumLabel.fs`) after parsing, rather than resolved during parsing.

### Trade-off

| Aspect | Separate Pass | Inline During Parsing |
|--------|--------------|----------------------|
| Implementation complexity | ✅ Each phase is simple and focused | ❌ Parser must manage label state and resolve on the fly |
| Error detection | ✅ Clear phases for validation (no duplicates, no forward refs) | ❌ Errors are detected interleaved with parsing |
| Code organization | ✅ Readable separation of concerns | ❌ Parser becomes more complex |
| Performance | ❌ Two tree traversals | ✅ Single pass |

### Rationale

Datum label resolution involves:

1. **Collecting** all label definitions from the tree.
2. **Validating** the labels (no duplicates, no forward references, no invalid circular refs).
3. **Substituting** back-references with the actual values.

A separate pass keeps the parser simple and focused on syntax. The label resolver operates purely on the SExpression tree with clear error messages. The performance cost of a second traversal is negligible compared to the parsing time.

---

## 13. Centralized Builtin Binding List vs Distributed Registration

### Context

All built-ins are registered in a single `builtinBindings` list in `Builtin.fs`, rather than each module registering itself.

### Trade-off

| Aspect | Centralized List | Distributed Registration |
|--------|-----------------|------------------------|
| Discoverability | ✅ All bindings visible in one place | ❌ Must search across modules |
| Addition workflow | ❌ Must edit two places (impl + registration) | ✅ Register at definition site |
| Conflict detection | ✅ Single file, easy to spot duplicates | ❌ Conflicts may go unnoticed |
| Build order | ✅ Easy to control with explicit list | ❌ Registration order depends on module init |

### Rationale

A centralized list makes it easy to see all available procedures and special forms at a glance, which is valuable for an interpreter where the full API surface matters. The cost of editing two places (implementation module + registry) is minor.

---

## 14. AutoOpen Modules in Builtin/ vs Explicit Imports

### Context

All Builtin modules use `[<AutoOpen>]`:

```fsharp
[<AutoOpen>]
module SpecialForm =
    ...
```

### Trade-off

| Aspect | `[<AutoOpen>]` | Explicit Imports |
|--------|---------------|------------------|
| Conciseness | ✅ Functions accessible without qualification | ❌ Must open each module |
| Namespace pollution | ❌ All function names visible everywhere | ✅ Namespace is controlled |
| Module boundaries | ❌ Unclear which function comes from which module | ✅ Clear provenance |

### Rationale

Since all Builtin functions are assembled in `Builtin.fs` and referenced by name in `builtinBindings`, `[<AutoOpen>]` avoids the boilerplate of opening 17 modules. The names are relatively unique (prefixed with `s` for procedures like `sCons`, `sCar`, or descriptive like `isPair`), so name collisions are rare. New contributors can see the module name in the file path when needed.

---

## 15. CPS Printer with Visited-Set vs Simple Recursive Printer

### Context

The printer (`Print.fs`) is implemented in CPS with a visited-set for cycle detection, rather than as a simple recursive function.

### Trade-off

| Aspect | CPS + Visited-Set | Simple Recursive |
|--------|------------------|------------------|
| Cycle handling | ✅ Detects and prints `...` for cycles | ❌ Infinite recursion on cyclic structures |
| Stack safety | ✅ Tail-recursive, no stack overflow | ❌ Deeply nested structures overflow stack |
| Implementation complexity | ❌ Continuations + visited tracking | ✅ Simple, direct recursion |

### Rationale

Cycle handling is essential because Scheme programs can create cyclic lists via `set-cdr!`. With a simple recursive printer, `(let ((x '(a b c))) (set-cdr! (cddr x) x) x)` would hang forever. The CPS form also prevents stack overflow on deeply nested expressions (the continuation chain is heap-allocated rather than stack-allocated).

---

## 16. Reader Macro Expansion During Parsing vs Post-Parse Transformation

### Context

Reader macros (`'expr` → `(quote expr)`, `` `expr `` → `(quasiquote expr)`, `,expr` → `(unquote expr)`, `,@expr` → `(unquote-splicing expr)`) are expanded **directly by the parser**, producing the expanded form immediately.

### Trade-off

| Aspect | During Parsing | Post-Parse Transformation |
|--------|---------------|--------------------------|
| Complexity | ✅ Single representation from the start | ❌ Need a separate walk to desugar |
| Performance | ✅ No extra tree traversal | ❌ Additional pass over the tree |
| Parser coupling | ❌ Parser knows about reader macro expansion | ✅ Parser is purely syntactic |
| Error reporting | ✅ Errors reference original source position | ✅ Still possible if positions are preserved |

### Rationale

Expanding reader macros during parsing eliminates a full tree walk and produces the canonical representation immediately. The parser already handles these forms by producing `SQuote`, `SQuasiquote`, `SUnquote`, and `SUnquoteSplicing` nodes — this is the simplest approach since the parser naturally encounters these syntax constructs while processing `'`, `` ` ``, `,`, and `,@` tokens.

---

## 17. SNumber Unified Type vs Separate SExpressionKind Cases for Numeric Operations

### Context

Arithmetic operations (`+`, `-`, `*`, `/`) previously dispatched on pairs of `SExpressionKind` variants (comparing `SRational`/`SReal`/`SComplex` combinations), leading to 7+ branches per operation and very high cyclomatic complexity in `loopCalc` (93).

### Trade-off

`SNumber` (`Builtin/Number.fs`) is a discriminated union unifying all numeric types:

```fsharp
type SNumber = NRational of bigint * bigint | NReal of float | NComplex of Complex
```

| Aspect | SNumber Unification | Per-Type Dispatch in SExpression |
|--------|-------------------|---------------------------------|
| Arithmetic dispatch | ✅ 3 SNumber cases, unified | ❌ 7+ SExpressionKind pair combinations |
| Conversion overhead | ❌ Must convert SExpression → SNumber → SExpression | ✅ Direct match on SExpressionKind |
| Code duplication | ✅ `add`/`sub`/`mul`/`div` are single functions | ❌ Repeated type-checking in each operation |
| Cyclomatic complexity | ✅ `loopCalc`: 93 → 10 | ❌ Very high per-operation complexity |
| File organization | ✅ Separate `Number.fs` + `Math.fs` | ✅ Single file |

### Rationale

The unification was motivated by complexity — `loopCalc` at 93 cyclomatic complexity was far above the 15 threshold. Converting to `SNumber` at the entry point (via `ofExpr`/`toSExpr`) and operating on the 3-case DU reduces each arithmetic operation to a simple 3-way match. The conversion overhead is negligible since most operations are already CPU-bound by bigint arithmetic.

`nRational` is the canonical `NRational` constructor that normalizes the rational (GCD-reduced, zero-denominator check, sign normalization). It replaces ad-hoc normalization previously scattered across Math.fs.

---

## 18. QqKeyword DU vs Raw Symbol Matching in Quasiquote Expansion

### Context

The quasiquote expander (`replaceQuasiquoteDatum`, `replaceQuasiquoteList`) checked for quasiquote keywords by comparing against string literals (`"quasiquote"`, `"unquote"`, `"unquote-splicing"`, `"quote"`) in multiple places, with high cyclomatic complexity (`replaceQuasiquoteDatum`: 57, `replaceQuasiquoteList`: 59).

### Trade-off

A `QqKeyword` discriminated union normalizes all keyword comparisons:

```fsharp
type QqKeyword = QqUnquote | QqUnquoteSplicing | QqQuasiquote | QqQuote
```

| Aspect | QqKeyword DU | Raw Symbol Comparison |
|--------|-------------|----------------------|
| Keyword matching | ✅ Single `normalizeQqKeyword` function | ❌ Repeated string comparison everywhere |
| Code duplication | ✅ `consQq`/`joinQq` build keyword-tagged pairs | ❌ Ad-hoc pair construction |
| Complexity | ✅ `replaceQuasiquoteDatum`: 57→12, `replaceQuasiquoteList`: 59→30 | ❌ Very high per-function complexity |
| New keyword addition | ✅ Add variant + update `normalizeQqKeyword` | ❌ Find and update all comparisons |

### Rationale

By extracting keyword normalization into a single function and introducing `consQq`/`joinQq` as pair-tree constructors that use the normalized keyword, the quasiquote expander's main functions were dramatically simplified. The `replaceQuasiquoteDatum` function went from 57 to 12 complexity.

---

## 19. CPS Incompatibility with Ref Cells for Accumulation

### Context

During refactoring, an attempt was made to replace CPS continuation threading with a mutable `ref` cell in `matchPatternListWithEllipsisParts` (`Builtin/Macro.fs`).

### Trade-off

| Aspect | CPS Continuation Threading | Mutable Ref Cell |
|--------|---------------------------|-----------------|
| Composition | ✅ Composes naturally with caller's continuation | ❌ Ref cell captures only the final continuation value |
| Referential transparency | ✅ Pure data flow | ❌ Hidden mutable state |
| Correctness | ✅ Always correct | ❌ Breaks when multiple continuations share the same ref |
| Boilerplate | ❌ Must explicitly thread state through `cont` | ✅ Simple assignment |

### Rationale

This was discovered empirically — replacing `cont` calls with ref cell assignments produced incorrect results because CPS relies on the continuation chain to compose partial results across recursive calls. A ref cell captures only the **last** value written, losing the chain. The lesson is that CPS and mutable state don't mix: if you're in CPS, thread everything through `cont`.

---

## 20. Option List Accumulator vs Plain List in `loopListInfo`

### Context

`loopListInfo` (`Type.fs`) previously used `Option<SExpression list>` for the accumulator parameter, where `None` meant "don't accumulate" (used by `isProperList`) and `Some list` meant "accumulate" (used by `toList`).

### Trade-off

| Aspect | Plain List `[]` | `Option<SExpression list>` |
|--------|----------------|---------------------------|
| API surface | ✅ `loopListInfo pair pair 0I []` (always accumulates) | ❌ `loopListInfo pair pair 0I None` vs `Some []` |
| Internal complexity | ✅ 2 branches (pair continue / empty finish) | ❌ 2× branches (None vs Some for each case) |
| Caller adaptation | ❌ `isProperList` discards the result with `|> ignore` | ✅ No extra computation |

### Rationale

The `Option` was eliminated because the complexity overhead (wrapping/unwrapping `Option` cases in the loop) was not justified by the micro-optimization of skipping list construction for `isProperList`. All callers now unconditionally build the list, and `isProperList` discards it with pattern matching (the compiler may optimize this away). The elimination simplified the function signature and removed the `failwith "unreachable."` branch.
