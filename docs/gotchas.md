# Recurring Gotchas

This document records common mistakes, subtle pitfalls, and non-obvious behaviors encountered when working with this codebase.

---

## Table of Contents

- [1. Result Handling: `cont` Must Be Called in Tail Position](#1-result-handling-cont-must-be-called-in-tail-position)
- [2. `eqv?` vs `equal?` Semantics](#2-eqv-vs-equal-semantics)
- [3. Mutable Pair Identity in `eqv?` and `equal?`](#3-mutable-pair-identity-in-eqv-and-equal)
- [4. String Mutability: Immutable Strings from `symbol->string`](#4-string-mutability-immutable-strings-from-symbol-string)
- [5. `evalArgs` Rejects Multiple Values in Single-Value Context](#5-evalargs-rejects-multiple-values-in-single-value-context)
- [6. The `eachEval` Accumulator Pattern](#6-the-eacheval-accumulator-pattern)
- [7. `collectInternalDefinitions` Destructures `begin` Blocks](#7-collectinternaldefinitions-destructures-begin-blocks)
- [8. `toList` Returns Error for Improper Lists](#8-tolist-returns-error-for-improper-lists)
- [9. Floyd's Algorithm in `loopListInfo` Skips Every Other Element](#9-floyds-algorithm-in-looplistinfo-skips-every-other-element)
- [10. Printer Visited-Set Uses Identity Comparison](#10-printer-visited-set-uses-identity-comparison)
- [11. Printer Cycle Detection Only Tracks Pairs, Vectors, and Values](#11-printer-cycle-detection-only-tracks-pairs-vectors-and-values)
- [12. `resolveDatumRefPair` Mutates Pair Fields In-Place](#12-resolvedatumrefpair-mutates-pair-fields-in-place)
- [13. `collectDatum` Does Not Descend into `SSymbol`, `SString`, etc.](#13-collectdatum-does-not-descend-into-ssymbol-sstring-etc)
- [14. `Context.reset` After Errors Clears Winders and Handlers](#14-contextreset-after-errors-clears-winders-and-handlers)
- [15. `SchemeRaise` Bypasses Normal Continuation Chain](#15-schemeraise-bypasses-normal-continuation-chain)
- [16. Error Message Testing Must Use `should startWith`](#16-error-message-testing-must-use-should-startwith)
- [17. `failwith "unreachable."` Guarded by Invariants Only](#17-failwith-unreachable-guarded-by-invariants-only)
- [18. Macro Pattern Matching Overloads `matchOne` with Many Cases](#18-macro-pattern-matching-overloads-matchone-with-many-cases)
- [19. Macro Ellipsis Matching: `EllipsisB` Groups Pattern Variables](#19-macro-ellipsis-matching-ellipsisb-groups-pattern-variables)
- [20. `decodePair` Splits List into Prefix and Tail](#20-decodepair-splits-list-into-prefix-and-tail)
- [21. Quasiquote Nested Expansion](#21-quasiquote-nested-expansion)
- [22. `doAroundProc` vs Direct CPS for Dynamic-Wind](#22-doaroundproc-vs-direct-cps-for-dynamic-wind)
- [23. `newContext` Does Not Copy Libraries](#23-newcontext-does-not-copy-libraries)
- [24. String Case Conversion Uses Invariant Culture](#24-string-case-conversion-uses-invariant-culture)
- [25. `realToRational` Float-to-Rational Conversion Precision](#25-realtorational-float-to-rational-conversion-precision)
- [26. `SUnspecified` Is a Valid Return Value but Prints Distinctively](#26-sunspecified-is-a-valid-return-value-but-prints-distinctively)
- [27. `eqv?` on Numbers Does Not Distinguish `+0.0`/`-0.0`](#27-eqv-on-numbers-does-not-distinguish-00-00)
- [28. `make-parameter` with Converter Applies on Every Call](#28-make-parameter-with-converter-applies-on-every-call)
- [29. CPS Continuations Are Incompatible with Ref Cells](#29-cps-continuations-are-incompatible-with-ref-cells)
- [30. Cobertura Cyclomatic Complexity May Not Reflect Structural Improvements](#30-cobertura-cyclomatic-complexity-may-not-reflect-structural-improvements)

---

## 1. Result Handling: `cont` Must Be Called in Tail Position

### Symptom

Calls to `cont` that are not in tail position cause stack growth, defeating the purpose of CPS.

### Root Cause

The CPS convention requires that `cont` always be called in tail position:

```fsharp
// ✅ Correct: cont in tail position
doSomething x |> Ok |> cont

// ✅ Correct: result bound and passed to cont in tail
let result = compute x
Ok result |> cont

// ❌ Wrong: cont is not in tail position
let result = compute x
cont (Ok result)  // This adds a return frame!
someOtherOperation ()  // unreachable but compiler doesn't know
```

### Prevention

Always end a function branch with a call to `cont`, and ensure it's the last expression — not followed by anything else.

---

## 2. `eqv?` vs `equal?` Semantics

### Symptom

`eqv?` uses physical equality for pairs, vectors, bytevectors, continuations, and procedures while `equal?` uses structural/recursive equality for pairs and vectors.

### Root Cause

`eqv?` (`Builtin/Helper.fs` lines 59–72):

```fsharp
| (SPair x, _), (SPair y, _) -> LanguagePrimitives.PhysicalEquality x y
| (SVector x, _), (SVector y, _) -> LanguagePrimitives.PhysicalEquality x y
```

`equal?` (`Builtin/Core.fs` loops) recursively compares elements.

### Pitfall

Two separate calls to `(list 1 2 3)` produce different objects, so `(eqv? (list 1 2 3) (list 1 2 3))` is `#f` but `(equal? ...)` is `#t`. This matches R7RS but may surprise users from other languages.

---

## 3. Mutable Pair Identity in `eqv?` and `equal?`

### Symptom

`eqv?` on pairs uses `LanguagePrimitives.PhysicalEquality x y` on `SPairData`, which is reference equality because `SPairData` is marked `[<ReferenceEquality>]` (Type.fs line 41).

### Pitfall

Because `SPairData` has `mutable car` and `mutable cdr`, two pairs that happen to have the same current contents are not `eqv?` unless they are the exact same object. However, `equal?` handles cycles via a visited-set to avoid infinite loops on cyclic pairs.

### Prevention

When implementing new list operations, always use `equal?` semantics for structural comparison and `eqv?` for identity checks. Never assume structural equality on pairs without cycle protection.

---

## 4. String Mutability: Immutable Strings from `symbol->string`

### Symptom

`(string-set! (symbol->string 'foo) 0 #\a)` raises an error.

### Root Cause

`symbol->string` returns a string marked as immutable (`isImmutable = true`):

```fsharp
let sSymbolToString context pos cont =
    function
    | [ SSymbol s, _ ] -> Ok(s |> newSString true, pos) |> cont
```

Per R7RS, the result of `symbol->string` is immutable. The `isImmutable` flag is checked in `string-set!`, `string-fill!`, and `string-copy!` (`Builtin/Str.fs`).

### Gotcha

All newly constructed strings from the library use `isImmutable = false`. But strings created via `symbol->string` or obtained from literal data in the source are immutable. Always check `isImmutable` before mutation.

---

## 5. `evalArgs` Rejects Multiple Values in Single-Value Context

### Symptom

```scheme
(+ (values 1 2))  ; error: "Multiple values in single value context."
```

### Root Cause

`evalArgs` (Eval.fs line 76) explicitly checks for `SValues`:

```fsharp
| Ok(SValues _, p) -> EvalError("Multiple values in single value context.", p) |> Error |> cont
```

### Pitfall

This means `(list (values 1 2))`, `(string (values #\a #\b))`, and similar constructs that try to pass multiple values where a single value is expected will raise an error, as required by R7RS.

---

## 6. The `eachEval` Accumulator Pattern

### Symptom

`eachEval` does not accumulate results — it only returns the **last** value:

```fsharp
let rec eachEval context cont acc =
    function
    | [] -> acc |> cont
    | x :: xs ->
        x |> eval context (function
            | Ok a -> xs |> eachEval context cont (Ok a)
            | x -> x |> cont)
```

### Pitfall

`eachEval` is used for `begin` blocks and lambda bodies. The `acc` parameter is the **last successfully evaluated result**, not a list of all results. This is correct for `begin`, but if you need a list of results, you must use a different accumulation strategy.

---

## 7. `collectInternalDefinitions` Destructures `begin` Blocks

### Symptom

Internal definitions work only if `begin` blocks are transparently flattened.

### Root Cause

`collectInternalDefinitions` (Eval.fs line 115) recursively destructures `begin` forms found within a body:

```fsharp
| SPair { car = SSymbol "begin", _; cdr = inner }, _ ->
    match inner |> toList with
    | Ok ilist -> ilist :: rest :: stack |> collectInternalDefinitions acc
```

### Pitfall

If `toList` fails (the `begin` body is an improper list), the collector falls back to treating the remaining forms as expressions. This means a malformed `begin` inside internal definitions can silently change semantics.

---

## 8. `toList` Returns Error for Improper Lists

### Symptom

`(length '(1 2 . 3))` produces an error, not a result.

### Root Cause

`toList` (`Type.fs` lines 112–120) uses `loopListInfo` which returns `Error` for improper lists:

```fsharp
| Ok(None, _) -> failwith "unreachable."
| Error msg -> EvalError(msg, snd pair) |> Error
```

### Pitfall

Many list operations (`map`, `append`, `reverse`, `member`, etc.) call `toList` internally. Passing an improper list to these procedures raises an error. In contrast, Scheme's `list?` correctly returns `#f` for improper lists (using the same `loopListInfo` without accumulation).

---

## 9. Floyd's Algorithm in `loopListInfo` Skips Every Other Element

### Symptom

Floyd's cycle detection is split across two mutually recursive functions (`Type.fs` lines 86–101): `loopListInfo` advances the hare and terminates on empty/improper lists, while `checkAndAdvance` advances the tortoise and detects cycles:

```fsharp
[<TailCall>]
let rec loopListInfo tortoise hare accLength accList =
    match hare with
    | SEmpty, _ -> Ok(List.rev accList, accLength)
    | SPair pHare, _ ->
        match pHare.cdr with
        | SEmpty, _ -> Ok(List.rev (pHare.car :: accList), accLength + 1I)
        | SPair pHareNext, _ -> checkAndAdvance tortoise pHareNext accLength accList pHare.car
        | _ -> Error "not a proper list."
    | _ -> Error "not a proper list."

and checkAndAdvance tortoise pHareNext accLength accList pCar =
    match tortoise with
    | SPair pTortoise, _ when obj.ReferenceEquals(pTortoise, pHareNext) -> Error "circular list."
    | SPair pTortoise, _ ->
        loopListInfo pTortoise.cdr pHareNext.cdr (accLength + 2I) (pHareNext.car :: pCar :: accList)
    | _ -> Error "invalid list structure."
```

The tortoise advances by one while the hare advances by two (via `pHareNext.cdr`). The accumulator collects elements in **reverse order** and must be `List.rev`-ed at the end.

### Pitfall

When extending these functions, be careful with the accumulator reversal and the invariant that the tortoise pointer checks `pHareNext` (hare's second step) for cycle detection, not `pHare.cdr` (hare's first step). The `checkAndAdvance` helper is tightly coupled to `loopListInfo` — any change to the calling convention must update both functions.

---

## 10. Printer Visited-Set Uses Identity Comparison

### Symptom

The printer avoids infinite loops on cyclic structures using `obj.ReferenceEquals`.

### Root Cause

`isVisited` (Print.fs line 85) compares objects by identity:

```fsharp
let isVisited visited x =
    visited |> List.exists (fun v -> obj.ReferenceEquals(v, x))
```

### Pitfall

This only works for reference types. Value types (booleans, numerics, characters) never produce cycles, so they are never added to the visited list. However, when `SPair` objects are boxed to `obj` via `(pair :> obj)`, the identity comparison works correctly because `SPairData` has `[<ReferenceEquality>]`.

When the printer encounters `SVector` or `SValues` and detects a cycle, it prints `...`. But `formatPair` also prints `...` in cycle cases. The behavior differs slightly:

- **Pairs**: `(a b ...)` or `(...)`
- **Vectors**: `#(... ...)` — the entire vector is replaced with `...`
- **SValues**: `(values ...)` — the entire values list is replaced with `...`

---

## 11. Printer Cycle Detection Only Tracks Pairs, Vectors, and Values

### Symptom

The visited-set in `formatPair` only adds the current pair to visited:

```fsharp
let visited' = (pair :> obj) :: visited
```

But `formatError` also tracks whether the irritants list itself is visited. Other objects like records and datums are not tracked.

### Pitfall

If a record type has a field that circularly references itself (e.g., a record with a field pointing back to itself), the printer **does** handle it because `formatPair` adds the pair to visited. But if the cycle involves non-pair objects (e.g., a `SRecord` pointing to itself via an `SPair` that refers back), the pair insertion point in the cycle still provides protection.

---

## 12. `resolveDatumRefPair` Mutates Pair Fields In-Place

### Symptom

`DatumLabel.fs` line 100–111:

```fsharp
pair.car
|> resolveDatumRef labels (Result.bind (fun car ->
    pair.car <- car                        // mutation!
    pair.cdr
    |> resolveDatumRef labels (Result.bind (fun cdr ->
        pair.cdr <- cdr                    // mutation!
        Ok(SPair pair, pos) |> next)))
```

### Pitfall

The datum label resolver mutates `pair.car` and `pair.cdr` in place during the second traversal. This means the original parsed SExpression tree is **modified** during resolution. If resolution fails partway through, the tree may be left in a partially-modified state.

This is safe in practice because `resolveLabels` calls `resolveDatumRef` once, and any error causes the entire result to be discarded. But debugging may be confusing if an error occurs mid-resolution.

---

## 13. `collectDatum` Does Not Descend into `SSymbol`, `SString`, etc.

### Symptom

`collectDatum` recursively walks the tree looking for `SDatumLabel` definitions, but it does not descend into certain leaf types.

### Root Cause

`collectDatum` pattern matches on `SPair`, `SVector`, `SRecord`, `SValues`, `SError`, `SQuote`, `SQuasiquote`, `SUnquote`, `SUnquoteSplicing` — and the catch-all `| _ -> rest` skips everything else.

### Pitfall

If a datum label definition is embedded inside a `SPromise`, `SParameter`, or other non-traversed type, it will not be collected. However, these types cannot appear in parsed source text (they are only created during evaluation), so this is safe in practice.

---

## 14. `Context.reset` After Errors Clears Winders and Handlers

### Symptom

After a Scheme error in the REPL, `dynamic-wind` guards and exception handlers are reset.

### Root Cause

`Repl.fs` line 17:

```fsharp
|> Result.defaultWith (fun e ->
    context |> Context.reset
    ...)
```

`Context.reset` clears winders and restores the default exception handlers. This prevents accumulated state from leaking between REPL inputs.

### Pitfall

If you're using the `rep` function programmatically (e.g., in tests), a Scheme error **also** resets the context. This means subsequent calls to `rep` start with a clean state. If your test depends on state surviving an error, it will fail.

---

## 15. `SchemeRaise` Bypasses Normal Continuation Chain

### Symptom

`raise` inside a procedure does not unwind the stack in the traditional sense — it matches against installed exception handlers.

### Root Cause

`SchemeRaise` (like `EvalError` and `ParseError`) is an `Error` case in the `Result` type. The `with-exception-handler` (`Builtin/Exception.fs`) installs a handler on the `Context.handlers` stack, and `raise` pops a handler and applies it.

### Pitfall

If no handler is installed, the initial handler (set up in `Context.initialHandlers`) catches the raise and converts it to an `EvalError`. The initial handler is:

```fsharp
SProcedure(fun _ pos cont ->
    function
    | [ obj ] -> SchemeRaise(obj, pos) |> Error |> cont
    | _ -> failwith "unreachable."),
```

The `| _ -> failwith "unreachable."` branch is hit only if there's a bug in `raise` itself (it should always pass exactly one argument).

---

## 16. Error Message Testing Must Use `should startWith`

### Symptom

```fsharp
// ❌ This fails:
"(bad)" |> rep |> should equal "'()' invalid bad parameter"

// ✅ This works:
"(bad)" |> rep |> should startWith "'()' invalid bad parameter"
```

### Root Cause

The REPL appends source position information at the end of error messages via `formatPosition`:

```fsharp
let formatPosition =
    function
    | Some pos -> sprintf " (at line %d, column %d)" pos.line pos.column
    | None -> ""
```

So the actual output is `"'()' invalid bad parameter (at line 1, column 4)"`.

### Prevention

Always use `should startWith` when asserting error messages. Never use `should equal` on error messages unless you also include the position suffix.

---

## 17. `failwith "unreachable."` Guarded by Invariants Only

### Symptom

Several places in the codebase use `failwith "unreachable."` to mark branches that should never execute:

| Location | Context |
|----------|---------|
| `Read.fs:140` | Parser character class fallback |
| `Context.fs:10` | Initial exception handler with wrong number of arguments |
| `Builtin/Helper.fs:93,97` | `loopDiffWinders` with mismatched lengths |

### Pitfall

These are not guarded by any runtime checks — if the invariant is violated (e.g., due to a refactoring), the program will crash with an unhandled `System.Exception`. New code should avoid adding new `failwith` calls; prefer returning a well-typed error via `Result` instead.

---

## 18. Macro Pattern Matching in `matchOne` with Many Cases

### Symptom

`matchOne` in `Builtin/Macro.fs` handles many literal types. The 6 atomic cases (`SEmpty`, `SBool`, `SRational`, `SReal`, `SString`, `SChar`) are extracted into a `matchAtom` helper, while `SSymbol "_"`, ellipsis, literal symbols, `SPair`, and `SVector` are matched directly.

### Pitfall

If a new `SExpressionKind` variant is added (e.g., `SComplex`), `matchOne` will not match it (the catch-all `| _ -> None` at the end), so macros will silently fail to match any pattern containing that literal value. `matchAtom` also needs updating for new atomic types.

Similarly, `matchPatternListWithEllipsisParts` must be updated for any new compound types that can appear in macro patterns.

---

## 19. Macro Ellipsis Matching: `EllipsisB` Groups Pattern Variables

### Symptom

`buildEllipsisBindings` groups repeated pattern matches under `EllipsisB`:

```fsharp
let buildEllipsisBindings variables bindings =
    variables |> List.fold (fun acc variable ->
        let values = bindings |> List.map (fun binding ->
            match binding |> Map.tryFind variable with
            | Some b -> b
            | None -> SingleB(SEmpty, None))  // ⚠️ defaults to empty!
        acc |> Map.add variable (EllipsisB values)) Map.empty
```

### Pitfall

If a pattern variable does not appear in a particular iteration's bindings (e.g., because it wasn't present in that match), it defaults to `SingleB(SEmpty, None)` — an empty list element. This can produce unexpected template output if a conditional or optional pattern element is involved.

---

## 20. `decodePair` Splits List into Prefix and Tail

### Symptom

`decodePair` (Macro.fs line 15–18) converts a pair tree into a list of elements plus an optional tail:

```fsharp
let rec decodePair acc =
    function
    | SPair p, _ -> p.cdr |> decodePair (p.car :: acc)
    | x -> acc |> List.rev, x
```

### Pitfall

The second return value (`x`) is the tail of the list — it's `SEmpty` for proper lists, but for improper lists it's the terminating non-pair value. The macro engine uses this to handle improper list patterns like `(a . b)`.

When using `decodePair` elsewhere, always check whether the tail is `SEmpty` before assuming the result is a proper list.

---

## 21. Quasiquote Nested Expansion

### Symptom

The quasiquote implementation (`Builtin/SpecialForm.fs`) handles nested quasiquotation with a depth counter (`n`). The functions `replaceQuasiquoteDatum`, `replaceQuasiquoteList`, etc. thread this counter through recursive calls.

### Pitfall

The depth tracking is complex and involves mutual recursion between `replaceQuasiquoteDatum`, `replaceQuasiquoteList`, `replaceQuasiquoteListItems`, and several other helpers. A bug in the depth tracking can cause nested quasiquotes (e.g., `` `(,x `(,,x)) ``) to expand incorrectly.

If modifying quasiquote expansion, always test with:

```scheme
``(a ,b c)        ; double quasiquote
,,x               ; double unquote
`(,x `(,,x))      ; mixed nesting
```

### Refactoring Note

The quasiquote code was refactored to use a `QqKeyword` DU (`QqUnquote | QqUnquoteSplicing | QqQuasiquote | QqQuote`) that normalizes the various keyword forms (`unquote`, `unquote-splicing`, `quasiquote`, `quote`). The `normalizeQqKeyword` function maps any Scheme keyword form to its `QqKeyword` variant. The helpers `consQq` and `joinQq` construct pair trees with normalized quasiquote keywords, reducing duplication in the expansion logic.

---

## 22. `doAroundProc` vs Direct CPS for Dynamic-Wind

### Symptom

`dynamic-wind` in `Procedure.fs` uses `doAroundProc`, which wraps the thunk with before/after procedures using `SProcedure` values:

```fsharp
let doAroundProc context cont before thunk after =
    // before is called on entry
    // thunk is executed
    // after is called on exit
```

### Pitfall

The before/after procedures are themselves evaluated as Scheme procedures via `Eval.apply`. This means:

1. `before` and `after` run in the evaluator, not as direct F# calls.
2. Errors in `before` or `after` propagate through the CPS chain.
3. The `SProcedure` wrapper means they appear in stack traces as procedure calls.

This also means `dynamic-wind` has overhead even when no winders are active, because the continuation always checks and potentially runs winder differences via `doWind` in `Procedure.fs`.

---

## 23. `newContext` Does Not Copy Libraries

### Symptom

`Repl.newContext()` creates a fresh REPL context but shares the library registry:

```fsharp
let newContext () =
    let context = Builtin.builtinContext
    { context with
        environments = (Map.empty |> ref) :: context.environments
        ... }
```

The `libraries` field is a `Map<string, Library> ref` and is **not** replaced — it's inherited from `builtinContext`.

### Pitfall

`define-library` registers libraries into the shared `context.libraries` ref cell. Since `newContext` does not reset or copy it, libraries accumulate across REPL sessions if the same process runs multiple `newContext()` calls. This is usually fine, but a test that calls `newContext()` multiple times may see libraries from previous sessions.

---

## 24. String Case Conversion Uses Invariant Culture

### Symptom

String case conversion uses `ToUpperInvariant()` / `ToLowerInvariant()`:

```fsharp
let sStringUpcase context pos cont =
    function
    | [ SString s, _ ] ->
        Ok((s.runes |> runesToString).ToUpperInvariant() |> newSString false, pos)
```

### Pitfall

In Scheme, `string-upcase` should follow Unicode's Default Case Conversion, which is **culture-invariant** but does differ from `ToUpperInvariant()` in some edge cases (e.g., Turkish `i`/`I` handling, which `ToUpperInvariant` handles in a culture-invariant way that differs from ICU). For most ASCII and common Unicode text this is fine, but for full R7RS conformance, ICU-based case conversion would be needed.

---

## 25. `realToRational` Float-to-Rational Conversion Precision

### Symptom

`realToRational` (Type.fs line 147–167) converts a float to the nearest exact rational by formatting it with `%.17g` and parsing the decimal representation:

```fsharp
let s = sprintf "%.17g" r
```

### Pitfall

Using `%.17g` means the conversion is limited by the formatting precision. Very small or very large floats may lose precision in the string representation. Additionally, the conversion always succeeds (falls back to `SReal r` on error), so errors are silently swallowed.

This function is used by `inexact->exact`, and the conversion may not match R7RS's requirement for exact rational representation of all IEEE 754 doubles.

---

## 26. `SUnspecified` Is a Valid Return Value but Prints Distinctively

### Symptom

`SUnspecified` prints as `#<unspecified>` and is a valid Scheme value, but certain uses can be surprising:

- `(define x (if #f 1))` — `x` is bound to `SUnspecified`, which prints as `#<unspecified>`.
- `(list (if #f 1))` evaluates to `(#<unspecified>)`.

### Pitfall

Unspecified values are valid Scheme values but are not `eqv?` to anything (except themselves by identity). Since `SUnspecified` is a singleton DU case (not a reference type), `eqv?` compares it by structural equality (the default for F# DUs), so two `SUnspecified` values are `eqv?` to each other — which is implementation-defined but accepted.

---

## 27. `eqv?` on Numbers Does Not Distinguish `+0.0`/`-0.0`

### Symptom

`(eqv? +0.0 -0.0)` returns `#t`.

### Root Cause

`eqv?` for reals uses `x = y` which is IEEE 754 equality where `+0.0 = -0.0` is `true`.

R7RS says `eqv?` on reals should behave like `=` (which also treats `+0.0` and `-0.0` as equal). So this is actually correct per R7RS, but may surprise users who expect `eqv?` to distinguish signed zeros.

---

## 28. `make-parameter` with Converter Applies on Every Call

### Symptom

The converter function passed to `make-parameter` is applied whenever the parameter is **set**, not when it's read.

### Root Cause

In `Builtin/SavedParameter.fs`, the converter is stored alongside the parameter's current value. When `parameterize` sets a new value, it applies the converter. When the parameter value is read (via `applyParameter` in Eval.fs), the raw value is returned directly.

```fsharp
let sMakeParameter context pos cont =
    function
    | [ init ] -> Ok(ref init |> SParameter, None) |> Ok |> cont
    | [ init; converter ] ->
        converter
        |> Eval.apply context (Result.map (fun conv ->
            ref init |> fun p -> SParameter(p, Some conv)) >> cont) [ init ]
    | x -> x |> invalidParameter pos "'%s' invalid make-parameter parameter." |> cont
```

### Pitfall

The converter is applied at `parameterize` time (not at read time). This means if you mutate the parameter value directly via `set!` on the underlying reference, the converter is **not** applied. This matches R7RS semantics, but it's a subtle distinction.

---

## 29. CPS Continuations Are Incompatible with Ref Cells

### Symptom

Replacing CPS continuation calls with mutable ref cells in a recursive function causes incorrect results and mysterious failures.

### Root Cause

CPS-style functions pass a continuation (`cont`) that is called with the result. If you try to accumulate results in a `ref` cell instead of threading them through the continuation chain, the ref cell captures only the **last** continuation's result rather than building the full chain:

```fsharp
// ❌ Wrong: ref cell captures only final continuation
let acc = ref []
let rec processList cont xs =
    match xs with
    | [] -> Ok(List.rev !acc) |> cont
    | x :: xs' ->
        acc := x :: !acc
        processList cont xs'  // same 'cont' passed through
```

This fails because CPS relies on the continuation chain to compose results. Ref cells break referential transparency and don't compose with the CPS calling convention.

### Prevention

When working in CPS code, always pass accumulated state forward through continuation arguments, never through mutable ref cells. If a helper function needs to be extracted, thread the state as a parameter through the recursive calls.

---

## 30. Cobertura Cyclomatic Complexity May Not Reflect Structural Improvements

### Symptom

After significant structural refactoring (extracting helpers, unifying match cases with or-patterns), the cyclomatic complexity reported by Coverlet may remain unchanged or even increase.

### Root Cause

Coverlet measures cyclomatic complexity at the **IL level**, counting individual IL branches. Structural improvements like:
- Merging match cases via or-patterns
- Simplifying destructuring in pattern matches
- Moving code to helper functions

do not always reduce the IL branch count if the underlying decision points remain. Helper extraction can increase total IL branches because the helper itself introduces new branches (e.g., match statements), even as it reduces the complexity of the original function.

### Prevention

Use Coverlet complexity as a **guide** rather than an absolute quality metric. The true measure is code maintainability (readability, testability, ease of modification). The project thresholds are:
- **Error threshold**: complexity > 15 — must be refactored
- **Warning threshold**: complexity > 10 — should be addressed where practical
- **Target**: ≤ 10 for most functions

Focus on functions above the error threshold (15) for meaningful reductions. Improvements that bring a function from 61 to 50 are still progress — break down large match expressions incrementally, one helper at a time.
