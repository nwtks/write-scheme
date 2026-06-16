# Language Reference

This document describes the Scheme language as implemented by this interpreter. It follows the R7RS (Small) specification.

> Most R7RS (Small) features are implemented. See [Known Limitations](#known-limitations) for details.

---

## Table of Contents

- [1. Data Types](#1-data-types)
- [2. Literal Syntax](#2-literal-syntax)
- [3. Special Forms](#3-special-forms)
- [4. Built-in Procedures](#4-built-in-procedures)
- [5. Macros](#5-macros)
- [6. Libraries](#6-libraries)
- [7. Known Limitations](#7-known-limitations)

---

## 1. Data Types

### 1.1 Booleans

Boolean values are represented as `#t` / `#true` (true) and `#f` / `#false` (false). In conditional contexts, `#f` is the only false value; everything else is truthy.

### 1.2 Numbers

The numeric tower supports integers, rationals, reals, and complex numbers.

| Category | Examples | Notes |
|----------|----------|-------|
| Integer | `42`, `-1`, `#x1F`, `#o17`, `#b1010` | Exact; stored as `numerator/1` |
| Rational | `1/2`, `10/3` | Exact; automatically reduced |
| Real | `3.14`, `1e2`, `+inf.0`, `-inf.0`, `+nan.0` | Inexact (IEEE 754 double) |
| Complex | `1+2i`, `1@1.57` | Inexact; rectangular or polar form |

Exactness prefixes:

| Prefix | Meaning | Example |
|--------|---------|---------|
| `#e` | Force exact | `#e1.0` → exact `1` |
| `#i` | Force inexact | `#i1/2` → inexact `0.5` |

Radix prefixes:

| Prefix | Radix | Example |
|--------|-------|---------|
| `#b` | Binary | `#b1010` → `10` |
| `#o` | Octal | `#o17` → `15` |
| `#x` | Hexadecimal | `#x1F` → `31` |
| `#d` | Decimal (default) | `#d42` → `42` |

### 1.3 Characters

Written as `#\` followed by the character.

| Example | Meaning |
|---------|---------|
| `#\a` | The character `a` |
| `#\x3071` | Character by Unicode code point (`ぱ`) |
| `#\space` | Space character |
| `#\newline` | Newline character |

### 1.4 Strings

Written in double quotes. Supports escape sequences:

| Escape | Meaning |
|--------|---------|
| `\"` | Double quote |
| `\\` | Backslash |
| `\n` | Newline |
| `\t` | Tab |
| `\xNN;` | Character by hexadecimal code point |

Strings are stored as arrays of Unicode scalar values (`System.Text.Rune`), making them code-point-aware.

### 1.5 Symbols

Symbols are interned identifiers. Examples: `foo`, `+`, `list->vector`, `|two words|`.

Bar notation (`|...|`) allows symbols containing spaces or other special characters.

### 1.6 Pairs and Lists

A pair is written as `(a . b)`. Proper lists are chains of pairs ending in `()`.

```scheme
(1 2 3)        → proper list of three elements
(1 . 2)        → improper pair
(1 2 . 3)      → improper list
```

The empty list is written as `()`.

### 1.7 Vectors

Written as `#(elem1 elem2 ...)`.

```scheme
#(1 2 3)            → vector of three integers
#(a #\b "c")        → mixed-content vector
```

### 1.8 Bytevectors

Written as `#u8(byte1 byte2 ...)`.

```scheme
#u8(0 10 255)       → bytevector of three bytes
```

### 1.9 Procedures and Special Forms

Procedures and special forms are first-class values. They are created by `lambda` and `syntax-rules` respectively.

### 1.10 Record Types

Records are created by `define-record-type`. They are distinct data types with named fields.

### 1.11 Promises

Promises are created by `delay` / `delay-force` and evaluated by `force`. They implement lazy evaluation.

### 1.12 Parameters

Parameters are dynamic binding containers created by `make-parameter`.

### 1.13 Continuations

First-class continuations are captured by `call-with-current-continuation` (`call/cc`).

### 1.14 Error Objects

Error objects are created by `raise` and `error`. They carry a message and irritants.

### 1.15 Unspecified Values

The `SUnspecified` value represents an unspecified return value. It is printed as `#<unspecified>`.

---

## 2. Literal Syntax

### 2.1 Comments

```scheme
; line comment

#| block comment |#
```

### 2.2 Boolean Literals

```scheme
#t #true          → true
#f #false         → false
```

### 2.3 Numeric Literals

```scheme
42                  ; decimal integer
-3/4                ; negative rational
#x1A                ; hexadecimal
#o777               ; octal
#b1010              ; binary
#e3.14              ; exact
#i1/2               ; inexact
1e6                 ; scientific notation
1+2i                ; rectangular complex
1@3.14              ; polar complex
+inf.0              ; positive infinity
-nan.0              ; NaN
```

### 2.4 Datum Labels

Circular and shared structure can be expressed with datum labels:

```scheme
(let ((x '(a b c)))
  (set-cdr! (cddr x) x)
  x)                ; prints as #0=(a b c . #0#)
```

---

## 3. Special Forms

### 3.1 `quote` / `'`

```scheme
(quote expr)
'expr
```

Returns the expression literally without evaluation.

### 3.2 `lambda`

```scheme
(lambda (args ...) body ...)
(lambda (args ... . rest) body ...)    ; variadic
(lambda args body ...)                 ; all arguments as a list
```

Creates a closure. The body is evaluated when the procedure is called.

### 3.3 `if`

```scheme
(if test consequent alternate)
(if test consequent)
```

Evaluates `test`; if truthy (not `#f`), evaluates `consequent`; otherwise evaluates `alternate` (if provided).

### 3.4 `set!`

```scheme
(set! variable expr)
```

Assigns a new value to an existing variable.

### 3.5 `cond`

```scheme
(cond
  (test expr ...)
  (else expr ...))           ; else clause

(cond
  (test => proc) ...)        ; => passes test result to proc
```

### 3.6 `case`

```scheme
(case expr
  ((key ...) expr ...)
  (else expr ...))
```

### 3.7 `and`, `or`

```scheme
(and test ...)               ; short-circuit AND
(or test ...)                ; short-circuit OR
```

### 3.8 `when`, `unless`

```scheme
(when test expr ...)         ; execute if truthy
(unless test expr ...)       ; execute if false
```

### 3.9 `begin`

```scheme
(begin expr ...)
```

Evaluates expressions sequentially, returning the last value.

### 3.10 `let`, `let*`, `letrec`, `letrec*`

```scheme
(let ((var val) ...) body ...)         ; parallel bindings
(let* ((var val) ...) body ...)        ; sequential bindings
(letrec ((var val) ...) body ...)      ; recursive bindings
(letrec* ((var val) ...) body ...)     ; sequential recursive bindings
```

### 3.11 `let-values`, `let*-values`

```scheme
(let-values (((var ...) expr) ...) body ...)    ; bind multiple values
(let*-values (((var ...) expr) ...) body ...)   ; sequential multiple values
```

### 3.12 `do`

```scheme
(do ((var init step ...) ...)
    (test result ...)
  expr ...)
```

Iteration construct. Variables are bound to `init`, updated with `step` each iteration. Loop terminates when `test` is truthy.

### 3.13 `delay`, `delay-force`, `force`

```scheme
(delay expr)                 ; create a lazy promise
(delay-force expr)           ; create a lazy promise (expr must return a promise)
(force promise)              ; evaluate a promise
```

### 3.14 `parameterize`

```scheme
(parameterize ((param value) ...) body ...)
```

Dynamically binds parameters for the duration of `body`.

### 3.15 `guard`

```scheme
(guard (var (condition expr ...) ...) body ...)
```

Exception handling with condition matching.

### 3.16 `quasiquote` / `` ` ``

```scheme
`template
`(,expr ,@expr)             ; unquote (,) and unquote-splicing (,@)
```

### 3.17 `case-lambda`

```scheme
(case-lambda
  ((args ...) body ...)
  ...)
```

Creates a procedure that dispatches on argument count.

### 3.18 `define`

```scheme
(define var expr)
(define (var args ...) body ...)        ; shorthand for lambda
(define (var args ... . rest) body ...) ; variadic shorthand
```

Defines a variable or procedure in the current environment.

### 3.19 `define-values`

```scheme
(define-values (var ...) expr)
```

Binds multiple values returned by `expr` to variables.

### 3.20 `define-record-type`

```scheme
(define-record-type name
  (constructor param ...)
  predicate
  (field accessor modifier?) ...)
```

R7RS record type definition.

### 3.21 `define-syntax`

```scheme
(define-syntax name syntax-rules ...)
```

Defines a hygienic macro.

### 3.22 `syntax-rules`

```scheme
(syntax-rules ()                      ; no custom ellipsis
  (pattern template) ...)

(syntax-rules (...)                   ; with literals
  (pattern template) ...)

(syntax-rules (ellipsis ...)          ; custom ellipsis symbol
  (pattern template) ...)
```

Hygienic macro patterns. Supports:

- Flexible ellipsis positions (`...`)
- Custom ellipsis symbols
- Pattern matching with `_` wildcard
- Template escaping

### 3.23 `syntax-error`

```scheme
(syntax-error "message")             ; signal a macro expansion error
(syntax-error "message" expr ...)    ; with irritants
```

### 3.24 `cond-expand`

```scheme
(cond-expand
  (feature expr ...)
  (else expr ...))
```

Feature-based conditional expansion at macro time.

### 3.25 `include`, `include-ci`

```scheme
(include "filename.scm")
(include-ci "filename.scm")          ; case-insensitive
```

Includes source files at expansion time.

### 3.26 `import`

```scheme
(import (library))
(import (prefix (library) 'prefix))
(import (only (library) name ...))
(import (except (library) name ...))
(import (rename (library) (old new) ...))
```

R7RS import forms with all standard set operators.

### 3.27 `define-library`

```scheme
(define-library (name ...)
  (export ...)
  (import ...)
  (begin ...)
  (include ...)
  ...)
```

R7RS library definition.

### 3.28 `let-syntax`, `letrec-syntax`

```scheme
(let-syntax ((name syntax-rules ...) ...) body ...)
(letrec-syntax ((name syntax-rules ...) ...) body ...)
```

Local macro bindings.

---

## 4. Built-in Procedures

### 4.1 Equivalence Predicates

| Procedure | Description |
|-----------|-------------|
| `(eqv? a b)` | Equivalent objects |
| `(eq? a b)` | Object identity |
| `(equal? a b)` | Structural equality |

### 4.2 Numeric Operations

#### Type Predicates

`number?`, `complex?`, `real?`, `rational?`, `integer?`, `exact?`, `inexact?`, `exact-integer?`, `finite?`, `infinite?`, `nan?`

#### Comparison

`=`, `<`, `>`, `<=`, `>=`, `zero?`, `positive?`, `negative?`, `odd?`, `even?`

#### Arithmetic

`+`, `-`, `*`, `/`, `abs`, `max`, `min`, `quotient`, `remainder`, `modulo`

#### Division Operations

`floor/`, `floor-quotient`, `floor-remainder`, `truncate/`, `truncate-quotient`, `truncate-remainder`

#### Number Theory

`gcd`, `lcm`, `numerator`, `denominator`, `rationalize`

#### Transcendental

`exp`, `log`, `sin`, `cos`, `tan`, `asin`, `acos`, `atan`, `sqrt`, `square`

#### Integer Functions

`exact-integer-sqrt`, `expt`

#### Complex

`make-rectangular`, `make-polar`, `real-part`, `imag-part`, `magnitude`, `angle`

#### Rounding

`floor`, `ceiling`, `truncate`, `round`

#### Conversion

`inexact`, `exact`, `number->string`, `string->number`

### 4.3 Booleans

`not`, `boolean?`, `boolean=?`

### 4.4 Pair and List Operations

#### Constructors / Selectors

`cons`, `car`, `cdr`, `caar`, `cadr`, `cdar`, `cddr`, `caaar` ... `cdddr`

#### Mutators

`set-car!`, `set-cdr!`

#### Predicates

`pair?`, `null?`, `list?`

#### List Utilities

`make-list`, `list`, `length`, `append`, `reverse`, `list-tail`, `list-ref`, `list-set!`, `list-copy`

#### Searching / Association

`memq`, `memv`, `member`, `assq`, `assv`, `assoc`

### 4.5 Symbol Operations

`symbol?`, `symbol=?`, `symbol->string`, `string->symbol`

### 4.6 Character Operations

#### Predicates

`char?`, `char-alphabetic?`, `char-numeric?`, `char-whitespace?`, `char-upper-case?`, `char-lower-case?`

#### Comparison

`char=?`, `char<?`, `char>?`, `char<=?`, `char>=?`, `char-ci=?`, `char-ci<?`, `char-ci>?`, `char-ci<=?`, `char-ci>=?`

#### Conversion

`char->integer`, `integer->char`, `char-upcase`, `char-downcase`, `char-foldcase`

#### Utility

`digit-value`

### 4.7 String Operations

#### Constructors

`make-string`, `string`

#### Predicates / Selectors

`string?`, `string-length`, `string-ref`

#### Mutators

`string-set!`, `string-copy!`, `string-fill!`

#### Comparison

`string=?`, `string<?`, `string>?`, `string<=?`, `string>=?`, `string-ci=?`, `string-ci<?`, `string-ci>?`, `string-ci<=?`, `string-ci>=?`

#### Conversion / Transformation

`string-upcase`, `string-downcase`, `string-foldcase`, `substring`, `string-append`, `string->list`, `list->string`, `string-copy`

### 4.8 Vector Operations

`vector?`, `make-vector`, `vector`, `vector-length`, `vector-ref`, `vector-set!`, `vector->list`, `list->vector`, `vector->string`, `string->vector`, `vector-copy`, `vector-copy!`, `vector-append`, `vector-fill!`

### 4.9 Bytevector Operations

`bytevector?`, `make-bytevector`, `bytevector`, `bytevector-length`, `bytevector-u8-ref`, `bytevector-u8-set!`, `bytevector-copy`, `bytevector-copy!`, `bytevector-append`, `utf8->string`, `string->utf8`

### 4.10 Higher-Order Functions

| Procedure | Description |
|-----------|-------------|
| `(procedure? obj)` | Is `obj` a procedure? |
| `(apply proc arg ... args)` | Apply with last argument as list |
| `(map proc list ...)` | Map over lists |
| `(string-map proc string ...)` | Map over strings |
| `(vector-map proc vector ...)` | Map over vectors |
| `(for-each proc list ...)` | Apply for side effects |
| `(string-for-each proc string ...)` | Apply over strings |
| `(vector-for-each proc vector ...)` | Apply over vectors |

### 4.11 Continuations and Control Flow

| Procedure | Description |
|-----------|-------------|
| `(call-with-current-continuation proc)` | Capture current continuation (aliased as `call/cc`) |
| `(values obj ...)` | Return multiple values |
| `(call-with-values producer consumer)` | Capture and use multiple values |
| `(dynamic-wind before thunk after)` | Guarded execution with winders |

### 4.12 Exception Handling

| Procedure | Description |
|-----------|-------------|
| `(with-exception-handler handler thunk)` | Install an exception handler |
| `(raise obj)` | Raise a non-continuable exception |
| `(raise-continuable obj)` | Raise a continuable exception |
| `(error message obj ...)` | Signal an error |
| `(error-object? obj)` | Is `obj` an error object? |
| `(error-object-message err)` | Get error message |
| `(error-object-irritants err)` | Get error irritants |

### 4.13 Lazy Evaluation

| Procedure | Description |
|-----------|-------------|
| `(delay expr)` | Create a promise |
| `(delay-force expr)` | Create a promise (expr returns a promise) |
| `(force promise)` | Evaluate and cache promise |
| `(promise? obj)` | Is `obj` a promise? |
| `(make-promise obj)` | Create a promise from a value |

### 4.14 Parameters

| Procedure | Description |
|-----------|-------------|
| `(make-parameter init)` | Create a parameter |
| `(make-parameter init converter)` | Create with converter |

### 4.15 I/O

| Procedure | Description |
|-----------|-------------|
| `(display obj)` | Print an object |
| `(load filename)` | Load and evaluate a file |

---

## 5. Macros

The macro system implements R7RS `syntax-rules` with full hygiene.

### 5.1 Basic Macro Definition

```scheme
(define-syntax my-macro
  (syntax-rules ()
    ((my-macro pattern)
     template)))
```

### 5.2 Macro with Literals

```scheme
(define-syntax my-macro
  (syntax-rules (literal ...)
    ((my-macro pattern)
     template)))
```

### 5.3 Supported Features

- **Hygiene**: Automatic renaming of bound variables prevents unintended capture.
- **Flexible ellipsis**: `...` can appear in various pattern positions.
- **Custom ellipsis**: A different ellipsis symbol can be specified.
- **Template escaping**: Templates can escape the pattern variable substitution.
- **`_` wildcard**: Underscore matches any pattern position.
- **`syntax-error`**: Macros can report errors at expansion time.

---

## 6. Libraries

R7RS library system with `define-library` and `import`.

### 6.1 Library Definition

```scheme
(define-library (example lib)
  (export public-proc)
  (import (scheme base))
  (begin
    (define (public-proc x)
      (+ x 1))))
```

### 6.2 Import Forms

```scheme
(import (scheme base))                          ; full import
(import (only (scheme base) car cdr))          ; selective import
(import (except (scheme base) set!))           ; import with exclusion
(import (prefix (scheme base) 'base:))         ; prefix all names
(import (rename (scheme base) (car first)))    ; rename on import
```

---

## 7. Known Limitations

- **I/O**: File I/O procedures beyond `load` and `display` are not yet fully implemented.
- **Full R7RS conformance**: Some edge cases of the specification are still being addressed.
