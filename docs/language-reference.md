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

Written as `#\` followed by the character or a named character.

| Example | Meaning |
|---------|---------|
| `#\a` | The character `a` |
| `#\x3071` | Character by Unicode code point (`ぱ`) |
| `#\alarm` | Bell (`U+0007`) |
| `#\backspace` | Backspace (`U+0008`) |
| `#\delete` | Delete (`U+007F`) |
| `#\escape` | Escape (`U+001B`) |
| `#\newline` | Newline (`U+000A`) |
| `#\null` | Null (`U+0000`) |
| `#\return` | Carriage return (`U+000D`) |
| `#\space` | Space (`U+0020`) |
| `#\tab` | Tab (`U+0009`) |

### 1.4 Strings

Written in double quotes. Supports escape sequences:

| Escape | Meaning |
|--------|---------|
| `\"` | Double quote |
| `\\` | Backslash |
| `\|` | Vertical bar (for use inside strings) |
| `\a` | Bell (`U+0007`) |
| `\b` | Backspace (`U+0008`) |
| `\t` | Tab (`U+0009`) |
| `\n` | Newline (`U+000A`) |
| `\v` | Vertical tab (`U+000B`) |
| `\f` | Form feed (`U+000C`) |
| `\r` | Carriage return (`U+000D`) |
| `\x<hex>;` | Unicode code point (hexadecimal) |
| `\<newline>` | Line continuation (newline and following intraline whitespace are ignored) |

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

### 1.9 End-of-File Object

The end-of-file object is returned by input operations when no more data is available. It is printed and written literally as `#!eof`. Test it with `eof-object?`.

---

## 2. Literal Syntax

### 2.1 Comments

```scheme
; line comment

#| block comment |#

#; datum comment (discards the following datum)
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

### 3.13 `delay`, `delay-force`

```scheme
(delay expr)                 ; create a lazy promise
(delay-force expr)           ; create a lazy promise (expr must return a promise)
```

`force` is a procedure (see [4.13 Lazy Evaluation](#413-lazy-evaluation)), not a special form.

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
| `(eqv? a b)` | Equivalent objects (uses physical equality for pairs, vectors, bytevectors, continuations, and procedures) |
| `(eq? a b)` | Object identity (current alias for `eqv?` in this implementation) |
| `(equal? a b)` | Structural equality (recursively compares pairs and vectors) |

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

`inexact`, `exact`, `exact->inexact`, `inexact->exact`, `number->string`, `string->number`

### 4.3 Booleans

`not`, `boolean?`, `boolean=?`

### 4.4 Pair and List Operations

#### Constructors / Selectors

`cons`, `car`, `cdr`, `caar`, `cadr`, `cdar`, `cddr`

The following 24 car/cdr compositions (3-level and 4-level) are available from the `(scheme cxr)` library:
`caaar`, `caadr`, `cadar`, `caddr`, `cdaar`, `cdadr`, `cddar`, `cdddr`,
`caaaar`, `caaadr`, `caadar`, `caaddr`, `cadaar`, `cadadr`, `caddar`, `cadddr`,
`cdaaar`, `cdaadr`, `cdadar`, `cdaddr`, `cddaar`, `cddadr`, `cdddar`, `cddddr`

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
| `(read-error? obj)` | Is `obj` a read error (raised by `read` on parse failure)? |
| `(file-error? obj)` | Is `obj` a file error (raised by file I/O operations)? |

### 4.13 Lazy Evaluation

| Procedure | Description |
|-----------|-------------|
| `(force promise)` | Evaluate and cache promise |
| `(promise? obj)` | Is `obj` a promise? |
| `(make-promise obj)` | Create a promise from a value |

### 4.14 Parameters

| Procedure | Description |
|-----------|-------------|
| `(make-parameter init)` | Create a parameter |
| `(make-parameter init converter)` | Create with converter |

### 4.15 I/O

#### Port Predicates

| Procedure | Description |
|-----------|-------------|
| `(port? x)` | Returns `#t` if x is a port |
| `(input-port? x)` | Returns `#t` if x is an input port |
| `(output-port? x)` | Returns `#t` if x is an output port |
| `(textual-port? x)` | Returns `#t` if x is a textual port |
| `(binary-port? x)` | Returns `#t` if x is a binary port |
| `(input-port-open? x)` | Returns `#t` if x is an open input port |
| `(output-port-open? x)` | Returns `#t` if x is an open output port |

#### Current Ports

| Procedure | Description |
|-----------|-------------|
| `(current-input-port)` | Returns the current input port |
| `(current-output-port)` | Returns the current output port |
| `(current-error-port)` | Returns the current error port |

#### String Ports

| Procedure | Description |
|-----------|-------------|
| `(open-input-string string)` | Creates a textual input port from a string |
| `(open-output-string)` | Creates a textual output port |
| `(get-output-string port)` | Returns the accumulated output string |

#### Bytevector Ports

| Procedure | Description |
|-----------|-------------|
| `(open-input-bytevector bytevector)` | Creates a binary input port from a bytevector |
| `(open-output-bytevector)` | Creates a binary output port |
| `(get-output-bytevector port)` | Returns the accumulated output bytevector |

#### Input Operations

| Procedure | Description |
|-----------|-------------|
| `(read)` | Read a datum from the current input port |
| `(read port)` | Read a datum from a specific port |
| `(read-char)` | Read a character from the current input port |
| `(read-char port)` | Read a character from a specific port |
| `(peek-char)` | Peek the next character without consuming it |
| `(peek-char port)` | Peek from a specific port |
| `(read-line)` | Read a line from the current input port |
| `(read-line port)` | Read a line from a specific port |
| `(read-string k)` | Read up to k characters |
| `(read-string k port)` | Read up to k characters from a specific port |
| `(read-u8)` | Read a byte from the current binary input port |
| `(read-u8 port)` | Read a byte from a specific binary port |
| `(peek-u8)` | Peek the next byte without consuming it |
| `(peek-u8 port)` | Peek from a specific port |
| `(read-bytevector k)` | Read up to k bytes |
| `(read-bytevector k port)` | Read up to k bytes from a specific port |
| `(read-bytevector! bytevector)` | Read bytes into a bytevector |
| `(read-bytevector! bytevector port)` | Read bytes into a bytevector from a specific port |
| `(char-ready?)` | Returns `#t` if a character is ready (always returns `#t` in this implementation) |
| `(char-ready? port)` | Returns `#t` if a character is ready on port (always returns `#t` in this implementation) |
| `(u8-ready?)` | Returns `#t` if a byte is ready (always returns `#t` in this implementation) |
| `(u8-ready? port)` | Returns `#t` if a byte is ready on port (always returns `#t` in this implementation) |

#### Output Operations

| Procedure | Description |
|-----------|-------------|
| `(write obj)` | Write an object in machine-readable form |
| `(write-shared obj)` | Write an object with shared structure notation |
| `(write-simple obj)` | Write an object without shared structure notation |
| `(display obj)` | Print an object for human consumption |
| `(write-char char)` | Write a character |
| `(write-char char port)` | Write a character to a specific port |
| `(write-string string)` | Write a string |
| `(write-string string port)` | Write a string to a specific port |
| `(write-u8 byte)` | Write a byte |
| `(write-u8 byte port)` | Write a byte to a specific port |
| `(write-bytevector bytevector)` | Write a bytevector |
| `(write-bytevector bytevector port)` | Write a bytevector to a specific port |
| `(newline)` | Write a newline |
| `(newline port)` | Write a newline to a specific port |
| `(flush-output-port)` | Flush the output port |
| `(flush-output-port port)` | Flush a specific port |

#### File Ports

| Procedure | Description |
|-----------|-------------|
| `(open-input-file filename)` | Open a file for textual input |
| `(open-binary-input-file filename)` | Open a file for binary input |
| `(open-output-file filename)` | Open a file for textual output |
| `(open-binary-output-file filename)` | Open a file for binary output |
| `(close-input-port port)` | Close an input port |
| `(close-output-port port)` | Close an output port |
| `(close-port port)` | Close a port |
| `(call-with-input-file filename proc)` | Open a file, call proc with it, close |
| `(call-with-output-file filename proc)` | Open a file, call proc with it, close |
| `(call-with-port port proc)` | Call proc with port, then close it |
| `(with-input-from-file filename thunk)` | Set current input port to file, call thunk, restore |
| `(with-output-to-file filename thunk)` | Set current output port to file, call thunk, restore |

#### File System Operations

| Procedure | Description |
|-----------|-------------|
| `(file-exists? filename)` | Returns `#t` if the file exists |
| `(delete-file filename)` | Delete a file; raises `file-error` if not found |

#### Special Values

| Procedure | Description |
|-----------|-------------|
| `(eof-object)` | Returns the end-of-file object |
| `(eof-object? x)` | Returns `#t` if x is the EOF object |
| `#!eof` | Literal EOF object syntax |

### 4.16 Load and System Interface

| Procedure | Description |
|-----------|-------------|
| `(load filename)` | Load and evaluate a file |
| `(load filename environment)` | Load and evaluate a file in the given environment |
| `(command-line)` | Returns the command-line arguments as a list of strings |
| `(exit obj ...)` | Run dynamic-wind `after` thunks, then exit |
| `(emergency-exit obj ...)` | Exit immediately without running winders |
| `(get-environment-variable name)` | Returns the value of an environment variable, or `#f` |
| `(get-environment-variables)` | Returns an alist of all environment variables |
| `(current-second)` | Returns the current time in seconds since the Unix epoch (inexact real) |
| `(current-jiffy)` | Returns the current time in jiffies (exact integer; high-resolution monotonic counter) |
| `(jiffies-per-second)` | Returns the number of jiffies per second (exact integer) |
| `(features)` | Returns a list of symbols naming the supported feature identifiers |

#### Environment and Evaluation

| Procedure | Description |
|-----------|-------------|
| `(environment import-spec ...)` | Creates a new environment populated with the specified library imports |
| `(eval expr env)` | Evaluates `expr` in the given `env` |
| `(interaction-environment)` | Returns the REPL's current environment (all bindings) |
| `(null-environment version)` | Returns an empty environment (only version 5 is supported) |
| `(scheme-report-environment version)` | Returns the `(scheme r5rs)` library environment (only version 5 is supported) |

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
(import (scheme base))                         ; full import
(import (only (scheme base) car cdr))          ; selective import
(import (except (scheme base) set!))           ; import with exclusion
(import (prefix (scheme base) 'base:))         ; prefix all names
(import (rename (scheme base) (car first)))    ; rename on import
```

### 6.3 Built-in Libraries

The interpreter registers the following libraries in `Builtin.builtinContext` (bindings still live in a single global environment; library separation is achieved through export filtering at registration time):

| Library | Exports |
|---------|---------|
| `(scheme base)` | Core R7RS procedures: arithmetic, pairs, strings, vectors, bytevectors, symbols, characters, I/O primitives, control, exceptions, ports (excluding procedures delegated to other libraries below) |
| `(scheme case-lambda)` | `case-lambda` |
| `(scheme cxr)` | 24 car/cdr compositions: `caaaar`, `caaadr`, `caaar`, `caadar`, `caaddr`, `caadr`, `cadaar`, `cadadr`, `cadar`, `caddar`, `cadddr`, `caddr`, `cdaaar`, `cdaadr`, `cdaar`, `cdadar`, `cdaddr`, `cdadr`, `cddaar`, `cddadr`, `cddar`, `cdddar`, `cddddr`, `cdddr` |
| `(scheme char)` | `char-alphabetic?`, `char-ci=?`, `char-ci<?`, `char-ci>?`, `char-ci<=?`, `char-ci>=?`, `char-downcase`, `char-foldcase`, `char-lower-case?`, `char-numeric?`, `char-upcase`, `char-upper-case?`, `char-whitespace?`, `digit-value`, `string-ci=?`, `string-ci<?`, `string-ci>?`, `string-ci<=?`, `string-ci>=?`, `string-downcase`, `string-foldcase`, `string-upcase` |
| `(scheme complex)` | `make-rectangular`, `make-polar`, `real-part`, `imag-part`, `magnitude`, `angle` |
| `(scheme eval)` | `environment`, `eval` |
| `(scheme file)` | `call-with-input-file`, `call-with-output-file`, `with-input-from-file`, `with-output-to-file`, `open-input-file`, `open-output-file`, `open-binary-input-file`, `open-binary-output-file`, `file-exists?`, `delete-file` |
| `(scheme inexact)` | `finite?`, `infinite?`, `nan?`, `exp`, `log`, `sin`, `cos`, `tan`, `asin`, `acos`, `atan`, `sqrt` |
| `(scheme lazy)` | `delay`, `delay-force`, `force`, `promise?`, `make-promise` |
| `(scheme load)` | `load` |
| `(scheme process-context)` | `command-line`, `exit`, `emergency-exit`, `get-environment-variable`, `get-environment-variables` |
| `(scheme read)` | `read` |
| `(scheme repl)` | `interaction-environment` |
| `(scheme r5rs)` | All `(scheme base)` exports plus `exact->inexact`, `inexact->exact`, `null-environment`, `scheme-report-environment` |
| `(scheme time)` | `current-second`, `current-jiffy`, `jiffies-per-second` |
| `(scheme write)` | `write`, `write-shared`, `write-simple`, `display` |

### 6.4 Feature Identifiers

The `features` procedure and `cond-expand` recognize the following feature identifiers (the static identifiers below are always reported; the dynamic ones are determined at runtime from the host platform):

| Id | Always / Dynamic | Meaning |
|----|-----------|---------|
| `r7rs` | Always | Conformance to R7RS |
| `exact-closed` | Always | Exact arithmetic is closed under basic operations |
| `exact-rational` | Always | Exact rational arithmetic is supported |
| `ieee-float` | Always | IEEE 754 floating-point |
| `full-unicode` | Always | Full Unicode scalar value support |
| `ratios` | Always | Native ratio (rational) type |
| `windows` | Dynamic | Running on Windows (`OperatingSystem.IsWindows()`) |
| `linux` | Dynamic | Running on Linux (`OperatingSystem.IsLinux()`) |
| `unix` | Dynamic | POSIX-family OS (reported together with `linux`) |
| `posix` | Dynamic | POSIX-family OS (reported together with `linux`) |
| `little-endian` | Dynamic | Host byte order is little-endian (`BitConverter.IsLittleEndian`) |
| `big-endian` | Dynamic | Host byte order is big-endian |
| `x86-64` | Dynamic | `RuntimeInformation.ProcessArchitecture = X64` |
| `arm64` | Dynamic | `RuntimeInformation.ProcessArchitecture = Arm64` |

---

## 7. Known Limitations

Most R7RS (Small) features are implemented. Known deviations and simplifications:

- **`char-ready?` / `u8-ready?`**: Always return `#t`. R7RS permits this when readiness cannot be determined, so it is conformant, but the result does not reflect actual port state.
- **`string-upcase` / `string-downcase` / `string-foldcase` (and character variants)**: Use `ToUpperInvariant` / `ToLowerInvariant`, which is culture-invariant ASCII-safe but differs from full Unicode Default Case Conversion in some edge cases (e.g., Turkish dotted/dotless `i`).
- **`inexact->exact` on reals**: `realToRational` formats the double with `"%.17g"` before parsing, which is sufficient for round-trip-printability of IEEE 754 doubles but is not the formal rational representation required for arbitrary floats.
- **`eqv?` on signed zeros**: `(eqv? +0.0 -0.0)` returns `#t` (matches R7RS `=` semantics).
- **Block comments and datum labels**: Standard R7RS syntax is supported; exotic reader edge cases (e.g., interleaving with `#;` datum comments and nested labels in unusual positions) may not all be exercised.
- **Tail-call guarantees in the printer / macros**: The evaluator, `evalArgs`, and `loopListInfo` are explicitly `[<TailCall>]`; CPS helpers elsewhere rely on tail-position discipline but are not uniformly annotated.

For full details and reproduction cases, see [Recurring Gotchas](gotchas.md).
