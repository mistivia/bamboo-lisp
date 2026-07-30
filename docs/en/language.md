# Language Reference

## Reader syntax

| Syntax | Meaning |
| ------ | ------- |
| `; ...` | Line comment (to end of line) |
| `42`, `-7` | Integer |
| `3.14`, `1e-3` | Real (floating point) |
| `"hello\n"` | String (supports `\n`, `\t`, `\"`, `\\`, …) |
| `#\a`, `#\newline`, `#\space`, `#\tab`, `#\return` | Character |
| `#\(`, `#\)`, `#\;`, `#\#`, `#\ ` | Character (delimiters name themselves) |
| `#t`, `#f` | Boolean true / false |
| `foo`, `list->vec`, `+`, `char<=` | Symbol |
| `(a b c)` | List / call |
| `(a . b)` | Dotted pair |
| `'x` | `(quote x)` |
| `` `x `` | `(quasiquote x)` |
| `,x` | `(unquote x)` |
| `,@x` | `(slicing-unquote x)` — splice inside quasiquote |
| `#'f` | `(function f)` — the function value of symbol `f` |

## Data types

Integers, reals, booleans, characters, strings, symbols, the empty list `()`
(also written `nil`), pairs (cons cells), functions (lambdas / built-ins),
macros, and opaque user data (used by the vector / dict / io modules).

`nil` is the empty list and the only false-ish list value. Truthiness: `#f` and
`nil` are false; everything else (including `0` and `""`) is true.

## Lisp-2: functions vs. variables

A symbol can name a function and a variable at the same time. In *call
position* (head of a list), a symbol is looked up as a function/macro; elsewhere
it is looked up as a variable.

```lisp
(defvar list 10)        ; variable `list` = 10
(list list list)        ; => (10 10)   ; `list` the function, `list` the variable
```

To pass a function as a value use `#'name` or `(function name)`, and to call a
function value use `funcall` (or `apply`):

```lisp
(funcall #'+ 1 2 3)     ; => 6
(apply #'+ '(1 2 3))    ; => 6
(map #'car '((1 2) (3 4)))  ; => (1 3)
```

## Definitions

```lisp
(defvar name value)             ; define/redefine a global variable
(defun name (params...) body...); define a global function
(defun name value)              ; bind a name to an existing function value
(defmacro name (params...) body...) ; define a macro
```

Parameter lists may use a dotted tail to capture the rest of the arguments:

```lisp
(defun list* (first . rest) (cons first rest))
(defun k () 42)                 ; zero parameters
```

`defun` / `defvar` / `defmacro` may only appear at top level.

## Special forms

- `(quote x)` / `'x` — return `x` unevaluated.
- `(if test then else?)` — `else` is optional (defaults to `nil`).
- `(cond (test expr) ...)` — first clause whose `test` is true; each clause is
  exactly `(test expr)`.
- `(when test body...)`, `(unless test body...)` — prelude macros.
- `(and e...)`, `(or e...)` — short-circuiting; return the deciding value.
- `(progn body...)` — evaluate in sequence, return the last.
- `(let ((name init) ...) body...)` — bindings are *sequential*: a later
  `init` can refer to an earlier binding in the same `let`.
- `(setq name value)` — assign to an existing (lexical or global) variable.
- `(lambda (params...) body...)` — a lexical closure. `(lambda args body...)`
  with a symbol parameter captures all arguments as a list.
- `(function f)` / `#'f` — the function value of `f`.
- `(funcall fn args...)`, `(apply fn args-list)` — call a function value.
- `(while test body...)` — loop.
- `(return v)`, `(break)`, `(continue)` — C-like control flow inside
  functions / loops.
- `(quasiquote t)` / `` `t `` with `,x` and `,@x` — template construction.
- `(eval form)`, `(macroexpand-1 form)`.
- `(load "file.lisp")` — read and evaluate a file at top level.

### Exceptions

```lisp
(try EXPR CATCH)        ; run EXPR; if it throws, call (CATCH value)
(throw value)           ; raise an exception
(unwind-protect EXPR CLEANUP)  ; always run CLEANUP after EXPR

(assert EXPR)            ; error unless EXPR is true
(assert-error EXPR)      ; assert that EXPR raises an error
(assert-exception EXPR)  ; assert that EXPR throws an exception
```

Runtime errors carry a message and a stack trace, printed when they reach top
level.

## Control flow example

```lisp
(defun first-even (lst)
  (while (not (null? lst))
    (when (zero? (mod (car lst) 2))
      (return (car lst)))
    (setq lst (cdr lst)))
  nil)
```

## Macros

Macros receive their arguments unevaluated and return a new form, which is then
evaluated. Build the result with quasiquote:

```lisp
(defmacro incq (i) `(setq ,i (+ ,i 1)))

(defmacro for (start pred inc . body)
  `(let (,start)
     (while ,pred
       ,@body
       ,inc)))

(let ((sum 0))
  (for (i 0) (< i 5) (incq i)
    (setq sum (+ sum i)))
  sum)                      ; => 10
```

Use `gensym` to create fresh symbols and avoid variable capture. Note that macro
expansions are cached per function definition, so a `gensym` in a macro is
evaluated once per use-site (fresh symbol per call site, stable across calls of
the enclosing function) — which is exactly what you want for hygiene.

`(macroexpand-1 '(when x y))` returns the one-step expansion, handy for
debugging.

## `pcase` — pattern matching

`pcase` is a prelude macro providing ML-style pattern matching (a subset of
Emacs Lisp's `pcase`):

```lisp
(pcase EXPR
  (PATTERN body...)
  ...)
```

`EXPR` is evaluated once; clauses are tried in order; the body of the first
matching pattern runs with the pattern's variables bound. If nothing matches the
result is `nil`.

### Patterns

| Pattern | Matches |
| ------- | ------- |
| `_` | anything (no binding) |
| `x` | anything, binding `x` to the value |
| `nil` | the empty list |
| `42`, `"s"`, `#\c`, `#t` | a literal, compared with `equal?` |
| `'value` | a value `equal?` to `value` (e.g. a literal symbol) |
| `(pred FN)` | when `(FN value)` is true |
| `(guard EXPR)` | when `EXPR` (using already-bound vars) is true |
| `(and PAT...)` | when every sub-pattern matches |
| `(or PAT...)` | when some sub-pattern matches |
| `` `TEMPLATE `` | structural match; `,PAT` matches/binds a position |

Backquote patterns destructure lists: `` `(,a ,b) `` matches a two-element list
binding `a` and `b`; `` `(tag ,x) `` matches a list whose head is the literal
symbol `tag`; `` `(,head . ,tail) `` splits a cons into head and tail.

```lisp
(defun describe (x)
  (pcase x
    (0 "zero")
    ((pred integer?) "some integer")
    ((and n (guard (> n 0))) "positive non-integer")   ; won't fire here
    ('hello "a greeting symbol")
    (`(,a ,b) (format "pair of %s and %s" a b))
    (`(,h . ,t) (format "list starting with %s" h))
    (_ "something else")))
```
