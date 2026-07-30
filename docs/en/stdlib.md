# Standard Library

This lists the built-in functions plus the prelude (functions/macros written in
Lisp) and the bundled modules. Functions are called `(name args...)`; unless
noted otherwise they evaluate all their arguments.

## Arithmetic

- `(+ ...)`, `(- ...)`, `(* ...)`, `(/ ...)` — sum, difference, product,
  division. `/` yields a real when the result isn't integral.
- `(i/ a b)` — integer division (truncates toward zero).
- `(mod a b)` — floored modulo: the result takes the sign of the divisor, so
  `(mod -3 100)` is `97` (as in Common Lisp / Emacs Lisp).
- `(rem a b)` — truncating remainder: the result takes the sign of the dividend,
  so `(rem -3 100)` is `-3` (as in C).
- Dividing by zero is an error, not a crash.
- `(abs x)`, `(min ...)`, `(max ...)`.
- `(floor x)`, `(ceiling x)`, `(round x)`, `(truncate x)`.
- `(expt b e)` / `(pow b e)`, `(sqrt x)`, `(cbrt x)`, `(exp x)`, `(ln x)`,
  `(log2 x)`, `(log10 x)`.
- Trigonometry: `(sin x)` `(cos x)` `(tan x)` `(asin x)` `(acos x)` `(atan x)`.
- `(float x)` — convert to real.

## Comparison and logic

- Numeric: `(= ...)`, `(/= ...)`, `(< ...)`, `(> ...)`, `(<= ...)`, `(>= ...)`.
- `(not x)` — logical negation (`and` / `or` are special forms).
- `(eq? a b)` — identity; `(equal? a b)` — structural equality.

## Predicates

`null?`, `cons?`, `list?`, `atom?`, `symbol?`, `string?`, `char?`, `number?`,
`integer?`, `float?`, `function?`, `member?` (`(member? x lst)`), and the
prelude helpers `zero?`, `plus?`, `minus?`, `contains?` (`(contains? x lst)`).

## Pairs and lists

- `(cons a b)`, `(car p)`, `(cdr p)`, `(set-car p v)`, `(set-cdr p v)`.
- All `c[ad]+r` accessors up to four letters: `caar`, `cadr`, `caddr`,
  `cddddr`, … (prelude).
- `(list ...)`, `(length lst)`, `(nth n lst)`, `(nthcdr n lst)`,
  `(set-nth n lst v)`, `(set-nthcdr n lst v)`, `(last lst)`.
- `(reverse lst)`, `(nreverse lst)` (destructive), `(append ...)`,
  `(nconc ...)` (destructive).
- Higher order: `(map f lst)`, `(filter pred lst)` (keeps the elements the
  predicate accepts), `(remove pred lst)` (keeps the others),
  `(count pred lst)`, `(foreach f lst)`, `(foldl f init lst)`.
- Prelude: `(find x lst)`, `(take n lst)`, `(drop n lst)`,
  `(take-while pred lst)`, `(drop-while pred lst)`, `(sublist start end lst)`,
  `(sort lst pred)` — stable merge sort, `pred` being a "strictly less"
  predicate, and `(merge-sorted a b pred)` for two already sorted lists.
- Prelude conversions: `(list->vector lst)`, `(vector->list vec)`.

## Strings

- `(string ...)`, `(concat ...)` — build/concatenate strings.
- `(format fmt args...)` — return a formatted string (`%s` inserts an argument).
- `(print x)` — write a machine-readable form (strings quoted); `(princ x)` —
  write a display form (strings unquoted). Both go to standard output.
- Comparison: `string=`, `string/=`, `string<`, `string>`, `string<=`,
  `string>=`.
- `(split-string s sep)`, `(strip-string s)`.
- `(string-length s)`, `(string-ref s i)` — the `i`th character.
- `(substring s start)` / `(substring s start end)` — `end` defaults to the end
  of the string; `start` and `end` must be within it.
- `(string->list s)`, `(list->string chars)`.
- `(string->number s)` — an integer or a real, or `nil` when `s` (ignoring
  surrounding blanks) is not a number; `(number->string n)`.

## Characters

- Comparison: `char=`, `char/=`, `char<`, `char>`, `char<=`, `char>=`.
- `(int->char n)`, `(char->int c)`.
- `(alphabetic? c)`, `(numeric? c)`, `(alphanum? c)`.

## Symbols

- `(gensym)` — a fresh, unique symbol (for macro hygiene).
- `(intern s)` — the symbol named by string `s`.
- `(symbol->string sym)`.

## Bitwise

`(logand ...)`, `(logior ...)`, `(logxor ...)`, `(lognot x)`,
`(lsh x n)` (logical shift), `(ash x n)` (arithmetic shift).

## Misc

- `(eval form)`, `(apply fn args-list)`, `(funcall fn args...)`.
- `(error fmt args...)` — raise an error; `(throw v)` — raise an exception.
- `(exit)` — quit the interpreter.
- `(_gcstat)`, `(_alwaysgc bool)` — GC diagnostics (used by the test suite).

## Prelude macros

- `(when test body...)`, `(unless test body...)`.
- `(incq place)`, `(decq place)` — increment / decrement in place.
- `(dolist (x lst) body...)`, `(dotimes (i n) body...)` — loops. The cursor and
  the counter are advanced before `body` runs, so `break` and `continue` behave
  as they do in a plain `while`.
- `(pcase ...)` — pattern matching (see the [Language Reference](language.md)).

---

# Bundled modules

These are registered at startup (no separate loading step needed).

## Mutable vectors (`vector.c`)

- `(make-vector)` / `(make-vector size)` / `(make-vector size fill)`,
  `(vector? x)`.
- `(vector-length v)`, `(vector-ref v i)`, `(vector-set v i x)`.
- `(vector-append v x)`, `(vector-insert v i x)`, `(vector-remove v i)`.

## Dictionaries (`dict.c`)

Keys are strings.

- `(make-dict)`, `(dict? x)`.
- `(dict-get d key)`, `(dict-set d key value)`, `(dict-remove d key)`,
  `(dict-keys d)`.

## I/O streams (`io.c`)

- `(open-file path mode)`, `(stream? x)`, `(stream-close s)`.
- Reading: `(read-char s)`, `(read-line s)`, `(read-integer s)`,
  `(read-number s)`, `(lines s)`. The stream argument may be omitted, in which
  case these read standard input.
- Writing: `(write-char s c)`, `(write-obj s x)`.
