# Bamboo Lisp — Documentation (English)

Bamboo Lisp is a small, embeddable, hackable **Lisp-2** interpreter. Functions
and variables live in separate namespaces (like Common Lisp / Emacs Lisp). It
offers lexical scoping, tail-call optimization, a mark-sweep GC, exceptions with
`try`, C-like control flow (`return` / `break` / `continue`), and an easy macro
system built on quasiquote.

## Contents

- [Language Reference](language.md) — syntax, data types, special forms, macros,
  and `pcase` pattern matching.
- [Standard Library](stdlib.md) — built-in functions, the prelude, and the
  vector / dict / io modules.

中文文档见 [`../zh/`](../zh/README.md)。

## Getting started

Build and run:

```sh
make
./bamboo-lisp            # REPL
./bamboo-lisp file.lisp  # run a script
```

Load a script from within the interpreter:

```lisp
(load "my-script.lisp")
```

## A taste

```lisp
;; Lisp-2: use #'f / (function f) to get a function value, funcall to call it
(defun square (x) (* x x))
(princ (format "%s\n" (map #'square '(1 2 3 4))))   ; => (1 4 9 16)

;; Macros with quasiquote
(defmacro unless (test . body)
  `(if ,test nil (progn ,@body)))

;; Pattern matching
(defun eval-expr (e)
  (pcase e
    ((pred integer?) e)
    (`(+ ,a ,b) (+ (eval-expr a) (eval-expr b)))
    (`(* ,a ,b) (* (eval-expr a) (eval-expr b)))))
(princ (format "%s\n" (eval-expr '(+ 1 (* 2 3)))))   ; => 7
```
