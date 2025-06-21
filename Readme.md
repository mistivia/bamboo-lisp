# Bamboo Lisp

Embeddable & Hackable Lisp-2 Interpreter

## WARNING

This project is still a toy project in very early stage. Don't use it in production!

## Features & Drawbacks

- Lisp-2 (more like Common Lisp or Emacs Lisp)
- Lexical scoping
- < 3000 LOC 
- Tail call optimization
- Any C99 compiler should work
- Depend only on standard library
- A little bit slow (trade-off for simplicity)
- A simple mark-sweep GC
- Writing macro is easy with quasiquote, unquote, and slicing-unquote
- No global state, you can run multiple interpreters in multiple threads
- Support C-like control flow statements
    - return
    - break
    - continue

## Build

Init submodule:

```bash
git submodule init --recursive
```

Debug:

```bash
git submodule init --recursive
make
```

Release:

```bash
make clean
make mode=release
```

## Usage

After building, you can run the Bamboo Lisp interpreter using:

```bash
./bamboo-lisp # To enter the REPL (if applicable)
./bamboo-lisp <filename.lisp> # To run a Lisp file
```

You can use `load` to load a lisp script into the interpreter:

```lisp
(load "my-script.lisp")
```

## Example

See `tests/` for more examples.

### 1. Y Combinator

```lisp
(defun Y (f)
  (funcall
    (lambda (g) (funcall g g))
    (lambda (h)
       (funcall f (lambda args (apply (funcall h h) args))))))

(defun fibo-impl (self)
  (lambda (n)
    (if (<= n 2)
        1
        (+ (funcall self (- n 1)) (funcall self (- n 2))))))

(defvar fibo (Y #'fibo-impl))

(funcall fibo 10)
;; Expected output: 55
```

### 2. Macro

```lisp
(defmacro inc (x)
  `(setq ,x (+ ,x 1)))

(defmacro for (start pred inc . body)
  `(let (,start)
     (while ,pred
       ,@body
       ,inc)))

(for (i 0) (< i 10) (inc i)
  (princ "meow\n"))

;; Expected output:
;; meow
;; meow
;; ... (10 times)
```

