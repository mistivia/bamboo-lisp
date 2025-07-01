# Bamboo Lisp

Embeddable & Hackable Lisp-2 Interpreter

There is a WebAssembly build, you can [try it online](https://mistivia.github.io/bamboo-lisp/).

## Features

- Lisp-2 (more like Common Lisp or Emacs Lisp)
- Lexical scoping
- The interpreter part is ~2500 LOC (excluding built-in functions)
- Tail call optimization
- Any C99 compiler should work
- Depends only on C standard library
- A simple mark-sweep GC
- Writing macro is easy with quasiquote, unquote, and slicing-unquote
- No global state, you can run multiple interpreters in multiple threads
- Exception and try-catch
- Support C-like control flow statements
    - return
    - break
    - continue

## Drawbacks

To keep simplicity, Bamboo Lisp is a VERY SLOW tree-walking interpreter. The performance is similar to other small Lisp interpreters like TinyScheme or very early Emacs Lisp, which is only 1/5 to 1/10 that of modern Python.

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

## Documents (WIP)

- [Primitives](https://github.com/mistivia/bamboo-lisp/blob/master/docs/primitives.md)

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


