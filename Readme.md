# Bamboo Lisp

Embeddable & Hackable Lisp-2 Interpreter

(Work in Progress)

## Build

Debug:

```bash
git submodule init --recursive
make
```

Release:

```bash
git submodule init --recursive
make profile=release
```

## Example

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
  (show "meow"))
```

