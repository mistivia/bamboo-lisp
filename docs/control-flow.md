# Control Flow Primitives

Bamboo Lisp provides standard Lisp control flow and some C-like additions.

## `if`
Standard conditional.
```lisp
(if (> 2 1) 'larger 'smaller) ; => larger
```

## `cond`
Multi-way conditional.
```lisp
(cond
  ((< x 0) 'negative)
  ((> x 0) 'positive)
  (#t 'zero))
```

## `while`
Loops while a condition is true. Supports `break` and `continue`.
```lisp
(let ((i 0))
  (while (< i 3)
    (princ i)
    (setq i (+ i 1))))
; Outputs: 012
```

## `progn`
Executes multiple expressions and returns the result of the last one.
```lisp
(progn
  (princ "hello ")
  (princ "world")
  10) ; => 10
```

## `return`, `break`, `continue`
C-like control flow within loops and functions.
```lisp
(defun find-first-even (lst)
  (foreach (x lst)
    (when (zero? (% x 2))
      (return x))))
```

## `try` and `throw`
Exception handling.
```lisp
(try
  (throw "error message")
  (catch (e) (princ e)))
```
