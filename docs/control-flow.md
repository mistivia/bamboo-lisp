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
Exception handling. `try` takes a protected expression and a handler function. The handler function is called with the thrown value if an exception occurs.

```lisp
(try
  (throw "something went wrong")
  (lambda (e) (princ e)))
```

## `unwind-protect`
Ensures that cleanup forms are executed regardless of how the protected form exits (whether normally, or via `throw`, `error`, `return`, `break`, etc.).

```lisp
(let ((file (open-file "test.txt" "r")))
  (unwind-protect
    (read-line file)
    (stream-close file)))
```

## Errors vs. Exceptions

Bamboo Lisp distinguishes between **Errors** and **Exceptions**:

- **Exceptions** (`throw`): Used for recoverable conditions or intentional non-local exits. They can be caught and handled using `try`.
- **Errors** (`error`): Used for fatal, unrecoverable conditions (e.g., type errors, syntax errors). They **cannot** be caught by `try` and will typically terminate the execution with an error message.

Both will trigger `unwind-protect` cleanup forms as they propagate up the call stack.
