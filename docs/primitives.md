# Core Primitives

These are the fundamental building blocks implemented directly in the evaluator.

## `quote` or `'`
Prevents evaluation of an expression.
```lisp
'(1 2 3) ; => (1 2 3)
```

## `lambda`
Creates an anonymous function.
```lisp
(lambda (x) (* x x))
```

## `defun`
Defines a named function.
```lisp
(defun square (x) (* x x))
```

## `defvar`
Defines a top-level variable.
```lisp
(defvar *config* 'debug)
```

## `setq`
Assigns a value to a variable.
```lisp
(setq x 10)
```

## `let`
Creates local bindings.
```lisp
(let ((x 1)
      (y 2))
  (+ x y)) ; => 3
```

## `defmacro`
Defines a macro.
```lisp
(defmacro inc (x)
  `(setq ,x (+ ,x 1)))
```

## `funcall` and `apply`
- `funcall`: Call a function object with arguments.
- `apply`: Call a function object with a list of arguments.

```lisp
(funcall #'+ 1 2)     ; => 3
(apply #'+ '(1 2 3))  ; => 6
```

## `load`
Loads and executes a Lisp file.
```lisp
(load "library.lisp")
```
