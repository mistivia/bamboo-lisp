# Primitives

**(assert *expression*)**

Evaluates *expression*. If *expression* evaluates to a non-false,
non-control-flow value, `assert` returns `#t`. Otherwise, it signals an error
indicating that the assertion failed, including the string representation of
the original *expression*.

```lisp
(assert (= 1 1))
;; -> #t

(assert (> 1 2))
;; -> Error
```

---

**(assert-exception *form*)**

Evaluates *form*. If *form* evaluates to an exception signal,
`assert-exception` returns `#t`. Otherwise, it raises an error indicating that
no exception was thrown by *form*. This primitive is useful for testing code
that is expected to throw exceptions.

```lisp
(assert-exception (throw "This is an exception."))
;; -> #t

(assert-exception (+ 1 2))
;; -> Error
```

---

**(assert-error *form*)**

Evaluates *form*. If *form* results in an **error signal**, `assert-error` returns `#t`. Otherwise, it throws an error indicating that *form* did not produce an error. This primitive is useful for writing tests that verify if specific code paths correctly raise errors.

```lisp
(assert-error (error "This is an error."))
;; -> #t

(assert-error (+ 1 2))
;; -> Error
```

---

**(try *expression* *catch-function*)**

The `try` primitive evaluates the given ***expression***. If the evaluation of ***expression*** results in an **exception signal**, the ***catch-function*** is then invoked with the exception's return value as its sole argument. If ***expression*** evaluates without an exception, its result is returned directly, and the ***catch-function*** is not called. If the second argument, ***catch-function***, is not a valid function, `try` will signal a syntax error.

```lisp
(try
  (throw "Alas!")
  (lambda (e) (concat "Caught exception: " e)))
;; -> "Caught exception: Alas!"

(try
  (+ 1 2)
  (lambda (e) (concat "Caught exception: " e)))
;; -> 3

(try
  (+ 1 2)
  'not-a-function)
;; -> Error: try: syntax error, catch is not a function.
```

---

**(load *filename*)**

Evaluates the Lisp expressions contained in the file specified by `filename`. The `filename` argument must be a string. This primitive can only be called from the top-level environment.

```lisp
(load "my-program.lisp")
;; -> <Result of the last expression in my-program.lisp>
```

---

**(return *expression*)**

Evaluates `expression` and returns its value from the current **function**. If `expression` is omitted, `return` evaluates to **nil**. It's important to note that `return` only exits functions and does not break out of `let` blocks or other control structures.

```lisp
(defun my-func (x)
    (if (> x 10)
        (return "Value too large!")
        (+ x 5)))
;; -> my-func

(my-func 5)
;; -> 10

(my-func 12)
;; -> "Value too large!"
```

---

**(break)**

This primitive immediately exits the innermost enclosing loop or iteration construct. It's analogous to the `break` statement in C. The `break` primitive takes no arguments.

```lisp
(defun count-to-five ()
    (let ((i 0))
        (while #t
            (setq i (+ i 1))
            (when (> i 5)
                (break))
            (print i))))
;; -> count-to-five

(count-to-five)
;; 1
;; 2
;; 3
;; 4
;; 5
;; -> ()
```

--- 

**(eval *expression*)**

Evaluates *expression* and returns its result. This primitive is typically used to evaluate code that is constructed or obtained at runtime.

```lisp
(eval '(+ 1 2))
;; -> 3

(eval '(* 3 4))
;; -> 12

(defvar my-expression '(+ 10 20))
(eval my-expression)
;; -> 30
```

---

**(unwind-protect *protected-form* *cleanup-form* \&rest *more-cleanup-forms*)**

Evaluates *protected-form*. After *protected-form* is evaluated, whether it completes normally or exits via a non-local exit (like an error or a `return`), each *cleanup-form* is evaluated sequentially. The return value of `unwind-protect` is the result of *protected-form*. This primitive is crucial for ensuring that cleanup actions (like closing files or releasing resources) always occur.

```lisp
(unwind-protect
    (progn (print "Doing something...")
           (/ 10 2))
    (print "Cleaning up!"))
;; "Doing something..."
;; "Cleaning up!"
;; -> 5

(unwind-protect
    (progn (print "About to error...")
           (error "error!"))
    (print "Still cleaning up!"))
;; "About to error..."
;; "Still cleaning up!"
;; -> Error
```

