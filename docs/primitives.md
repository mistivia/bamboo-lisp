# Primitives

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

```llisp
(assert-error (error "This is an error."))
    -> #t

(assert-error (+ 1 2))
    -> Error
```

---

**(try *expression* *catch-function*)**

The `try` primitive evaluates the given ***expression***. If the evaluation of ***expression*** results in an **exception signal**, the ***catch-function*** is then invoked with the exception's return value as its sole argument. If ***expression*** evaluates without an exception, its result is returned directly, and the ***catch-function*** is not called. If the second argument, ***catch-function***, is not a valid function, `try` will signal a syntax error.

```lisp
(try
  (throw "Alas!")
  (lambda (e) (string-append "Caught exception: " e)))
    -> "Caught exception: Alas!"

(try
  (+ 1 2)
  (lambda (e) (string-append "Caught exception: " e)))
    -> 3

(try
  (+ 1 2)
  'not-a-function)
    -> Error: try: syntax error, catch is not a function.
```

