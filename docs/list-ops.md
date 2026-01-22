# List Operations

Bamboo Lisp is a Lisp-2, with extensive list manipulation built-ins.

## Basic
- `cons`: Create a pair.
- `car`, `cdr`: Access head and tail.
- `list`: Create a list.

```lisp
(cons 1 2)       ; => (1 . 2)
(car '(1 2 3))   ; => 1
(cdr '(1 2 3))   ; => (2 3)
(list 1 2 3)     ; => (1 2 3)
```

## Transformation & Search
- `map`: Apply function to each element.
- `filter`: Keep elements matching predicate.
- `reverse`, `nreverse`: Reverse list (n- prefix is destructive).
- `append`, `nconc`: Concatenate lists.
- `member?`: Check if element exists.

```lisp
(map (lambda (x) (* x x)) '(1 2 3)) ; => (1 4 9)
(filter (lambda (x) (> x 1)) '(1 2 3)) ; => (2 3)
(append '(1 2) '(3 4)) ; => (1 2 3 4)
```

## Accessors
- `nth`, `nthcdr`: Access by index.
- `last`: Get last pair.
- `length`: List length.
- `caar`, `cadr`, `caddr`, etc. (up to 4 levels deep in prelude).

```lisp
(nth 1 '(a b c)) ; => b
(cadr '(a b c))  ; => b
```
