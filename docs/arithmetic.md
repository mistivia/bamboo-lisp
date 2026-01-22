# Arithmetic & Math Functions

## Basic Operators
Support both integers and floating point numbers.
- `+`, `-`, `*`, `/`: Basic arithmetic.
- `%` (or `mod`): Modulo.
- `//` (or `idiv`): Integer division.

```lisp
(+ 1 2 3) ; => 6
(/ 10 4)  ; => 2.5
(// 10 4) ; => 2
(% 10 3)  ; => 1
```

## Comparisons
- `=`, `/=`, `<`, `>`, `<=`, `>=`

```lisp
(= 1 1.0) ; => #t
(< 1 2)   ; => #t
```

## Math Functions
- `sqrt`, `abs`, `pow`, `exp`, `ln`, `log10`, `log2`
- `sin`, `cos`, `tan`, `asin`, `acos`, `atan`
- `floor`, `ceiling`, `round`, `truncate`

```lisp
(sqrt 16)   ; => 4.0
(pow 2 10)  ; => 1024.0
(floor 2.9) ; => 2.0
```
