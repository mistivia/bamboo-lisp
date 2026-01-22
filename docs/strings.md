# String Functions

## Basic
- `string`: Convert to string.
- `concat`: Concatenate strings.
- `format`: Formatted string (supports `%s`, `%d`, etc.).

```lisp
(concat "hello" " " "world") ; => "hello world"
(format "Value: %d" 42)      ; => "Value: 42"
```

## Comparisons
- `string=`, `string<`, `string>`, etc.

```lisp
(string= "abc" "abc") ; => #t
```

## Utilities
- `split-string`: Split string by delimiter.
- `strip-string`: Trim whitespace.
- `string->symbol`: Convert string to symbol.
- `symbol->string`: Convert symbol to string.

```lisp
(split-string "a,b,c" ",") ; => ("a" "b" "c")
(strip-string "  trim me  ") ; => "trim me"
```

## Printing
- `print`: Print with newline and escaping.
- `princ`: Print without escaping (machine-readable).

```lisp
(princ "Hello\n") ; Prints: Hello
```

