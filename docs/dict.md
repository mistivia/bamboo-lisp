# Dictionary Extension

The `dict` extension provides a string-keyed hash map (implemented as a tree map in `dict.c`).

## Functions

### `(make-dict)`
Creates a new dictionary.
```lisp
(defvar d (make-dict))
```

### `(dict? obj)`
Returns `#t` if `obj` is a dictionary.

### `(dict-set dict key value)`
Sets the `value` for `key` (must be a string) in `dict`. Returns `nil`.
```lisp
(dict-set d "name" "Bamboo")
```

### `(dict-get dict key)`
Returns the value associated with `key`, or `nil` if not found.
```lisp
(dict-get d "name") ; => "Bamboo"
```

### `(dict-remove dict key)`
Removes the `key` from `dict`. Returns `nil`.
```lisp
(dict-remove d "name")
```

### `(dict-keys dict)`
Returns a list of all keys in the dictionary as strings.
```lisp
(dict-keys d) ; => ("name" ...)
```
