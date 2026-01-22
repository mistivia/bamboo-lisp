# Vector Extension

The `vector` extension provides dynamic arrays for efficient indexed access and growth.

## Functions

### `(make-vector)`
Creates a new empty vector.
```lisp
(defvar v (make-vector))
```

### `(vector? obj)`
Returns `#t` if `obj` is a vector.

### `(vector-length vec)`
Returns the number of elements in the vector.
```lisp
(vector-length v)
```

### `(vector-ref vec index)`
Returns the element at `index`. Errors if out of bounds.
```lisp
(vector-ref v 0)
```

### `(vector-set vec index value)`
Sets the element at `index` to `value`. Returns `nil`.
```lisp
(vector-set v 0 "new-value")
```

### `(vector-append vec value)`
Appends `value` to the end of the vector.
```lisp
(vector-append v 42)
```

### `(vector-insert vec index value)`
Inserts `value` before `index`.
```lisp
(vector-insert v 0 "first")
```

### `(vector-remove vec index)`
Removes the element at `index`.
```lisp
(vector-remove v 0)
```
