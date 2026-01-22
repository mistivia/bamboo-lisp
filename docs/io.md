# I/O Extension

The `io` extension provides stream-based file and console I/O.

## Streams

### `(stream? obj)`
Returns `#t` if `obj` is an I/O stream.

### `(open-file filename mode)`
Opens a file. `mode` is a string like `"r"`, `"w"`, `"a"`. Returns a stream object.
```lisp
(defvar f (open-file "test.txt" "r"))
```

### `(stream-close stream)`
Closes the stream.

## Input Functions
Most input functions take an optional `stream` argument (defaults to `stdin`).

### `(read-char [stream])`
Reads one character. Returns `nil` on EOF.

### `(read-line [stream])`
Reads a line as a string. Returns `nil` on EOF.

### `(read-integer [stream])`
Reads an integer from the stream.

### `(read-number [stream])`
Reads a number (integer or float) from the stream.

### `(lines stream)`
Reads all lines from the stream into a list of strings.
```lisp
(let ((f (open-file "data.txt" "r")))
  (foreach (line (lines f))
    (princ line))
  (stream-close f))
```

## Output Functions
Take an optional `stream` argument (defaults to `stdout`).

### `(write-char char [stream])`
Writes a single character to the stream.

### `(write-obj obj [stream])`
Writes the printed representation of `obj` to the stream.
```lisp
(write-obj '(1 2 3) f)
```
