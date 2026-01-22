# Bamboo Lisp: AI Context & Instructions

Bamboo Lisp is an embeddable and hackable Lisp-2 interpreter written in C99. It is designed for simplicity, ease of integration, and extensibility.

## Project Overview

- **Type:** Lisp-2 Interpreter (separate namespaces for functions and variables).
- **Architecture:** Tree-walking interpreter with lexical scoping.
- **Key Features:**
  - Tail Call Optimization (TCO).
  - Mark-sweep garbage collection.
  - Macros (quasiquote, unquote, slicing-unquote).
  - Exception handling (try-catch).
  - C-like control flow (`return`, `break`, `continue`).
  - No global state (supports multiple instances).
- **Performance:** Prioritizes simplicity over speed; comparable to TinyScheme or early Emacs Lisp.

## Building and Running

### Prerequisites
- **C99 Compiler** (e.g., GCC).
- **[algds](https://github.com/mistivia/algds)**: A library for data structures (must be installed).
- **readline**: Required for REPL support (`-lreadline`).

### Commands
- **Build (Release):** `make mode=release`
- **Build (Debug):** `make mode=debug`
- **Install:** `sudo make install`
- **Run REPL:** `./bamboo-lisp`
- **Run Script:** `./bamboo-lisp <filename.lisp>`
- **Clean:** `make clean`

## Testing

Tests are divided into binary C tests and Lisp-level script tests.

- **Run all tests:** `make test`
- **C Tests:** Located in `tests/*.c`. They are compiled into binaries and executed by `scripts/runall.sh`.
- **Lisp Tests:** Located in `tests/*.lisp`. The main test suite is `tests/test.lisp`, which loads other modules.

## Key Files & Directory Structure

- `main.c`: CLI entry point and REPL loop.
- `interp.c/h`: Core evaluation engine (`lisp_eval`, `lisp_apply`).
- `sexp.c/h`: S-expression representation (`SExp` union) and mark-sweep GC.
- `parser.c/h`: S-expression parser.
- `builtins.c/h` & `primitives.c/h`: Implementation of built-in functions in C.
- `prelude.lisp`: The standard library defined in Lisp.
- `prelude.c`: Generated from `prelude.lisp` via `scripts/genprelude.py`.
- `exts/`: Optional extensions (dictionaries, vectors, I/O) implemented as C user-data.
- `docs/`: Comprehensive documentation for primitives, built-ins, and extensions.
- `tests/`: Comprehensive test suite and examples.

## Development Conventions

- **Language:** C99.
- **Memory Management:** Objects are managed via `SExpRef` (an index-based reference). Always use `SExpRef` when passing Lisp objects to ensure GC safety.
- **Lisp-2 Semantics:** Use `defun` for functions and `defvar` / `setq` for variables. Functions are called via `funcall` if stored in variables.
- **Error Handling:** Errors and exceptions are propagated as special `SExpType` signals (`kErrSignal`, `kExceptionSignal`).
- **Extensions:** New primitives should be added via `Interp_add_primitive` or `Interp_add_userfunc`. Extensions in `exts/` use the `UserData` type and `LispUserdataMeta` for custom GC and cleanup.
- **Prelude:** Modifications to core Lisp functions should be made in `prelude.lisp` and then recompiled (which triggers `scripts/genprelude.py`).

## Documentation

For detailed information on available functions, refer to the `docs/` directory:
- `docs/primitives.md`: Fundamental evaluator building blocks.
- `docs/control-flow.md`: Conditionals and loops.
- `docs/arithmetic.md`: Math and logic operations.
- `docs/list-ops.md`: List manipulation.
- `docs/strings.md`: String handling and printing.
- `docs/dict.md`: Key-value dictionary extension.
- `docs/vector.md`: Dynamic array extension.
- `docs/io.md`: File and stream I/O extension.

## Future AI Instructions
- When adding new primitives, verify they are registered in `Interp_init` or relevant extension initialization functions.
- When modifying the evaluator, ensure that tail positions are correctly handled to maintain TCO.
- Always run `make test` after changes to ensure no regressions in the core Lisp logic or the GC.
