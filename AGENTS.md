# AGENTS.md

本文件面向在本仓库中工作的 AI 代理与贡献者，说明项目结构、构建方式、
架构要点与常见陷阱。请在动手改代码前通读一遍。

## 项目简介

Bamboo Lisp 是一个可嵌入、易于修改的 **Lisp-2** 解释器（函数与变量分属
不同命名空间，风格接近 Common Lisp / Emacs Lisp）。特性包括：词法作用域、
尾调用优化、标记-清除 GC、异常与 try-catch、类 C 的控制流（return / break /
continue）、以及基于 quasiquote 的宏系统。它是一个树遍历解释器，追求简单
而非速度。

## 构建与测试

```sh
make            # 构建 bamboo-lisp 可执行文件与测试
make test       # 运行全部测试（Lisp 脚本测试 + 解析器单元测试）
./bamboo-lisp             # 进入 REPL
./bamboo-lisp foo.lisp    # 运行脚本
```

- 依赖：C99 编译器、`libreadline`、以及仓库内的 `algds/`（构建时自动编译）。
- `make test` 会先跑 `tests/*.bin`（C 单元测试），再执行
  `./bamboo-lisp tests/test.lisp`。`test.lisp` 把每个模块跑两遍，第二遍打开
  `_alwaysgc`（每次分配都触发 GC），用来暴露 GC 相关的 bug——**新功能务必
  能通过这一遍**。
- 调试内存问题时，用 AddressSanitizer 重新编译（**注意 `algds/` 也要带
  `-fsanitize=address` 重新编译**，否则堆访问越界看不到）。

## 目录结构

| 文件 / 目录        | 作用                                                       |
| ------------------ | ---------------------------------------------------------- |
| `sexp.h` / `sexp.c`| 对象表示 `SExp`、引用 `SExpRef`、分页对象堆 `SExpHeap`     |
| `interp.h/.c`      | 求值器、GC、对象构造函数、环境与查找、宏展开缓存           |
| `primitives.c`     | 特殊形式（if/let/lambda/defun/quote/try…）                 |
| `builtins.c`       | 普通内建函数（算术、列表、字符串、谓词…）                  |
| `parser.c`         | 读取器（S 表达式解析）                                     |
| `prelude.lisp`     | 用 Lisp 写的标准库（宏与函数），**这是唯一应手改的来源**   |
| `prelude.c`        | 由 `scripts/genprelude.py` 从 `prelude.lisp` 自动生成      |
| `vector.c` / `dict.c` / `io.c` | 内建模块（可变数组、字典、文件流），启动时注册 |
| `main.c`           | 入口：REPL 与脚本运行                                       |
| `tests/`           | 测试；每个 `*.lisp` 是一个模块，登记在 `tests/test.lisp`   |
| `algds/`           | 通用数据结构库（vector、hash table 等）                    |

## 架构要点

- **对象模型**：一切皆 `SExp`（带类型 tag 的联合体）。代码里不直接传
  `SExp`，而是传 `SExpRef`（对象在堆中的下标）。用宏 `REF(x)` 取指针，
  `CAR/CDR/CONS/NILP/EVAL` 等宏见 `interp.h`。
- **分页对象堆 `SExpHeap`**：对象存放在固定大小（4096）的 chunk 里，chunk
  一旦分配就不再移动。堆增长只是新分配一个 chunk，**因此 `SExp*` 指针在后续
  分配之后仍然有效**（旧的“扁平 vector + realloc”会移动全部对象，导致跨分配
  持有的 `SExp*` 悬空）。即便如此，跨分配时优先持有 `SExpRef`（下标），用到
  时再 `REF` 取指针。
- **GC（标记-清除）**：根集合包括求值栈 `stack`、临时寄存器链 `reg`、顶层
  环境 `top_level`（顶层绑定都挂在它上面）等。任何必须在一次会触发 GC 的调用
  （求值、宏展开、`CONS` 之外的分配路径）中存活的临时对象，都要么可从根到达，
  要么用 `PUSH_REG(x)` / `POP_REG()` 压入寄存器链保护。**这是最容易出错的地方。**
- **Lisp-2**：函数位置用 `lisp_lookup_func`（只查顶层的 `binding.func`），
  变量位置用 `lisp_lookup`。宏也存在函数命名空间里。
- **宏展开缓存**：`Interp.version` 在每次全局定义（defvar/defun/defmacro）时
  自增。函数对象 `SExpFunc` 缓存了“全部宏都展开后的函数体”和当时的 version；
  `lisp_apply` 里通过 `get_expanded_body` 复用缓存，仅当 version 变化时才
  重新展开（见 `macroexpand_all`）。展开过程会跑用户代码从而触发 GC，所以
  中间结果都用 `PUSH_REG` 保护。

## 修改约定

- **加一个内建函数**：在 `builtins.c` 写 `SExpRef builtin_foo(Interp*, SExpRef args)`，
  在 `builtins.h` 声明，在 `interp.c` 的 `Interp_init` 里用
  `Interp_add_userfunc(self, "foo", builtin_foo)` 注册。参数是一个求值后的
  实参列表；用 `LENGTH` 校验个数，`CAR/CADR/...` 取参，返回值用 `new_*`
  构造，出错返回 `new_error(interp, ...)`。
- **加一个特殊形式**：在 `primitives.c` 写 `primitive_foo`，声明并用
  `Interp_add_primitive` 注册。它拿到的是**未求值**的实参，自行决定求值。
- **加标准库函数/宏**：改 `prelude.lisp`（不要改 `prelude.c`，它是生成的）。
  `make` 会经 `scripts/genprelude.py` 重新生成 `prelude.c`。
- **加测试**：在 `tests/` 新增 `foo.lisp`，用 `assert` / `assert-error`，
  并把 `(test-module foo)` 加进 `tests/test.lisp`（两处：普通遍与 GC 遍）。

## 常见陷阱

- 忘记 GC 根：在两次分配之间持有的 `SExpRef`，若在此期间发生 GC 且它不可
  从根到达，就会被回收，之后使用得到已被复用的槽位（表现为“类型错乱”）。
  用 `PUSH_REG` 保护，或先把它接到某个已被保护的结构上。
- `car`/`cdr` 作用在非 pair 上会返回错误对象（不是崩溃）；解构前先用
  `cons?` 判断。
- 头文件已全部本地化（`#include "interp.h"`），**不再**依赖
  `/usr/local/include/bamboo_lisp/`。所有源码（含原 exts）都用本地头文件，
  以保证 `sizeof(SExp)`/结构布局在整个程序内一致。

## 更多文档

面向语言使用者的文档见 `docs/en/`（英文）与 `docs/zh/`（中文）。
