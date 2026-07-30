# 语言参考

## 读取器语法

| 语法 | 含义 |
| ---- | ---- |
| `; ...` | 行注释（到行尾） |
| `42`、`-7` | 整数 |
| `3.14`、`1e-3` | 实数（浮点） |
| `"hello\n"` | 字符串（支持 `\n`、`\t`、`\"`、`\\` 等） |
| `#\a`、`#\newline`、`#\space`、`#\tab`、`#\return` | 字符 |
| `#\(`、`#\)`、`#\;`、`#\#`、`#\ ` | 字符（分隔符本身也可以直接写） |
| `#t`、`#f` | 布尔真 / 假 |
| `foo`、`list->vec`、`+`、`char<=` | 符号 |
| `(a b c)` | 列表 / 调用 |
| `(a . b)` | 点对 |
| `'x` | `(quote x)` |
| `` `x `` | `(quasiquote x)` |
| `,x` | `(unquote x)` |
| `,@x` | `(slicing-unquote x)` —— 在 quasiquote 中展开拼接 |
| `#'f` | `(function f)` —— 符号 `f` 的函数值 |

## 数据类型

整数、实数、布尔、字符、字符串、符号、空列表 `()`（也写作 `nil`）、点对
（cons 单元）、函数（lambda / 内建）、宏，以及不透明的用户数据
（vector / dict / io 模块使用）。

`nil` 即空列表。真值判断：只有 `#f` 与 `nil` 为假，其余（包括 `0` 和 `""`）
均为真。

## Lisp-2：函数与变量分离

同一个符号可以同时命名一个函数和一个变量。处于**调用位置**（列表的头部）时
按函数/宏查找，其它位置按变量查找。

```lisp
(defvar list 10)        ; 变量 list = 10
(list list list)        ; => (10 10)   ; 头部的 list 是函数，参数里的 list 是变量
```

把函数当值传递用 `#'name` 或 `(function name)`，调用函数值用 `funcall`
（或 `apply`）：

```lisp
(funcall #'+ 1 2 3)     ; => 6
(apply #'+ '(1 2 3))    ; => 6
(map #'car '((1 2) (3 4)))  ; => (1 3)
```

## 定义

```lisp
(defvar name value)                 ; 定义/重定义全局变量
(defun name (params...) body...)    ; 定义全局函数
(defun name value)                  ; 把名字绑定到一个已有函数值
(defmacro name (params...) body...) ; 定义宏
```

参数表可用点对尾部捕获其余参数：

```lisp
(defun list* (first . rest) (cons first rest))
(defun k () 42)                     ; 无参数
```

`defun` / `defvar` / `defmacro` 只能出现在顶层。

## 特殊形式

- `(quote x)` / `'x` —— 返回未求值的 `x`。
- `(if test then else?)` —— `else` 可省略（默认 `nil`）。
- `(cond (test expr) ...)` —— 取第一个 `test` 为真的子句；每个子句恰为
  `(test expr)`。
- `(when test body...)`、`(unless test body...)` —— prelude 宏。
- `(and e...)`、`(or e...)` —— 短路求值，返回决定性的那个值。
- `(progn body...)` —— 顺序求值，返回最后一个。
- `(let ((name init) ...) body...)` —— 绑定是**顺序**的：同一 `let` 中后面的
  `init` 可以引用前面的绑定。
- `(setq name value)` —— 给已有（词法或全局）变量赋值。
- `(lambda (params...) body...)` —— 词法闭包。`(lambda args body...)`（参数是
  单个符号）会把全部实参捕获为一个列表。
- `(function f)` / `#'f` —— `f` 的函数值。
- `(funcall fn args...)`、`(apply fn args-list)` —— 调用函数值。
- `(while test body...)` —— 循环。
- `(return v)`、`(break)`、`(continue)` —— 函数/循环内的类 C 控制流。
- `(quasiquote t)` / `` `t ``，配合 `,x` 与 `,@x` —— 模板构造。
- `(eval form)`、`(macroexpand-1 form)`。
- `(load "file.lisp")` —— 在顶层读取并求值一个文件。

### 异常

```lisp
(try EXPR CATCH)        ; 运行 EXPR；若抛出异常，则调用 (CATCH value)
(throw value)           ; 抛出异常
(unwind-protect EXPR CLEANUP)  ; EXPR 结束后总会执行 CLEANUP

(assert EXPR)            ; EXPR 为假则报错
(assert-error EXPR)      ; 断言 EXPR 会触发错误
(assert-exception EXPR)  ; 断言 EXPR 会抛出异常
```

运行时错误带有消息和调用栈，冒泡到顶层时打印。

## 控制流示例

```lisp
(defun first-even (lst)
  (while (not (null? lst))
    (when (zero? (mod (car lst) 2))
      (return (car lst)))
    (setq lst (cdr lst)))
  nil)
```

## 宏

宏拿到未求值的参数，返回一个新的表达式，随后该表达式被求值。用 quasiquote
构造结果：

```lisp
(defmacro incq (i) `(setq ,i (+ ,i 1)))

(defmacro for (start pred inc . body)
  `(let (,start)
     (while ,pred
       ,@body
       ,inc)))

(let ((sum 0))
  (for (i 0) (< i 5) (incq i)
    (setq sum (+ sum i)))
  sum)                      ; => 10
```

用 `gensym` 生成新符号以避免变量捕获。注意：宏展开会按函数定义缓存，因此宏里
的 `gensym` 对每个使用点只求值一次（每个使用点得到一个新符号，且在外层函数
多次调用之间保持稳定）—— 这正是宏卫生所需要的行为。

`(macroexpand-1 '(when x y))` 返回单步展开结果，便于调试。

## `pcase` —— 模式匹配

`pcase` 是一个 prelude 宏，提供 ML 风格的模式匹配（Emacs Lisp `pcase` 的
一个子集）：

```lisp
(pcase EXPR
  (PATTERN body...)
  ...)
```

`EXPR` 只求值一次；子句按顺序尝试；第一个匹配的模式的 body 在其变量已绑定的
情况下执行。若无匹配，结果为 `nil`。

### 模式

| 模式 | 匹配 |
| ---- | ---- |
| `_` | 任意值（不绑定） |
| `x` | 任意值，并把 `x` 绑定到该值 |
| `nil` | 空列表 |
| `42`、`"s"`、`#\c`、`#t` | 字面量，用 `equal?` 比较 |
| `'value` | 与 `value` 用 `equal?` 相等的值（如某个字面符号） |
| `(pred FN)` | 当 `(FN value)` 为真 |
| `(guard EXPR)` | 当 `EXPR`（可用已绑定变量）为真 |
| `(and PAT...)` | 所有子模式都匹配 |
| `(or PAT...)` | 某个子模式匹配 |
| `` `TEMPLATE `` | 结构匹配；`,PAT` 匹配/绑定某个位置 |

反引号模式用于解构列表：`` `(,a ,b) `` 匹配长度为 2 的列表并绑定 `a`、`b`；
`` `(tag ,x) `` 匹配头部为字面符号 `tag` 的列表；`` `(,head . ,tail) `` 把一个
cons 拆成头与尾。

```lisp
(defun describe (x)
  (pcase x
    (0 "zero")
    ((pred integer?) "some integer")
    ('hello "a greeting symbol")
    (`(,a ,b) (format "pair of %s and %s" a b))
    (`(,h . ,t) (format "list starting with %s" h))
    (_ "something else")))
```
