# 标准库

下面列出内建函数、prelude（用 Lisp 写的函数/宏）以及内建模块。函数调用形式为
`(name args...)`；除非特别说明，都会先对所有参数求值。

## 算术

- `(+ ...)`、`(- ...)`、`(* ...)`、`(/ ...)` —— 加、减、乘、除。`/` 在结果
  非整数时返回实数。
- `(i/ a b)` —— 整数除法（向零截断）。
- `(mod a b)` —— 向下取整的取模，结果符号跟除数，`(mod -3 100)` 得 `97`
  （与 Common Lisp / Emacs Lisp 一致）。
- `(rem a b)` —— 截断取余，结果符号跟被除数，`(rem -3 100)` 得 `-3`（与 C 一致）。
- 除数为 0 时返回错误，而不是崩溃。
- `(abs x)`、`(min ...)`、`(max ...)`。
- `(floor x)`、`(ceiling x)`、`(round x)`、`(truncate x)`。
- `(expt b e)` / `(pow b e)`、`(sqrt x)`、`(cbrt x)`、`(exp x)`、`(ln x)`、
  `(log2 x)`、`(log10 x)`。
- 三角函数：`(sin x)` `(cos x)` `(tan x)` `(asin x)` `(acos x)` `(atan x)`。
- `(float x)` —— 转为实数。

## 比较与逻辑

- 数值比较：`(= ...)`、`(/= ...)`、`(< ...)`、`(> ...)`、`(<= ...)`、`(>= ...)`。
- `(not x)` —— 逻辑取反（`and` / `or` 是特殊形式）。
- `(eq? a b)` —— 同一性；`(equal? a b)` —— 结构相等。

## 谓词

`null?`、`cons?`、`list?`、`atom?`、`symbol?`、`string?`、`char?`、`number?`、
`integer?`、`float?`、`function?`、`member?`（`(member? x lst)`），以及 prelude
辅助谓词 `zero?`、`plus?`、`minus?`、`contains?`（`(contains? x lst)`）。

## 点对与列表

- `(cons a b)`、`(car p)`、`(cdr p)`、`(set-car p v)`、`(set-cdr p v)`。
- 四层以内的所有 `c[ad]+r` 访问器：`caar`、`cadr`、`caddr`、`cddddr` …（prelude）。
- `(list ...)`、`(length lst)`、`(nth n lst)`、`(nthcdr n lst)`、
  `(set-nth n lst v)`、`(set-nthcdr n lst v)`、`(last lst)`。
- `(reverse lst)`、`(nreverse lst)`（破坏性）、`(append ...)`、
  `(nconc ...)`（破坏性）。
- 高阶：`(map f lst)`、`(filter pred lst)`、`(remove pred lst)`、
  `(count pred lst)`、`(foreach f lst)`、`(foldl f init lst)`。
- prelude：`(find x lst)`、`(take n lst)`、`(drop n lst)`、
  `(take-while pred lst)`、`(drop-while pred lst)`、`(sublist start end lst)`、
  `(sort lst pred)` —— 稳定归并排序，`pred` 为「严格小于」；
  `(merge-sorted a b pred)` —— 合并两个已排序列表。
- prelude 转换：`(list->vector lst)`、`(vector->list vec)`。

## 字符串

- `(string ...)`、`(concat ...)` —— 构造/拼接字符串。
- `(format fmt args...)` —— 返回格式化字符串（`%s` 插入一个参数）。
- `(print x)` —— 输出机器可读形式（字符串带引号）；`(princ x)` —— 输出显示
  形式（字符串不带引号）。两者都写到标准输出。
- 比较：`string=`、`string/=`、`string<`、`string>`、`string<=`、`string>=`。
- `(split-string s sep)`、`(strip-string s)`。
- `(string-length s)`、`(string-ref s i)` —— 取第 `i` 个字符。
- `(substring s start)` / `(substring s start end)` —— `end` 默认为字符串末尾，
  下标必须在范围内。
- `(string->list s)`、`(list->string chars)`。
- `(string->number s)` —— 返回整数或实数；`s`（忽略首尾空白）不是数字时返回
  `nil`。`(number->string n)`。

## 字符

- 比较：`char=`、`char/=`、`char<`、`char>`、`char<=`、`char>=`。
- `(int->char n)`、`(char->int c)`。
- `(alphabetic? c)`、`(numeric? c)`、`(alphanum? c)`。

## 符号

- `(gensym)` —— 生成一个全新的唯一符号（用于宏卫生）。
- `(intern s)` —— 名为字符串 `s` 的符号。
- `(symbol->string sym)`。

## 位运算

`(logand ...)`、`(logior ...)`、`(logxor ...)`、`(lognot x)`、
`(lsh x n)`（逻辑移位）、`(ash x n)`（算术移位）。

## 其它

- `(eval form)`、`(apply fn args-list)`、`(funcall fn args...)`。
- `(error fmt args...)` —— 触发错误；`(throw v)` —— 抛出异常。
- `(exit)` —— 退出解释器。
- `(_gcstat)`、`(_alwaysgc bool)` —— GC 诊断（测试套件使用）。

## Prelude 宏

- `(when test body...)`、`(unless test body...)`。
- `(incq place)`、`(decq place)` —— 原地自增 / 自减。
- `(dolist (x lst) body...)`、`(dotimes (i n) body...)` —— 循环。游标与计数器
  在 body 之前推进，因此 `break` / `continue` 的行为与直接写 `while` 一致。
- `(pcase ...)` —— 模式匹配（见[语言参考](language.md)）。

---

# 内建模块

以下模块在启动时注册，无需单独加载。

## 可变数组（`vector.c`）

- `(make-vector)` / `(make-vector size)` / `(make-vector size fill)`、`(vector? x)`。
- `(vector-length v)`、`(vector-ref v i)`、`(vector-set v i x)`。
- `(vector-append v x)`、`(vector-insert v i x)`、`(vector-remove v i)`。

## 字典（`dict.c`）

键为字符串。

- `(make-dict)`、`(dict? x)`。
- `(dict-get d key)`、`(dict-set d key value)`、`(dict-remove d key)`、
  `(dict-keys d)`。

## I/O 流（`io.c`）

- `(open-file path mode)`、`(stream? x)`、`(stream-close s)`。
- 读取：`(read-char s)`、`(read-line s)`、`(read-integer s)`、
  `(read-number s)`、`(lines s)`。stream 参数可以省略，省略时读标准输入。
- 写入：`(write-char s c)`、`(write-obj s x)`。
