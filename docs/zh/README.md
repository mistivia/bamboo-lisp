# Bamboo Lisp —— 文档（中文）

Bamboo Lisp 是一个小巧、可嵌入、易于修改的 **Lisp-2** 解释器：函数与变量
分属两个命名空间（风格接近 Common Lisp / Emacs Lisp）。它支持词法作用域、
尾调用优化、标记-清除 GC、基于 `try` 的异常、类 C 的控制流
（`return` / `break` / `continue`），以及基于 quasiquote 的宏系统。

## 目录

- [语言参考](language.md) —— 语法、数据类型、特殊形式、宏、以及 `pcase`
  模式匹配。
- [标准库](stdlib.md) —— 内建函数、prelude、以及 vector / dict / io 模块。

English docs: [`../en/`](../en/README.md)。

## 快速开始

构建与运行：

```sh
make
./bamboo-lisp            # 进入 REPL
./bamboo-lisp file.lisp  # 运行脚本
```

在解释器内加载脚本：

```lisp
(load "my-script.lisp")
```

## 一瞥

```lisp
;; Lisp-2：用 #'f / (function f) 取函数值，用 funcall 调用
(defun square (x) (* x x))
(princ (format "%s\n" (map #'square '(1 2 3 4))))   ; => (1 4 9 16)

;; 用 quasiquote 写宏
(defmacro unless (test . body)
  `(if ,test nil (progn ,@body)))

;; 模式匹配
(defun eval-expr (e)
  (pcase e
    ((pred integer?) e)
    (`(+ ,a ,b) (+ (eval-expr a) (eval-expr b)))
    (`(* ,a ,b) (* (eval-expr a) (eval-expr b)))))
(princ (format "%s\n" (eval-expr '(+ 1 (* 2 3)))))   ; => 7
```
