
#include "prelude.h"

const char *bamboo_lisp_prelude = "(defvar nil \'())\n\n(defvar pi 3.1415926)\n(defvar e 2.718281828)\n\n(defmacro incq (i)\n  `(setq ,i (+ ,i 1)))\n\n(defmacro decq (i)\n  `(setq ,i (- ,i 1)))\n\n(defun zero? (x) (= x 0))\n(defun plus? (x) (> x 0))\n(defun minus? (x) (< x 0))\n\n(defmacro when (pred . body)\n  `(if ,pred\n     (progn ,@body)\n     nil))\n\n(defmacro unless (pred . body)\n  `(if ,pred\n     nil\n     (progn ,@body)))\n\n(defun caar (x) (car (car x)))\n(defun cadr (x) (car (cdr x)))\n(defun cddr (x) (cdr (cdr x)))\n(defun cdar (x) (cdr (car x)))\n\n(defun caaar (x) (car (caar x)))\n(defun cadar (x) (car (cdar x)))\n(defun cddar (x) (cdr (cdar x)))\n(defun cdaar (x) (cdr (caar x)))\n(defun caadr (x) (car (cadr x)))\n(defun caddr (x) (car (cddr x)))\n(defun cdddr (x) (cdr (cddr x)))\n(defun cdadr (x) (cdr (cadr x)))\n";


