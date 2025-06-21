
#include "prelude.h"

const char *bamboo_lisp_prelude = "(defvar nil \'())\n\n(defvar pi 3.1415926)\n\n(defmacro incq (i)\n  `(setq ,i (+ ,i 1)))\n\n(defmacro decq (i)\n  `(setq ,i (- ,i 1)))\n\n(defun zerop (x) (= x 0))\n\n(defmacro when (pred . body)\n  `(if ,pred\n     (progn ,@body)\n     nil))\n\n(defmacro unless (pred . body)\n  `(if ,pred\n     nil\n     (progn ,@body)))\n";


