(defvar nil '())

(defvar pi 3.1415926)
(defvar e 2.718281828)

(defmacro incq (i)
  `(setq ,i (+ ,i 1)))

(defmacro decq (i)
  `(setq ,i (- ,i 1)))

(defun zero? (x) (= x 0))
(defun plus? (x) (> x 0))
(defun minus? (x) (< x 0))

(defmacro when (pred . body)
  `(if ,pred
     (progn ,@body)
     nil))

(defmacro unless (pred . body)
  `(if ,pred
     nil
     (progn ,@body)))

(defun caar (x) (car (car x)))
(defun cadr (x) (car (cdr x)))
(defun cddr (x) (cdr (cdr x)))
(defun cdar (x) (cdr (car x)))

(defun caaar (x) (car (caar x)))
(defun cadar (x) (car (cdar x)))
(defun cddar (x) (cdr (cdar x)))
(defun cdaar (x) (cdr (caar x)))
(defun caadr (x) (car (cadr x)))
(defun caddr (x) (car (cddr x)))
(defun cdddr (x) (cdr (cddr x)))
(defun cdadr (x) (cdr (cadr x)))
