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
