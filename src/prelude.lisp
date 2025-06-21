(defvar nil '())

(defvar pi 3.1415926)

(defmacro incq (i)
  `(setq ,i (+ ,i 1)))

(defmacro decq (i)
  `(setq ,i (- ,i 1)))

(defun zerop (x) (= x 0))
