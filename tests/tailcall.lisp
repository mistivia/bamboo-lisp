(defun is-even (x)
  (if (= x 0)
    #t
    (is-odd (- x 1))))

(defun is-odd (x)
    (is-even (- x 1)))

(assert (is-even 100))
(assert (is-even 10))
(assert (is-even 0))
(assert (is-odd 1))
(assert (is-even 2))

(defun cnt-down (x)
  (if (= x 0)
    #t
    (progn
      (cnt-down (- x 1)))))

(cnt-down 100)
