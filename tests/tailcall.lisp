(defun is-even (x)
  (if (= x 0)
    #t
    (is-odd (- x 1))))

(defun is-odd (x)
    (is-even (- x 1)))

(assert (is-even 10000))
(assert (is-even 10))
(assert (is-even 0))
(assert (is-odd 1))
(assert (is-even 2))

;; can pass without stack overflow,
;; but comment out for too time-consuming
;; (assert (is-even 1000000)) 
