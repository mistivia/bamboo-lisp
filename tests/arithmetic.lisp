(assert (= 1 (+ 1 0)))
(assert (= -1 (- 0 1)))
(assert (= -1 (- 1)))
(assert (= 1.1 (+ 1 0.1)))
(assert (= 2 (i/ 11 5)))
(assert (= 1 (mod 11 5)))

(assert (zero? 0))
(assert (not (zero? 1)))
(assert (not (zero? -1)))

(assert (plus? 1))
(assert (plus? 1.0))
(assert (not (plus? 0)))
(assert (not (plus? -1)))

(assert (minus? -1))
(assert (not (minus? 0)))
(assert (not (minus? 1)))

(assert (< 1 2))
(assert (< 1.0 2))
(assert (not (> 1 2)))
(assert (= 1.0 1.0))
(assert (= 1 1.0))
(assert (not (= 1 2)))
(assert (>= 2 1))
(assert (>= 1 1))
(assert (not (>= 0 1)))
(assert (/= 2 1.0))
(assert (not (/= 1 1)))

(assert (= 1.0 (max -2 0.1 0.2 1)))
(assert (= 1.0 (min 1 2.0 3.2 4 100)))
(assert (= 3 (max 3)))
(assert (= 3 (min 3)))
(assert-error (max))
(assert-error (min))

(assert-error (+ 1 "a"))
(assert-error (- 1 "a"))
(assert-error (* 1 "a"))
(assert-error (/ 1 "a"))


;; mod is floored (the sign follows the divisor, as in Common Lisp / Emacs
;; Lisp), rem truncates (as in C).
(assert (= 97 (mod -3 100)))
(assert (= -3 (rem -3 100)))
(assert (= -97 (mod 3 -100)))
(assert (= 3 (rem 3 -100)))
(assert (= 0 (mod 100 100)))
(assert (= 1 (rem 11 5)))

(assert-error (i/ 1 0))
(assert-error (mod 1 0))
(assert-error (rem 1 0))
