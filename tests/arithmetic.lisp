(assert (= 1 (+ 1 0)))
(assert (= -1 (- 0 1)))
(assert (= -1 (- 1)))
(assert (= 1.1 (+ 1 0.1)))
(assert (= 2 (i/ 11 5)))
(assert (= 1 (mod 11 5)))

(assert-error (+ 1 "a"))
(assert-error (- 1 "a"))
(assert-error (* 1 "a"))
(assert-error (/ 1 "a"))

