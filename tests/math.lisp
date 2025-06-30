(defun ~~f (a b)
  (< (abs (- a b)) 0.01))

(defmacro ~~ (a b)
  `(assert (~~f ,a ,b)))

(assert (= 1 (abs -1)))
(assert (= 1.1 (abs -1.1)))
(assert (= 1 (abs 1)))
(assert (= 1.1 (abs 1.1)))

(~~ 0.2 (/ 5))

(~~ 3.141 pi)
(assert-error (~~ 3.2 pi))

(~~ 2.718 e)

(assert (= 1.0 (float 1)))
(assert (= -1.0 (float -1)))

(~~ 8 (pow 2 3))
(~~ 1.414 (pow 2 0.5))
(~~ 1.732 (pow 3 0.5))

(~~ 2.0 (floor 2.1))
(~~ 2.0 (floor 2.5))
(~~ 2.0 (floor 2.7))
(~~ -2.0 (floor -1.1))
(~~ -2.0 (floor -1.5))
(~~ -2.0 (floor -1.7))

(~~ 2.0 (truncate 2.1))
(~~ 2.0 (truncate 2.5))
(~~ 2.0 (truncate 2.7))
(~~ -2.0 (truncate -2.1))
(~~ -2.0 (truncate -2.5))
(~~ -2.0 (truncate -2.7))

(~~ 2.0 (ceiling 1.1))
(~~ 2.0 (ceiling 1.5))
(~~ 2.0 (ceiling 1.7))
(~~ -2.0 (ceiling -2.1))
(~~ -2.0 (ceiling -2.5))
(~~ -2.0 (ceiling -2.7))

(~~ 2.0 (round 2.1))
(~~ 2.0 (round 1.5))
(~~ 2.0 (round 1.7))
(~~ -2.0 (round -2.1))
(~~ -2.0 (round -1.5))
(~~ -2.0 (round -1.7))

(~~ 0 (sin 0))
(~~ 1 (sin (/ pi 2)))
(~~ -1 (sin (- (/ pi 2))))

(~~ 1 (cos 0))
(~~ 0 (cos (/ pi 2)))
(~~ 0 (cos (- (/ pi 2))))

(~~ (tan 1.1234) (/ (sin 1.1234) (cos 1.1234)))

(~~ (asin 0.5) 0.525)
(~~ (acos 0.5) 1.047)
(~~ (atan 0.5) 0.463)

(~~ 0 (ln 1))
(~~ 1 (ln e))
(~~ 2 (ln (* e e)))
(~~ 1.5 (ln (* e (sqrt e))))
(~~ 1.333 (ln (* e (cbrt e))))
(~~ 0.667 (ln (/ e (cbrt e))))

(~~ 0 (log10 1))
(~~ 1 (log10 10))
(~~ 2 (log10 (* 10 10)))
(~~ 1.5 (log10 (* 10 (sqrt 10))))
(~~ 1.333 (log10 (* 10 (cbrt 10))))
(~~ 0.667 (log10 (/ 10 (cbrt 10))))

(~~ 0 (log2 1))
(~~ 1 (log2 2))
(~~ 2 (log2 (* 2 2)))
(~~ 1.5 (log2 (* 2 (sqrt 2))))
(~~ 1.333 (log2 (* 2 (cbrt 2))))
(~~ 0.667 (log2 (/ 2 (cbrt 2))))

(~~ (pow e 1.5) (exp 1.5))
