(loadext "ext_example/vector.so")

(assert (vector? (make-vector)))
(assert (not (vector? 1)))

(defvar v (make-vector))

(assert (= 0 (vector-length v)))
(assert-error (vector-ref v 0))

(vector-append v 0)
(vector-append v "123")
(vector-append v 1.2)

(assert (= 3 (vector-length v)))

(vector-insert v 1 99)

(assert (equal? (vector-ref v 0) 0))
(assert (equal? (vector-ref v 1) 99))
(assert (equal? (vector-ref v 2) "123"))
(assert (equal? (vector-ref v 3) 1.2))

(vector-remove v 2)

(assert (equal? (vector-ref v 2) 1.2))
(assert (equal? (vector-ref v 1) 99))

(defvar x 2)
(vector-append v x)

(vector-set v 3 3)
(assert (= x 2))
(assert (= 3 (vector-ref v 3)))
