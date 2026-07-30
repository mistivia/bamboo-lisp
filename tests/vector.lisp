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

(defvar v3 (make-vector 3 0))
(assert (= 3 (vector-length v3)))
(assert (= 0 (vector-ref v3 2)))
(assert (= 0 (vector-length (make-vector 0))))
(assert (null? (vector-ref (make-vector 1) 0)))
(assert-error (make-vector -1))
(assert-error (make-vector "3"))

(assert-error (vector-ref v3 -1))
(assert-error (vector-ref v3 3))
(assert-error (vector-set v3 -1 1))
(assert-error (vector-remove v3 -1))
(assert-error (vector-insert v3 -1 1))
(assert-error (vector-insert v3 4 1))

(assert (equal? '(1 2 3) (vector->list (list->vector '(1 2 3)))))
(assert (null? (vector->list (list->vector nil))))
