(let ((l (list 1 2 3)))
  (set-cdr l 4)
  (assert (equal? l (cons 1 4))))

(let ((l (list 1 2 3)))
  (set-car l 4)
  (assert (equal? l (list 4 2 3))))

(assert-error (set-car 1))
(assert-error (set-car))
(assert-error (set-car (list 1) (list 2) (list 3)))
(assert-error (set-car ""))
(assert-error (set-cdr 1))
(assert-error (set-cdr))
(assert-error (set-cdr (list 1) (list 2) (list 3)))
(assert-error (set-cdr ""))


(assert (= 3 (length (list 1 2 3))))
(assert (= 0 (length nil)))

(assert (= 3 (nth 2 (list 1 2 3))))
(assert (equal? nil (nthcdr 2 (list 1 2 3))))
(assert (equal? (list 3) (nthcdr 1 (list 1 2 3))))

(assert (equal? (list 1 2 3 4)
                (map (lambda (x) (+ 1 x)) (list 0 1 2 3))))
