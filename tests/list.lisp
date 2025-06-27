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

(assert-error (reverse 1))
(assert-error (reverse (cons 1 2)))

(assert (equal? (list 1 2 3) (reverse (list 3 2 1))))
(assert (equal? (list 1 2) (reverse (list 2 1))))
(assert (equal? (list 1) (reverse (list 1))))
(assert (equal? nil (reverse nil)))

(assert-error (nreverse 1))
(assert-error (nreverse (cons 1 2)))

(assert (equal? (list 1 2 3) (nreverse (list 3 2 1))))
(assert (equal? (list 1 2) (nreverse (list 2 1))))
(assert (equal? (list 1) (nreverse (list 1))))
(assert (equal? nil (nreverse nil)))

(assert-error (last '()))
(assert-error (last 1))

(assert (equal? 3 (last (list 1 2 3))))
(assert (equal? 3 (last (list 2 3))))
(assert (equal? 3 (last (list 3))))

(assert (member? nil (list 1 2 nil)))
(assert (member? 1 (list 1 2 nil)))
(assert (member? 2 (list 1 2 nil)))
(assert (not (member? nil (list 1 2))))
(assert (not (member? 3 (list 1 2))))

    ;;Interp_add_userfunc(self, "nconc", builtin_reverse);

(let ((lst '(1 2 999 4)))
  (set-nth 2 lst 3)
  (assert (equal? '(1 2 3 4) lst)))

(let ((lst '(1 2 999 4)))
  (set-nthcdr 2 lst '(1000 1001))
  (assert (equal? '(1 2 999 1000 1001) lst)))

(assert (= 10 (foldl #'+ 0 '(1 2 3 4))))

(let ((a '(1 2 3))
      (b '(4 5 6))
      (c '(7 8 9)))
  (assert (equal? '(1 2 3 4 5 6 7 8 9) (append a b c)))
  (assert (equal? '(1 2 3) a))
  (assert (equal? '(4 5 6) b))
  (assert (equal? '(7 8 9) c)))

(let ((a '(1 2 3))
      (b '(4 5 6)))
  (assert (equal? '(1 2 3 4 5 6) (nconc a b))))
