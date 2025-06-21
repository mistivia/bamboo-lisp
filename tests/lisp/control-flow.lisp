(assert-error (if (error "") 1 2))

(let ((i 0))
  (while #t
    (if (> i 4)
      (break)
      nil)
    (incq i))
  (assert (= i 5)))

(let ((i 0))
  (while #t
    (if (> i 4)
      (let () (break))
      nil)
    (incq i))
  (assert (= i 5)))

(let ((flag 0)
      (i 0))
  (while (< i 10)
    (incq i)
    (continue)
    (setq flag 1))
  (assert (= i 10))
  (assert (= flag 0)))

(assert-error (funcall (lambda () (break))))
(assert-error (funcall (lambda () (continue))))
(assert (= 1 (funcall (lambda () (return 1)))))
(assert (= 1 (funcall (lambda () (while #t (return 1))))))
(assert (= 1 (funcall (lambda () (let () (return 1))))))
(assert (= 1 (funcall (lambda () (let ((x (return 1))) (return 2))))))
