(assert-error (if (error "") 1 2))

(defmacro inmacro x (progn ,@x))

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

(let ((f nil))
  (setq f
    (lambda (x)
      (cond ((> x 5) 1)
            ((> x 0) (+ 1 1))
            (#t (+ x 1)))))
  (assert (= 1 (funcall f 10)))
  (assert (= 2 (funcall f 3)))
  (assert (= 0 (funcall f -1))))

(let ((r nil))
  (if (> 2 1)
    (setq r 1)
    (setq r 2))
  (assert (= r 1)))

(let ((r 1))
  (when (> 2 1)
    (setq r 2))
  (assert (= r 2)))

(let ((r 1))
  (when (> 1 2)
    (setq r 2))
  (assert (= r 1)))

(let ((r 1))
  (unless (> 1 2)
    (setq r 2))
  (assert (= r 2)))

(let ((r 1))
  (unless (> 2 1)
    (setq r 2))
  (assert (= r 1)))

(assert-error (funcall (lambda () (break))))
(assert-error (funcall (lambda () (continue))))
(assert (= 1 (funcall (lambda () (return 1)))))
(assert (= 1 (funcall (lambda () (inmacro (return 1) (return 2))))))
(assert (= 1 (funcall (lambda () (while #t (return 1))))))
(assert (= 1 (funcall (lambda () (let () (return 1))))))
(assert (= 1 (funcall (lambda () (let ((x (return 1))) (return 2))))))
