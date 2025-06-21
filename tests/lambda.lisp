(defun Y (f)
  (funcall
    (lambda (g) (funcall g g))
    (lambda (h)
       (funcall f (lambda args (apply (funcall h h) args))))))

(defun fibo-impl (self)
  (lambda (n)
    (if (<= n 2)
        1
        (+ (funcall self (- n 1)) (funcall self (- n 2))))))

(defvar fibo (Y #'fibo-impl))

(assert (= 55 (funcall fibo 10)))

(defun generate-counter (init)
  (let ((i init))
    (lambda ()
      (setq i (+ 1 i))
      i)))

(let ((c (generate-counter 0)))
  (assert (= 1 (funcall c)))
  (assert (= 2 (funcall c)))
  (assert (= 3 (funcall c))))
