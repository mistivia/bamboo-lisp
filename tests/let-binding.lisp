(let ((a 1)
      (b 2))
  (assert (= 3 (+ a b))))

;; let is letrec by default
(let ((a 1)
      (b (+ a 1)))
  (assert (= 2 b)))

(let ((my-evenp
       (lambda (x)
         (if (= x 0)
           #t
           (funcall my-oddp (- x 1)))))
      (my-oddp
        (lambda (x)
          (if (= x 0)
            #f
            (funcall my-evenp (- x 1))))))
  (assert (funcall my-evenp 10))
  (assert (funcall my-oddp 9))
  (assert (not (funcall my-evenp 9)))
  (assert (not (funcall my-oddp 10))))

