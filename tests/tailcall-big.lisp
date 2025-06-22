(assert (is-even 10000))
(assert (cnt-down 10000))

;; can pass without stack overflow,
;; but comment out for too time-consuming
;; (assert (is-even 1000000)) 
;; (assert (cnt-down 1000000))

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
  (assert (funcall my-evenp 10000))
  (assert (funcall my-oddp 10009))
  (assert (not (funcall my-evenp 10009)))
  (assert (not (funcall my-oddp 10000))))


