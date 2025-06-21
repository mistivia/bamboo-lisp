(assert-error (let ((i 0)) (i > 4)))

(assert (= 3
(let ((a 1)
      (b 2))
  (+ a b))))

