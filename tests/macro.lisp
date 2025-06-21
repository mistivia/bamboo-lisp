(defmacro for (start pred inc . body)
  `(let (,start)
     (while ,pred
       ,@body
       ,inc)))
(assert (= 10
  (let ((sum 0))
    (for (i 0) (< i 5) (incq i)
      (setq sum (+ sum i)))
    sum)))
