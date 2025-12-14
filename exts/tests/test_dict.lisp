(loadext "./dict.so")

(defvar d (make-dict))

(dict-set d "a" 1)
(dict-set d "b" 2)
(dict-set d "c" 3)

(assert (equal? (dict-keys d) (list "a" "b" "c")))
(assert (equal? (dict-get d "a") 1))
(assert (equal? (dict-get d "b") 2))
(dict-set d "a" 5)
(assert (equal? (dict-get d "a") 5))

(dict-remove d "b")

(assert (equal? (dict-keys d) (list "a" "c")))