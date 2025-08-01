(assert (list? (list 12 2 3)))
(assert (list? nil))
(assert (list? (list 1)))
(assert (not (list? 1)))
(assert (not (list? (cons 1 2))))
(assert (not (list? '(1 2 . 3))))

(assert (cons? (list 1 2 3)))
(assert (cons? '(1 2 . 3)))
(assert (not (cons? '())))

(assert (null? nil))
(assert (null? '()))
(assert (not (null? #f)))

(assert (number? 1))
(assert (number? 1))
(assert (number? 1.1))
(assert (float? 1.1))
(assert (float? (float 1)))
(assert (not (float? 1)))
(assert (not (integer? 1.1)))

(assert (atom? nil))
(assert (atom? #t))
(assert (atom? 'a))
(assert (atom? 1))
(assert (atom? 1.1))
(assert (atom? #\c))
(assert (atom? "hello"))
(assert (not (atom? (cons 1 2))))
(assert (lambda () 1))
