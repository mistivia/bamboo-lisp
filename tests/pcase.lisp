;; pcase: pattern matching (subset of Emacs Lisp pcase)

;; binding + wildcard
(assert (= (pcase 42 (x x)) 42))
(assert (equal? (pcase '(1 2) (_ 'anything)) 'anything))

;; literals and quoted symbols
(assert (string= (pcase 'foo ('foo "F") ('bar "B") (_ "?")) "F"))
(assert (string= (pcase 'bar ('foo "F") ('bar "B") (_ "?")) "B"))
(assert (string= (pcase 0 (0 "zero") (_ "no")) "zero"))
(assert (string= (pcase "hi" ("hi" "greeting") (_ "no")) "greeting"))

;; pred / guard
(assert (string= (pcase 7 ((pred integer?) "int") (_ "no")) "int"))
(assert (string= (pcase 7 ((and n (guard (> n 5))) "big") (_ "small")) "big"))
(assert (string= (pcase 3 ((and n (guard (> n 5))) "big") (_ "small")) "small"))

;; or
(assert (string= (pcase 2 ((or 1 2 3) "low") (_ "hi")) "low"))
(assert (string= (pcase 9 ((or 1 2 3) "low") (_ "hi")) "hi"))

;; backquote structural destructuring
(assert (= (pcase '(1 2) (`(,a ,b) (+ a b))) 3))
(assert (equal? (pcase '(add 3 4) (`(add ,x ,y) (list 'sum (+ x y)))) '(sum 7)))
(assert (string= (pcase '(1 2 3) (`(,a ,b) "two") (`(,a ,b ,c) "three")) "three"))

;; head . tail destructuring
(assert (equal? (pcase '(1 2 3 4) (`(,h . ,t) t)) '(2 3 4)))
(assert (= (pcase '(10) (`(,h . ,t) h)) 10))

;; nested backquote
(assert (= (pcase '(point (3 4)) (`(point (,x ,y)) (+ x y))) 7))

;; no clause matches -> nil
(assert (null? (pcase 5 (1 'a) (2 'b))))

;; used to write a tiny expression evaluator
(defun pcase-test-eval (e)
  (pcase e
    ((pred integer?) e)
    (`(+ ,a ,b) (+ (pcase-test-eval a) (pcase-test-eval b)))
    (`(* ,a ,b) (* (pcase-test-eval a) (pcase-test-eval b)))
    (_ (error "bad expr"))))
(assert (= (pcase-test-eval '(+ 1 (* 2 3))) 7))
(assert (= (pcase-test-eval '(* (+ 1 2) (+ 3 4))) 21))
