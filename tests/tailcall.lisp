(defun is-even (x)
  (if (= x 0)
    #t
    (is-odd (- x 1))))

(defun is-odd (x)
    (is-even (- x 1)))

(assert (is-even 2050))
(assert (is-even 10))
(assert (is-even 0))
(assert (is-odd 1))
(assert (is-even 2))

(defun cnt-down (x)
  (if (= x 0)
    #t
    (progn
      (cnt-down (- x 1)))))

(cnt-down 100)

;; A tail call inside (nested) `let` bodies must not grow the stack.
(defun tc-through-let (n)
  (if (= n 0)
      'done
      (let ((m (- n 1)))
        (tc-through-let m))))
(assert (eq? 'done (tc-through-let 5000)))

(defun tc-through-two-lets (n)
  (if (= n 0)
      'done
      (let ((m (- n 1)))
        (let ((k m))
          (tc-through-two-lets k)))))
(assert (eq? 'done (tc-through-two-lets 5000)))

;; a `let` in tail position still returns its last value, and its bindings are
;; still visible to closures made inside it
(assert (= 3 (let ((x 1)) (let ((y 2)) (+ x y)))))
(assert (= 42 (funcall (let ((x 41)) (lambda () (+ x 1))))))
