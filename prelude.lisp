(defvar nil '())

(defvar pi 3.1415926)
(defvar e 2.718281828)

(defmacro incq (i)
  `(setq ,i (+ ,i 1)))

(defmacro decq (i)
  `(setq ,i (- ,i 1)))

(defun zero? (x) (= x 0))
(defun plus? (x) (> x 0))
(defun minus? (x) (< x 0))

(defmacro when (pred . body)
  `(if ,pred
     (progn ,@body)
     nil))

(defmacro unless (pred . body)
  `(if ,pred
     nil
     (progn ,@body)))

(defun take (n lst)
  (unless (integer? n)
    (error "take: type error."))
  (unless (list? lst)
    (error "take: type error."))
  (let ((i 0)
        (newlst nil))
    (while (and (< i n)
                (not (null? lst)))
      (setq newlst (cons (car lst) newlst))
      (setq lst (cdr lst))
      (incq i))
    (nreverse newlst)))

(defun drop (n lst)
  (unless (integer? n)
    (error "drop type error."))
  (unless (list? lst)
    (error "drop: type error."))
  (let ((i 0))
    (while (and (< i n)
                (not (null? lst)))
      (setq lst (cdr lst))
      (incq i))
    lst))

(defun take-while (pred lst)
  (unless (function? pred)
    (error "take-while: type error."))
  (unless (list? lst)
    (error "take-while: type error."))
  (let ((newlst nil))
    (while (and (not (null? lst))
                (funcall pred (car lst)))
      (setq newlst (cons (car lst) newlst))
      (setq lst (cdr lst)))
    (nreverse newlst)))

(defun drop-while (pred lst)
  (unless (function? pred)
    (error "drop-while: type error."))
  (unless (list? lst)
    (error "drop-while: type error."))
  (while (and (not (null? lst))
              (funcall pred (car lst)))
    (setq lst (cdr lst)))
  lst)

(defun sublist (start end lst)
  (unless (integer? start)
    (error "sublist: type error."))
  (unless (integer? end)
    (error "sublist: type error."))
  (unless (< start end)
    (error "sublist: start must less than end."))
  (unless (list? lst)
    (error "sublist: type error."))
  (drop start (take end lst)))

(defun find (x lst)
  (unless (list? lst)
    (error "find: type error."))
  (while (not (null? lst))
    (when (equal? x (car lst))
      (return lst))
    (setq lst (cdr lst)))
  nil)

(defun contains? (x lst)
  (unless (list? lst)
    (error "contains?: type error."))
  (while (not (null? lst))
    (when (equal? x (car lst))
      (return #t))
    (setq lst (cdr lst)))
  #f)

(defun caar (x) (car (car x)))
(defun cadr (x) (car (cdr x)))
(defun cddr (x) (cdr (cdr x)))
(defun cdar (x) (cdr (car x)))

(defun caaar (x) (car (caar x)))
(defun cadar (x) (car (cdar x)))
(defun cddar (x) (cdr (cdar x)))
(defun cdaar (x) (cdr (caar x)))
(defun caadr (x) (car (cadr x)))
(defun caddr (x) (car (cddr x)))
(defun cdddr (x) (cdr (cddr x)))
(defun cdadr (x) (cdr (cadr x)))

(defun caaaar (x) (car (caaar x)))
(defun cadaar (x) (car (cdaar x)))
(defun cddaar (x) (cdr (cdaar x)))
(defun cdaaar (x) (cdr (caaar x)))
(defun caadar (x) (car (cadar x)))
(defun caddar (x) (car (cddar x)))
(defun cdddar (x) (cdr (cddar x)))
(defun cdadar (x) (cdr (cadar x)))
(defun caaadr (x) (car (caadr x)))
(defun cadadr (x) (car (cdadr x)))
(defun cddadr (x) (cdr (cdadr x)))
(defun cdaadr (x) (cdr (caadr x)))
(defun caaddr (x) (car (caddr x)))
(defun cadddr (x) (car (cdddr x)))
(defun cddddr (x) (cdr (cdddr x)))
(defun cdaddr (x) (cdr (caddr x)))

;; ---------------------------------------------------------------------------
;; pcase: ML-style pattern matching, a subset of Emacs Lisp's `pcase`.
;;
;;   (pcase EXPR
;;     (PATTERN BODY...)
;;     ...)
;;
;; EXPR is evaluated once; clauses are tried in order and the body of the
;; first matching pattern is run (with the pattern's variables bound). If no
;; clause matches the result is nil.
;;
;; Supported patterns:
;;   _                wildcard, matches anything
;;   SYMBOL           binds SYMBOL to the value
;;   nil              matches the empty list
;;   123 "s" #\c #t   self-evaluating literal, matched with equal?
;;   'VALUE           matches a value equal? to VALUE (e.g. a literal symbol)
;;   (pred FN)        matches when (FN value) is true
;;   (guard EXPR)     matches when EXPR (using already-bound vars) is true
;;   (and PAT...)     matches when every PAT matches
;;   (or PAT...)      matches when some PAT matches
;;   `TEMPLATE        structural match; ,PAT matches/binds a position,
;;                    e.g. `(,a ,b) or `(,head . ,tail) or `(tag ,x)
;;
;; The helpers below run at macro-expansion time to compile a pattern into
;; nested if/let code. ACC is a side-effect-free accessor expression for the
;; value being matched; SUCC / FAIL are the code to run on match / mismatch.
;; ---------------------------------------------------------------------------

(defun pcase-body (body)
  (if (null? body) 'nil (cons 'progn body)))

(defun pcase-compile-and (pats acc succ fail)
  (if (null? pats)
      succ
      (pcase-compile (car pats) acc
                     (pcase-compile-and (cdr pats) acc succ fail)
                     fail)))

(defun pcase-compile-or (pats acc succ fail)
  (if (null? pats)
      fail
      (pcase-compile (car pats) acc
                     succ
                     (pcase-compile-or (cdr pats) acc succ fail))))

(defun pcase-compile-bq (tmpl acc succ fail)
  (cond
    ((null? tmpl) `(if (null? ,acc) ,succ ,fail))
    ((cons? tmpl)
       (if (eq? (car tmpl) 'unquote)
           (pcase-compile (cadr tmpl) acc succ fail)
           `(if (cons? ,acc)
                ,(pcase-compile-bq (car tmpl) `(car ,acc)
                     (pcase-compile-bq (cdr tmpl) `(cdr ,acc) succ fail)
                     fail)
                ,fail)))
    (#t `(if (equal? ,acc ,(list 'quote tmpl)) ,succ ,fail))))

(defun pcase-compile (pat acc succ fail)
  (cond
    ((eq? pat '_) succ)
    ((null? pat) `(if (null? ,acc) ,succ ,fail))
    ((symbol? pat) `(let ((,pat ,acc)) ,succ))
    ((atom? pat) `(if (equal? ,acc ,pat) ,succ ,fail))
    ((eq? (car pat) 'quote) `(if (equal? ,acc ,pat) ,succ ,fail))
    ((eq? (car pat) 'pred)
       (let ((fn (cadr pat)))
         (if (symbol? fn)
             `(if (,fn ,acc) ,succ ,fail)
             `(if (funcall ,fn ,acc) ,succ ,fail))))
    ((eq? (car pat) 'guard) `(if ,(cadr pat) ,succ ,fail))
    ((eq? (car pat) 'and) (pcase-compile-and (cdr pat) acc succ fail))
    ((eq? (car pat) 'or) (pcase-compile-or (cdr pat) acc succ fail))
    ((eq? (car pat) 'quasiquote) (pcase-compile-bq (cadr pat) acc succ fail))
    (#t (error "pcase: unknown pattern."))))

;; Chain the clauses. Each clause's mismatch continuation is a thunk so the
;; "rest of the clauses" is not duplicated across a pattern's several failure
;; points.
(defun pcase-compile-clauses (clauses vsym)
  (if (null? clauses)
      'nil
      (let ((pat (caar clauses))
            (body (cdar clauses))
            (fsym (gensym)))
        `(let ((,fsym (lambda () ,(pcase-compile-clauses (cdr clauses) vsym))))
           ,(pcase-compile pat vsym (pcase-body body) `(funcall ,fsym))))))

(defmacro pcase (expr . clauses)
  (let ((vsym (gensym)))
    `(let ((,vsym ,expr))
       ,(pcase-compile-clauses clauses vsym))))
