;;; Advent of Code 2025 -- Day 10, Part 2
;;;
;;; The indicator no longer matters. Each button now adds one joltage to each
;;; port it lists, and the {...} block is the joltage every port must end at.
;;; Buttons may be pressed any number of times; find the fewest presses per
;;; machine and add those up.
;;;
;;; So per machine we need the minimum of (sum x) over the non-negative integer
;;; solutions of A x = b, where column i of A is button i's ports. Gauss-Jordan
;;; on A expresses the pivot variables in terms of the few remaining free ones,
;;; which are then enumerated.
;;;
;;; Two things keep that enumeration cheap. The elimination is done with integer
;;; row operations (each row keeps its own pivot divisor and is reduced by the
;;; gcd of its entries), so no rational arithmetic is needed; and the range of
;;; each free variable is squeezed by propagating "every pivot variable stays
;;; >= 0" until the bounds stop moving. The reference implementation instead
;;; walks every free variable from 0 to max(b) over fractions, which is about
;;; 200 times more points than the bounds allow.
;;;
;;; Usage: bamboo-lisp part2.lisp < input

(defun gcd2 (a b)
  (let ((x (abs a))
        (y (abs b)))
    (while (not (zero? y))
      (let ((r (mod x y)))
        (setq x y)
        (setq y r)))
    x))

;;; --- parsing ---------------------------------------------------------------

(defun parse-number-list (token)
  (map #'string->number
       (split-string (substring token 1 (- (string-length token) 1)) #\,)))

;; "[..] (0,2) (1,2) {3,4,5}" -> (buttons . joltages), buttons as port lists
(defun parse-machine (line)
  (let ((buttons nil)
        (joltages nil))
    (dolist (token (split-string line #\space))
      (when (plus? (string-length token))
        (let ((kind (string-ref token 0)))
          (cond ((char= kind #\() (setq buttons (cons (parse-number-list token) buttons)))
                ((char= kind #\{) (setq joltages (parse-number-list token)))
                (#t nil)))))      ; the [...] indicator belongs to part 1
    (cons (nreverse buttons) joltages)))

;;; --- integer Gauss-Jordan --------------------------------------------------

(defun row-reduce-by-gcd (row)
  (let ((g 0)
        (len (vector-length row)))
    (dotimes (j len)
      (setq g (gcd2 g (vector-ref row j))))
    (when (> g 1)
      (dotimes (j len)
        (vector-set row j (i/ (vector-ref row j) g))))))

;; Reduce ROWS (each a vector of WIDTH coefficients plus a right-hand side) so
;; that every pivot column holds a single nonzero entry. Returns the pivot
;; columns, in order; row p of the result belongs to the pth pivot column.
(defun row-reduce (rows width)
  (let ((height (vector-length rows))
        (pivots nil)
        (rank 0)
        (col 0))
    (while (< col width)
      (let ((pivot-row rank))
        (while (and (< pivot-row height)
                    (zero? (vector-ref (vector-ref rows pivot-row) col)))
          (incq pivot-row))
        (when (< pivot-row height)
          (let ((swap (vector-ref rows rank)))
            (vector-set rows rank (vector-ref rows pivot-row))
            (vector-set rows pivot-row swap))
          (let ((prow (vector-ref rows rank))
                (pval (vector-ref (vector-ref rows rank) col)))
            (dotimes (k height)
              (unless (= k rank)
                (let ((krow (vector-ref rows k))
                      (kval (vector-ref (vector-ref rows k) col)))
                  (unless (zero? kval)
                    (dotimes (j (+ width 1))
                      (vector-set krow j (- (* (vector-ref krow j) pval)
                                            (* (vector-ref prow j) kval))))
                    (row-reduce-by-gcd krow))))))
          (setq pivots (cons col pivots))
          (incq rank)))
      (incq col))
    (nreverse pivots)))

;;; --- solving one machine ---------------------------------------------------

;; Tighten UBOUNDS using "pivot variable p stays >= 0", i.e.
;;   sum_k coefficient(p,k) * f_k <= rhs(p)
;; A variable with a positive coefficient is bounded once the others are pushed
;; to whichever end of their range helps the constraint least.
(defun propagate-bounds (ubounds columns rhs rank free-count)
  (let ((changed #t)
        (rounds 0))
    (while (and changed (< rounds 32))
      (setq changed #f)
      (incq rounds)
      (dotimes (p rank)
        (dotimes (k free-count)
          (let ((a (vector-ref (vector-ref columns k) p)))
            (when (plus? a)
              (let ((slack (vector-ref rhs p)))
                (dotimes (l free-count)
                  (unless (= l k)
                    (let ((al (vector-ref (vector-ref columns l) p)))
                      (when (minus? al)
                        (setq slack (+ slack (* (- al) (vector-ref ubounds l))))))))
                (let ((bound (if (minus? slack) 0 (i/ slack a))))
                  (when (< bound (vector-ref ubounds k))
                    (vector-set ubounds k bound)
                    (setq changed #t)))))))))))

(defun add-column (num columns k times rank)
  (let ((col (vector-ref columns k)))
    (dotimes (p rank)
      (vector-set num p (+ (vector-ref num p) (* times (vector-ref col p)))))))

;; Total presses for the current free-variable assignment, or nil when a pivot
;; variable would come out negative or fractional. NUM holds the numerators.
(defun presses-for (num divisors rank free-sum)
  (let ((total free-sum)
        (feasible #t)
        (p 0))
    (while (< p rank)
      (let ((value (vector-ref num p)))
        (when (minus? value)
          (setq feasible #f)
          (break))
        (unless (zero? (mod value (vector-ref divisors p)))
          (setq feasible #f)
          (break))
        (setq total (+ total (i/ value (vector-ref divisors p)))))
      (incq p))
    (if feasible total nil)))

(defun fewest-presses (buttons joltages)
  (let ((height (length joltages))
        (width (length buttons))
        (rows nil) (pivots nil) (rank 0) (free nil) (free-count 0)
        (divisors nil) (rhs nil) (columns nil) (ubounds nil)
        (num nil) (assignment nil) (free-sum 0) (best nil))
    (when (or (zero? height) (zero? width))
      (error "fewest-presses: empty machine."))
    ;; row j of the system is port j: which buttons feed it, and its joltage
    (setq rows (make-vector height nil))
    (let ((j 0))
      (dolist (joltage joltages)
        (let ((row (make-vector (+ width 1) 0))
              (i 0))
          (dolist (ports buttons)
            (when (contains? j ports)
              (vector-set row i 1))
            (incq i))
          (vector-set row width joltage)
          (vector-set rows j row))
        (incq j)))
    (setq pivots (row-reduce rows width))
    (setq rank (length pivots))
    ;; a row without coefficients but with a joltage left over has no solution
    (let ((r rank))
      (while (< r height)
        (unless (zero? (vector-ref (vector-ref rows r) width))
          (error "fewest-presses: the machine cannot be balanced."))
        (incq r)))
    (dotimes (col width)
      (unless (contains? col pivots)
        (setq free (cons col free))))
    (setq free (nreverse free))
    (setq free-count (length free))
    ;; pivot variable p = (rhs[p] - sum_k columns[k][p] * f_k) / divisors[p]
    (setq divisors (make-vector rank 1))
    (setq rhs (make-vector rank 0))
    (setq columns (make-vector free-count nil))
    (dotimes (k free-count)
      (vector-set columns k (make-vector rank 0)))
    (let ((p 0))
      (dolist (col pivots)
        (let ((row (vector-ref rows p))
              (sign 1))
          (when (minus? (vector-ref row col))
            (setq sign -1))
          (vector-set divisors p (* sign (vector-ref row col)))
          (vector-set rhs p (* sign (vector-ref row width)))
          (let ((k 0))
            (dolist (fcol free)
              (vector-set (vector-ref columns k) p (* sign (vector-ref row fcol)))
              (incq k))))
        (incq p)))
    ;; a button is pressed at most max(b) times: any port it feeds bounds it
    (setq ubounds (make-vector free-count (apply #'max joltages)))
    (propagate-bounds ubounds columns rhs rank free-count)
    ;; walk the free variables like an odometer, keeping the numerators updated
    (setq num (make-vector rank 0))
    (dotimes (p rank)
      (vector-set num p (vector-ref rhs p)))
    (setq assignment (make-vector free-count 0))
    (while #t
      (let ((total (presses-for num divisors rank free-sum)))
        (when (and (not (null? total))
                   (or (null? best) (< total best)))
          (setq best total)))
      (let ((k (- free-count 1))
            (carry #t))
        (while (>= k 0)
          (if (< (vector-ref assignment k) (vector-ref ubounds k))
              (progn
                (vector-set assignment k (+ (vector-ref assignment k) 1))
                (add-column num columns k -1 rank)
                (incq free-sum)
                (setq carry #f)
                (break))
              (progn
                (add-column num columns k (vector-ref assignment k) rank)
                (setq free-sum (- free-sum (vector-ref assignment k)))
                (vector-set assignment k 0)
                (decq k))))
        (when carry
          (break))))
    (when (null? best)
      (error "fewest-presses: the machine cannot be balanced."))
    best))

(defun solve (input-lines)
  (let ((total 0))
    (dolist (line input-lines)
      (let ((text (strip-string line)))
        (when (plus? (string-length text))
          (let ((machine (parse-machine text)))
            (setq total (+ total (fewest-presses (car machine) (cdr machine))))))))
    total))

(princ (solve (lines)))
(princ "\n")
