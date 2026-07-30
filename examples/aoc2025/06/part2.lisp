;;; Advent of Code 2025 -- Day 6, Part 2
;;;
;;; Same sheet, read differently: every *character* column holds one number,
;;; its digits stacked top to bottom (blanks skipped), and the bottom row holds
;;; the operators. Walking the columns from right to left, each operator folds
;;; the numbers collected since the previous one; the column results are added.
;;;
;;; Usage: bamboo-lisp part2.lisp < input

;; Rows are used as a character grid, so trailing blanks matter: keep the lines
;; unstripped and pad them all to the same width.
(defun read-rows (input-lines)
  (let ((rows nil)
        (width 0))
    (dolist (line input-lines)
      (when (zero? (string-length line))
        (break))
      (setq rows (cons line rows))
      (setq width (max width (string-length line))))
    (setq rows (nreverse rows))
    (when (null? rows)
      (error "read-rows: empty input."))
    (let ((blanks (make-vector))
          (padding ""))
      (dotimes (i width)
        (vector-append blanks #\space))
      (setq padding (list->string (vector->list blanks)))
      (cons (map (lambda (row)
                   (concat row (substring padding 0 (- width (string-length row)))))
                 rows)
            width))))

;; The digits of column X, read downwards; nil when the column has none.
(defun column-number (rows x)
  (let ((value nil))
    (dolist (row rows)
      (let ((c (string-ref row x)))
        (when (numeric? c)
          (setq value (+ (* (if (null? value) 0 value) 10)
                         (- (char->int c) 48))))))
    value))

(defun solve (input-lines)
  (let ((input (read-rows input-lines))
        (rows nil) (width nil) (number-rows nil) (operator-row nil)
        (pending nil) (total 0) (x nil))
    (setq rows (car input))
    (setq width (cdr input))
    (setq operator-row (last rows))
    (setq number-rows (take (- (length rows) 1) rows))
    (setq x (- width 1))
    (while (>= x 0)
      (let ((value (column-number number-rows x))
            (operator (string-ref operator-row x)))
        (unless (null? value)
          (setq pending (cons value pending)))
        (when (char= operator #\+)
          (setq total (+ total (foldl #'+ 0 pending)))
          (setq pending nil))
        (when (char= operator #\*)
          (setq total (+ total (foldl #'* 1 pending)))
          (setq pending nil)))
      (decq x))
    total))

(princ (solve (lines)))
(princ "\n")
