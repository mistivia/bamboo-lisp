;;; Advent of Code 2025 -- Day 6, Part 1
;;;
;;; A block of numbers with a row of operators underneath. Each column is
;;; folded with its operator (+ or *) and the column results are added up.
;;;
;;; Usage: bamboo-lisp part1.lisp < input

(defun parse-numbers (line)
  (let ((nums nil))
    (dolist (field (split-string line #\space))
      (let ((n (string->number field)))
        (unless (null? n)
          (setq nums (cons n nums)))))
    (nreverse nums)))

(defun parse-operators (line)
  (filter (lambda (c) (or (char= c #\+) (char= c #\*)))
          (string->list line)))

;; The last non-empty line holds the operators, the ones above it the numbers.
(defun parse-input (input-lines)
  (let ((rows nil)
        (operators nil))
    (dolist (line input-lines)
      (let ((text (strip-string line)))
        (when (zero? (string-length text))
          (break))
        (if (numeric? (string-ref text 0))
            (setq rows (cons (parse-numbers text) rows))
            (progn (setq operators (parse-operators text))
                   (break)))))
    (cons (nreverse rows) operators)))

(defun fold-column (operator values)
  (if (char= operator #\+)
      (foldl #'+ 0 values)
      (foldl #'* 1 values)))

(defun solve (input-lines)
  (let ((input (parse-input input-lines))
        (rows nil)
        (column 0)
        (total 0))
    (setq rows (car input))
    (dolist (operator (cdr input))
      (setq total (+ total (fold-column operator
                                        (map (lambda (row) (nth column row)) rows))))
      (incq column))
    total))

(princ (solve (lines)))
(princ "\n")
