;;; Advent of Code 2025 -- Day 5, Part 1
;;;
;;; The input is a list of "start-end" ranges, a blank line, then a list of
;;; ids. Count the ids covered by at least one range.
;;;
;;; Usage: bamboo-lisp part1.lisp < input

;; "12-34" -> (12 . 34)
(defun parse-range (line)
  (let ((ends (split-string line #\-)))
    (unless (= (length ends) 2)
      (error "parse-range: bad range."))
    (cons (string->number (car ends))
          (string->number (cadr ends)))))

;; Split the input at the blank line into (ranges . ids).
(defun parse-input (input-lines)
  (let ((ranges nil)
        (ids nil)
        (in-ids #f))
    (dolist (line input-lines)
      (let ((text (strip-string line)))
        (cond ((zero? (string-length text)) (setq in-ids #t))
              (in-ids (setq ids (cons (string->number text) ids)))
              (#t (setq ranges (cons (parse-range text) ranges))))))
    (cons (nreverse ranges) (nreverse ids))))

(defun covered? (id ranges)
  (dolist (range ranges)
    (when (and (>= id (car range))
               (<= id (cdr range)))
      (return #t)))
  #f)

(defun solve (input-lines)
  (let ((input (parse-input input-lines))
        (ranges nil)
        (count 0))
    (setq ranges (car input))
    (dolist (id (cdr input))
      (when (covered? id ranges)
        (incq count)))
    count))

(princ (solve (lines)))
(princ "\n")
