;;; Advent of Code 2025 -- Day 5, Part 2
;;;
;;; How many ids does at least one range cover? Sort the ranges by their start,
;;; walk them once merging whatever overlaps, and add up the merged widths.
;;;
;;; Usage: bamboo-lisp part2.lisp < input

;; "12-34" -> (12 . 34)
(defun parse-range (line)
  (let ((ends (split-string line #\-)))
    (unless (= (length ends) 2)
      (error "parse-range: bad range."))
    (cons (string->number (car ends))
          (string->number (cadr ends)))))

;; The ranges are the lines before the blank one; the ids after it don't matter
;; for this part.
(defun parse-ranges (input-lines)
  (let ((ranges nil))
    (dolist (line input-lines)
      (let ((text (strip-string line)))
        (when (zero? (string-length text))
          (break))
        (setq ranges (cons (parse-range text) ranges))))
    (nreverse ranges)))

(defun range-start< (a b)
  (< (car a) (car b)))

;; Total number of ids covered by the union of RANGES.
(defun covered-count (ranges)
  (if (null? ranges)
      0
      (let ((sorted (sort ranges #'range-start<))
            (total 0)
            (start nil)
            (end nil))
        (setq start (caar sorted))
        (setq end (cdar sorted))
        (dolist (range (cdr sorted))
          (if (> (car range) end)
              (progn (setq total (+ total (- end start) 1))   ; disjoint: bank it
                     (setq start (car range))
                     (setq end (cdr range)))
              (setq end (max end (cdr range)))))
        (+ total (- end start) 1))))

(princ (covered-count (parse-ranges (lines))))
(princ "\n")
