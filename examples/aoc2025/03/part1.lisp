;;; Advent of Code 2025 -- Day 3, Part 1
;;;
;;; Each line is a row of digits. Take two of them, keeping their order, to get
;;; the largest possible two digit number; sum that over all lines.
;;;
;;; Picking the largest k-digit subsequence is the classic monotonic stack
;;; scan: a digit already kept is dropped as soon as a bigger one shows up,
;;; provided enough digits are left to still fill k slots.
;;;
;;; Usage: bamboo-lisp part1.lisp < input

(defvar keep-digits 2)

;; Largest number (as a list of digits) that keeps K of DIGITS in order.
(defun max-subsequence (digits k)
  (let ((kept nil)          ; kept digits, most recent first
        (size 0)
        (remaining (length digits)))
    (dolist (d digits)
      ;; `remaining` counts d and everything after it
      (while (and kept
                  (< (car kept) d)
                  (> (+ size remaining) k))
        (setq kept (cdr kept))
        (decq size))
      (when (< size k)
        (setq kept (cons d kept))
        (incq size))
      (decq remaining))
    (nreverse kept)))

(defun digits-of (line)
  (map (lambda (c) (- (char->int c) 48)) (string->list line)))

(defun digits->number (digits)
  (foldl (lambda (acc d) (+ (* acc 10) d)) 0 digits))

(defun solve (input-lines)
  (let ((total 0))
    (dolist (line input-lines)
      (let ((digits (digits-of (strip-string line))))
        (when (< (length digits) keep-digits)
          (continue))
        (setq total (+ total (digits->number (max-subsequence digits keep-digits))))))
    total))

(princ (solve (lines)))
(princ "\n")
