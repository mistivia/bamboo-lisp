;;; Advent of Code 2025 -- Day 1, Part 1
;;;
;;; The dial starts at 50 and has 100 notches (0..99). Each input line turns it
;;; right ("R17") or left ("L28") by that many notches. Count the turns that
;;; leave the dial pointing at 0.
;;;
;;; Usage: bamboo-lisp part1.lisp < input

;; "R17" -> 17, "L28" -> -28, nil when the line holds no turn.
(defun parse-turn (line)
  (if (< (string-length line) 2)
      nil
      (let ((n (string->number (substring line 1))))
        (cond ((null? n) nil)
              ((char= (string-ref line 0) #\L) (- n))
              (#t n)))))

(defun solve (input-lines)
  (let ((dial 50)
        (count 0))
    (dolist (line input-lines)
      (let ((turn (parse-turn line)))
        (when (null? turn)
          (continue))
        ;; `mod` is floored, so a left turn past 0 wraps around by itself.
        (setq dial (mod (+ dial turn) 100))
        (when (zero? dial)
          (incq count))))
    count))

(princ (solve (lines)))
(princ "\n")
