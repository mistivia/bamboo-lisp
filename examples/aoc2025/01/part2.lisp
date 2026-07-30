;;; Advent of Code 2025 -- Day 1, Part 2
;;;
;;; Same dial as part 1, but now count every time the pointer passes over (or
;;; stops on) 0 -- a single turn may sweep around the dial several times.
;;;
;;; Usage: bamboo-lisp part2.lisp < input

;; "R17" -> 17, "L28" -> -28, nil when the line holds no turn.
(defun parse-turn (line)
  (if (< (string-length line) 2)
      nil
      (let ((n (string->number (substring line 1))))
        (cond ((null? n) nil)
              ((char= (string-ref line 0) #\L) (- n))
              (#t n)))))

;; Turning right from DIAL, 0 is reached after (100 - dial) notches and then
;; every 100 notches; that is just how many hundreds (dial + amount) spans.
(defun crossings-right (dial amount)
  (i/ (+ dial amount) 100))

;; Turning left from DIAL, 0 is reached after DIAL notches and then every 100.
;; Starting on 0 does not count as a crossing, so the first hit is 100 away.
(defun crossings-left (dial amount)
  (let ((first-hit (if (zero? dial) 100 dial)))
    (if (< amount first-hit)
        0
        (+ 1 (i/ (- amount first-hit) 100)))))

(defun solve (input-lines)
  (let ((dial 50)
        (count 0))
    (dolist (line input-lines)
      (let ((turn (parse-turn line)))
        (when (null? turn)
          (continue))
        (setq count (+ count (if (minus? turn)
                                 (crossings-left dial (- turn))
                                 (crossings-right dial turn))))
        (setq dial (mod (+ dial turn) 100))))
    count))

(princ (solve (lines)))
(princ "\n")
