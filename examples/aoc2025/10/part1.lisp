;;; Advent of Code 2025 -- Day 10, Part 1
;;;
;;; Each line is a machine: an indicator like [##.....#.], then the buttons.
;;; Pressing a button toggles the lights it lists. Find the fewest presses that
;;; turn the given lights on, and add that up over all machines.
;;;
;;; Pressing a button twice cancels out, so a solution is a *set* of buttons and
;;; the lights are a bitmask xor. The reference implementation tries all 2^n
;;; subsets; instead we keep, for every light pattern reachable so far, the
;;; fewest presses that reach it, and fold the buttons in one at a time.
;;;
;;; Usage: bamboo-lisp part1.lisp < input

;; Bit N of the result is set for the Nth '#'.
(defun parse-indicator (token)
  (let ((mask 0)
        (body (substring token 1 (- (string-length token) 1))))
    (dotimes (i (string-length body))
      (when (char= (string-ref body i) #\#)
        (setq mask (logior mask (lsh 1 i)))))
    (cons mask (string-length body))))

;; "(0,1,5,8)" -> the mask of the lights it toggles.
(defun parse-button (token)
  (let ((mask 0))
    (dolist (field (split-string (substring token 1 (- (string-length token) 1)) #\,))
      (setq mask (logior mask (lsh 1 (string->number field)))))
    mask))

;; Fewest buttons whose combined toggles equal TARGET, or nil if unreachable.
(defun fewest-presses (target lights buttons)
  (let ((states (lsh 1 lights))
        (best nil))
    (setq best (make-vector states -1))
    (vector-set best 0 0)
    (dolist (button buttons)
      (let ((next (make-vector states -1)))
        (dotimes (state states)
          (let ((presses (vector-ref best state)))
            (when (>= presses 0)
              ;; without this button ...
              (when (or (< (vector-ref next state) 0)
                        (> (vector-ref next state) presses))
                (vector-set next state presses))
              ;; ... and with it
              (let ((toggled (logxor state button)))
                (when (or (< (vector-ref next toggled) 0)
                          (> (vector-ref next toggled) (+ presses 1)))
                  (vector-set next toggled (+ presses 1)))))))
        (setq best next)))
    (let ((presses (vector-ref best target)))
      (if (< presses 0) nil presses))))

(defun solve-line (line)
  (let ((target 0)
        (lights 0)
        (buttons nil))
    (dolist (token (split-string line #\space))
      (when (plus? (string-length token))
        (let ((kind (string-ref token 0)))
          (cond ((char= kind #\[)
                 (let ((indicator (parse-indicator token)))
                   (setq target (car indicator))
                   (setq lights (cdr indicator))))
                ((char= kind #\() (setq buttons (cons (parse-button token) buttons)))
                (#t nil)))))       ; the {...} joltages only matter in part 2
    (let ((presses (fewest-presses target lights (nreverse buttons))))
      (when (null? presses)
        (error "solve-line: no combination of buttons works."))
      presses)))

(defun solve (input-lines)
  (let ((total 0))
    (dolist (line input-lines)
      (let ((text (strip-string line)))
        (when (plus? (string-length text))
          (setq total (+ total (solve-line text))))))
    total))

(princ (solve (lines)))
(princ "\n")
