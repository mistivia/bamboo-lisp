;;; Advent of Code 2025 -- Day 7, Part 1
;;;
;;; A beam starts at each 'S' in the top row and travels down. A splitter '^'
;;; sends it to both diagonal neighbours; '.' lets it pass. Count how many
;;; splitters get hit.
;;;
;;; Two beams in the same column are indistinguishable, so each row only needs
;;; the *set* of occupied columns, kept as a vector of flags.
;;;
;;; Usage: bamboo-lisp part1.lisp < input

(defun read-rows (input-lines)
  (let ((rows nil))
    (dolist (line input-lines)
      (let ((row (strip-string line)))
        (when (zero? (string-length row))
          (break))
        (setq rows (cons row rows))))
    (nreverse rows)))

(defun solve (input-lines)
  (let ((rows (read-rows input-lines))
        (width nil)
        (beams nil)
        (hits 0))
    (when (null? rows)
      (error "solve: empty input."))
    (setq width (string-length (car rows)))
    (setq beams (make-vector width #f))
    (dotimes (x width)
      (when (char= (string-ref (car rows) x) #\S)
        (vector-set beams x #t)))
    (dolist (row (cdr rows))
      (let ((next (make-vector width #f)))
        (dotimes (x width)
          (when (vector-ref beams x)
            (let ((c (string-ref row x)))
              (cond ((char= c #\.) (vector-set next x #t))
                    ((char= c #\^)
                     (progn (incq hits)
                            (when (>= (- x 1) 0) (vector-set next (- x 1) #t))
                            (when (< (+ x 1) width) (vector-set next (+ x 1) #t))))
                    (#t (error "solve: unexpected map character."))))))
        (setq beams next)))
    hits))

(princ (solve (lines)))
(princ "\n")
