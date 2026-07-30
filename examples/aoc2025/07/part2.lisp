;;; Advent of Code 2025 -- Day 7, Part 2
;;;
;;; Now count the distinct paths the beam can take from the single 'S' down and
;;; out of the bottom of the grid: a splitter doubles the beam, a '.' passes it
;;; through.
;;;
;;; The reference implementation recurses with a memo table; the same counts fall
;;; out of a single sweep upwards. `below` holds, for every column, the number of
;;; paths leaving the grid from the row underneath the one being processed.
;;;
;;; Usage: bamboo-lisp part2.lisp < input

(defun read-rows (input-lines)
  (let ((rows nil))
    (dolist (line input-lines)
      (let ((row (strip-string line)))
        (when (zero? (string-length row))
          (break))
        (setq rows (cons row rows))))
    (nreverse rows)))

(defun find-start (row)
  (dotimes (x (string-length row))
    (when (char= (string-ref row x) #\S)
      (return x)))
  (error "find-start: no 'S' in the first row."))

(defun solve (input-lines)
  (let ((rows (read-rows input-lines))
        (grid nil) (height nil) (width nil) (start nil) (below nil) (d nil))
    (when (null? rows)
      (error "solve: empty input."))
    (setq grid (list->vector rows))
    (setq height (vector-length grid))
    (setq width (string-length (vector-ref grid 0)))
    (setq start (find-start (vector-ref grid 0)))
    ;; one row past the bottom: every column has exactly one way out
    (setq below (make-vector width 1))
    (setq d (- height 1))
    (while (>= d 1)
      (let ((row (vector-ref grid d))
            (here (make-vector width 0)))
        (dotimes (x width)
          (let ((c (string-ref row x)))
            (cond ((char= c #\.) (vector-set here x (vector-ref below x)))
                  ((char= c #\^)
                   (progn
                     (when (or (< (- x 1) 0) (>= (+ x 1) width))
                       (error "solve: a beam is split off the edge of the map."))
                     (vector-set here x (+ (vector-ref below (- x 1))
                                           (vector-ref below (+ x 1))))))
                  (#t (error "solve: unexpected map character.")))))
        (setq below here))
      (decq d))
    (vector-ref below start)))

(princ (solve (lines)))
(princ "\n")
