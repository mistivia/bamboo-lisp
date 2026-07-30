;;; Advent of Code 2025 -- Day 9, Part 1
;;;
;;; Every input line is a tile "x,y". Take two tiles as opposite corners of a
;;; rectangle and report the largest number of tiles it covers.
;;;
;;; Usage: bamboo-lisp part1.lisp < input

;; "12,34" per line -> (xs ys count)
(defun read-tiles (input-lines)
  (let ((xs (make-vector))
        (ys (make-vector)))
    (dolist (line input-lines)
      (let ((text (strip-string line)))
        (when (zero? (string-length text))
          (break))
        (let ((fields (split-string text #\,)))
          (unless (= (length fields) 2)
            (error "read-tiles: bad coordinate."))
          (vector-append xs (string->number (car fields)))
          (vector-append ys (string->number (cadr fields))))))
    (list xs ys (vector-length xs))))

(defun solve (input-lines)
  (let ((tiles (read-tiles input-lines))
        (xs nil) (ys nil) (count nil) (best 0) (i 0))
    (setq xs (car tiles))
    (setq ys (cadr tiles))
    (setq count (caddr tiles))
    (while (< i count)
      (let ((xi (vector-ref xs i))
            (yi (vector-ref ys i))
            (j (+ i 1)))
        (while (< j count)
          (let ((area (* (+ (abs (- xi (vector-ref xs j))) 1)
                         (+ (abs (- yi (vector-ref ys j))) 1))))
            (when (> area best)
              (setq best area)))
          (incq j)))
      (incq i))
    best))

(princ (solve (lines)))
(princ "\n")
