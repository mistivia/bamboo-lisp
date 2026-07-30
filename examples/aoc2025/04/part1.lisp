;;; Advent of Code 2025 -- Day 4, Part 1
;;;
;;; The map is a grid of paper sheets ('@') and gaps ('.'). A sheet can be
;;; lifted when fewer than 4 of its 8 neighbours are sheets. Count them.
;;;
;;; The grid is stored as one flat vector with a one-cell border of '.', so
;;; neighbour lookups never need a bounds check.
;;;
;;; Usage: bamboo-lisp part1.lisp < input

(defvar paper #\@)

;; Read the map into (grid width height stride); the border makes the row
;; stride two wider than the map itself.
(defun read-grid (input-lines)
  (let ((rows nil))
    (dolist (line input-lines)
      (let ((row (strip-string line)))
        (when (zero? (string-length row))
          (break))
        (setq rows (cons row rows))))
    (setq rows (nreverse rows))
    (when (null? rows)
      (error "read-grid: empty map."))
    (let ((height (length rows))
          (width (string-length (car rows)))
          (stride (+ (string-length (car rows)) 2))
          (grid nil)
          (y 0))
      (setq grid (make-vector (* stride (+ height 2)) #\.))
      (dolist (row rows)
        (dotimes (x width)
          (vector-set grid (+ (* (+ y 1) stride) x 1) (string-ref row x)))
        (incq y))
      (list grid width height stride))))

(defun count-adjacent (grid stride idx)
  (let ((count 0)
        (up (- idx stride))
        (down (+ idx stride)))
    (when (char= (vector-ref grid (- up 1)) paper) (incq count))
    (when (char= (vector-ref grid up) paper) (incq count))
    (when (char= (vector-ref grid (+ up 1)) paper) (incq count))
    (when (char= (vector-ref grid (- idx 1)) paper) (incq count))
    (when (char= (vector-ref grid (+ idx 1)) paper) (incq count))
    (when (char= (vector-ref grid (- down 1)) paper) (incq count))
    (when (char= (vector-ref grid down) paper) (incq count))
    (when (char= (vector-ref grid (+ down 1)) paper) (incq count))
    count))

(defun solve (input-lines)
  (let ((map (read-grid input-lines))
        (grid nil) (width nil) (height nil) (stride nil)
        (count 0))
    (setq grid (car map))
    (setq width (cadr map))
    (setq height (caddr map))
    (setq stride (cadddr map))
    (dotimes (y height)
      (dotimes (x width)
        (let ((idx (+ (* (+ y 1) stride) x 1)))
          (when (and (char= (vector-ref grid idx) paper)
                     (< (count-adjacent grid stride idx) 4))
            (incq count)))))
    count))

(princ (solve (lines)))
(princ "\n")
