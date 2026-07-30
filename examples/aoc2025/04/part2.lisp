;;; Advent of Code 2025 -- Day 4, Part 2
;;;
;;; Lifting a sheet frees its neighbours, so keep lifting until nothing can be
;;; lifted any more and report how many sheets were removed in total.
;;;
;;; Removals only ever lower neighbour counts, so the process has a single fixed
;;; point no matter in which order sheets are lifted. Rather than rescanning the
;;; whole grid on every round we keep a worklist: when a sheet goes away only
;;; its 8 neighbours can become liftable, so only those are re-checked.
;;;
;;; Usage: bamboo-lisp part2.lisp < input

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

(defun neighbours (stride idx)
  (let ((up (- idx stride))
        (down (+ idx stride)))
    (list (- up 1) up (+ up 1)
          (- idx 1) (+ idx 1)
          (- down 1) down (+ down 1))))

(defun count-adjacent (grid stride idx)
  (let ((count 0))
    (dolist (n (neighbours stride idx))
      (when (char= (vector-ref grid n) paper)
        (incq count)))
    count))

(defun liftable? (grid stride idx)
  (and (char= (vector-ref grid idx) paper)
       (< (count-adjacent grid stride idx) 4)))

(defun solve (input-lines)
  (let ((map (read-grid input-lines))
        (grid nil) (width nil) (height nil) (stride nil)
        (pending nil)
        (removed 0))
    (setq grid (car map))
    (setq width (cadr map))
    (setq height (caddr map))
    (setq stride (cadddr map))
    (dotimes (y height)
      (dotimes (x width)
        (let ((idx (+ (* (+ y 1) stride) x 1)))
          (when (liftable? grid stride idx)
            (setq pending (cons idx pending))))))
    (while (not (null? pending))
      (let ((idx (car pending)))
        (setq pending (cdr pending))
        ;; a cell can be queued twice; the first visit already lifted it
        (when (liftable? grid stride idx)
          (vector-set grid idx #\x)
          (incq removed)
          (dolist (n (neighbours stride idx))
            (when (liftable? grid stride n)
              (setq pending (cons n pending)))))))
    removed))

(princ (solve (lines)))
(princ "\n")
