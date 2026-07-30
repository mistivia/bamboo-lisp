;;; Advent of Code 2025 -- Day 8, Part 2
;;;
;;; Keep wiring up the closest pairs until every junction is on one circuit;
;;; report the product of the x coordinates of the two junctions joined by the
;;; wire that finished the job.
;;;
;;; Adding the shortest wires until everything is connected is Kruskal's
;;; algorithm, and the wire that completes it is the heaviest edge of a minimum
;;; spanning tree. Sorting ~500k pairs is out of reach here, so we grow the tree
;;; with Prim's algorithm instead -- O(n^2) with no sorting and no edge list --
;;; and remember its longest edge.
;;;
;;; Usage: bamboo-lisp part2.lisp < input

;; "12,34,56" per line -> (xs ys zs count)
(defun read-junctions (input-lines)
  (let ((xs (make-vector))
        (ys (make-vector))
        (zs (make-vector)))
    (dolist (line input-lines)
      (let ((text (strip-string line)))
        (when (zero? (string-length text))
          (break))
        (let ((fields (split-string text #\,)))
          (unless (= (length fields) 3)
            (error "read-junctions: bad coordinate."))
          (vector-append xs (string->number (car fields)))
          (vector-append ys (string->number (cadr fields)))
          (vector-append zs (string->number (caddr fields))))))
    (list xs ys zs (vector-length xs))))

;; The heaviest edge of a minimum spanning tree, as (junction-a . junction-b).
(defun longest-mst-edge (xs ys zs count)
  (let ((in-tree (make-vector count #f))
        (best (make-vector count 0))         ; distance to the tree
        (from (make-vector count 0))         ; tree node that is that close
        (worst-a 0) (worst-b 0) (worst -1)
        (added 1))
    (vector-set in-tree 0 #t)
    (dotimes (v count)
      (let ((dx (- (vector-ref xs 0) (vector-ref xs v)))
            (dy (- (vector-ref ys 0) (vector-ref ys v)))
            (dz (- (vector-ref zs 0) (vector-ref zs v))))
        (vector-set best v (+ (* dx dx) (* dy dy) (* dz dz)))))
    (while (< added count)
      ;; the junction closest to the tree joins it next
      (let ((pick -1)
            (pick-dist -1)
            (v 0))
        (while (< v count)
          (unless (vector-ref in-tree v)
            (when (or (= pick -1) (< (vector-ref best v) pick-dist))
              (setq pick v)
              (setq pick-dist (vector-ref best v))))
          (incq v))
        (vector-set in-tree pick #t)
        (incq added)
        (when (> pick-dist worst)
          (setq worst pick-dist)
          (setq worst-a (vector-ref from pick))
          (setq worst-b pick))
        ;; ... and everyone else may now be closer to the tree
        (let ((px (vector-ref xs pick))
              (py (vector-ref ys pick))
              (pz (vector-ref zs pick))
              (u 0))
          (while (< u count)
            (unless (vector-ref in-tree u)
              (let ((dx (- px (vector-ref xs u)))
                    (dy (- py (vector-ref ys u)))
                    (dz (- pz (vector-ref zs u)))
                    (d 0))
                (setq d (+ (* dx dx) (* dy dy) (* dz dz)))
                (when (< d (vector-ref best u))
                  (vector-set best u d)
                  (vector-set from u pick))))
            (incq u)))))
    (cons worst-a worst-b)))

(defun solve (input-lines)
  (let ((junctions (read-junctions input-lines))
        (xs nil) (ys nil) (zs nil) (count nil) (edge nil))
    (setq xs (car junctions))
    (setq ys (cadr junctions))
    (setq zs (caddr junctions))
    (setq count (cadddr junctions))
    (when (< count 2)
      (error "solve: need at least two junctions."))
    (setq edge (longest-mst-edge xs ys zs count))
    (* (vector-ref xs (car edge)) (vector-ref xs (cdr edge)))))

(princ (solve (lines)))
(princ "\n")
