;;; Advent of Code 2025 -- Day 8, Part 1
;;;
;;; Junctions in 3D. Wire up the 1000 closest pairs (squared distance), then
;;; multiply the sizes of the three biggest connected circuits.
;;;
;;; 999 junctions make ~500k pairs, far too many to sort in a tree-walking
;;; interpreter. We only ever need the 1000 shortest, so the pairs stream
;;; through a bounded max-heap: once it is full, a pair is only remembered when
;;; it beats the worst one kept so far.
;;;
;;; Usage: bamboo-lisp part1.lisp < input

(defvar wire-count 1000)

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

;; --- bounded max-heap over (distance, junction a, junction b) ---------------

(defun heap-swap (hd ha hb i j)
  (let ((d (vector-ref hd i))
        (a (vector-ref ha i))
        (b (vector-ref hb i)))
    (vector-set hd i (vector-ref hd j))
    (vector-set ha i (vector-ref ha j))
    (vector-set hb i (vector-ref hb j))
    (vector-set hd j d)
    (vector-set ha j a)
    (vector-set hb j b)))

(defun sift-down (hd ha hb size root)
  (let ((idx root))
    (while #t
      (let ((left (+ (* 2 idx) 1))
            (right (+ (* 2 idx) 2))
            (largest idx))
        (when (and (< left size)
                   (> (vector-ref hd left) (vector-ref hd largest)))
          (setq largest left))
        (when (and (< right size)
                   (> (vector-ref hd right) (vector-ref hd largest)))
          (setq largest right))
        (when (= largest idx)
          (break))
        (heap-swap hd ha hb idx largest)
        (setq idx largest)))))

;; The WIRE-COUNT shortest pairs, as (heap-a heap-b size).
(defun shortest-pairs (xs ys zs count)
  (let ((hd (make-vector wire-count 0))
        (ha (make-vector wire-count 0))
        (hb (make-vector wire-count 0))
        (size 0)
        (i 0))
    (while (< i count)
      (let ((xi (vector-ref xs i))
            (yi (vector-ref ys i))
            (zi (vector-ref zs i))
            (j (+ i 1)))
        (while (< j count)
          (let ((dx (- xi (vector-ref xs j)))
                (dy (- yi (vector-ref ys j)))
                (dz (- zi (vector-ref zs j)))
                (d 0))
            (setq d (+ (* dx dx) (* dy dy) (* dz dz)))
            (if (< size wire-count)
                (progn
                  (vector-set hd size d)
                  (vector-set ha size i)
                  (vector-set hb size j)
                  (incq size)
                  ;; heap order is only needed once the heap is full
                  (when (= size wire-count)
                    (let ((idx (- (i/ size 2) 1)))
                      (while (>= idx 0)
                        (sift-down hd ha hb size idx)
                        (decq idx)))))
                (when (< d (vector-ref hd 0))
                  (vector-set hd 0 d)
                  (vector-set ha 0 i)
                  (vector-set hb 0 j)
                  (sift-down hd ha hb size 0))))
          (incq j)))
      (incq i))
    (list ha hb size)))

;; --- circuits ---------------------------------------------------------------

(defun build-neighbours (ha hb size count)
  (let ((neighbours (make-vector count nil)))
    (dotimes (k size)
      (let ((a (vector-ref ha k))
            (b (vector-ref hb k)))
        (vector-set neighbours a (cons b (vector-ref neighbours a)))
        (vector-set neighbours b (cons a (vector-ref neighbours b)))))
    neighbours))

;; Sizes of the connected components, walked with an explicit stack (the
;; interpreter's recursion limit is far below 999 frames of DFS).
(defun circuit-sizes (neighbours count)
  (let ((seen (make-vector count #f))
        (sizes nil))
    (dotimes (start count)
      (unless (vector-ref seen start)
        (let ((stack (list start))
              (size 0))
          (vector-set seen start #t)
          (while (not (null? stack))
            (let ((node (car stack)))
              (setq stack (cdr stack))
              (incq size)
              (dolist (next (vector-ref neighbours node))
                (unless (vector-ref seen next)
                  (vector-set seen next #t)
                  (setq stack (cons next stack))))))
          (setq sizes (cons size sizes)))))
    sizes))

(defun solve (input-lines)
  (let ((junctions (read-junctions input-lines))
        (xs nil) (ys nil) (zs nil) (count nil) (pairs nil) (sizes nil))
    (setq xs (car junctions))
    (setq ys (cadr junctions))
    (setq zs (caddr junctions))
    (setq count (cadddr junctions))
    (setq pairs (shortest-pairs xs ys zs count))
    (setq sizes (sort (circuit-sizes (build-neighbours (car pairs) (cadr pairs)
                                                       (caddr pairs) count)
                                     count)
                      #'>))
    (when (< (length sizes) 3)
      (error "solve: fewer than three circuits."))
    (* (car sizes) (cadr sizes) (caddr sizes))))

(princ (solve (lines)))
(princ "\n")
