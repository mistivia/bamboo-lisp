;;; Advent of Code 2025 -- Day 9, Part 2
;;;
;;; The tiles, in input order, are the corners of a closed rectilinear loop.
;;; Same question as part 1, but the rectangle now has to stay inside the loop:
;;; each of its four sides must lie on the loop or in its interior.
;;;
;;; For one row, "inside" is found by the even-odd rule: walk the vertical edges
;;; left to right, and the stretch between crossing 1 and 2, 3 and 4, ... is
;;; interior. Together with the horizontal edges on that row (which are part of
;;; the loop) and after merging, that gives the row's usable spans; columns work
;;; the same way. A rectangle is then valid when each side fits inside a single
;;; span of its row or column.
;;;
;;; The reference implementation does this for all ~98000 rows and columns of
;;; the bounding box. Only the rows and columns that hold a tile can ever be
;;; asked about, and there are just 248 distinct values of each, so those are
;;; the only ones computed here.
;;;
;;; Usage: bamboo-lisp part2.lisp < input

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

;; Dicts are keyed by strings, so coordinates are used through this.
(defun push-range (table key lo hi)
  (let ((k (number->string key)))
    (dict-set table k (cons (cons lo hi) (dict-get table k)))))

(defun ranges-at (table key)
  (dict-get table (number->string key)))

(defun sorted-keys (table)
  (sort (map #'string->number (dict-keys table)) #'<))

;; The loop's edges, split into vertical ones (x -> y ranges) and horizontal
;; ones (y -> x ranges).
(defun collect-edges (xs ys count vlines hlines)
  (dotimes (i count)
    (let ((j (mod (+ i 1) count))
          (ax nil) (ay nil) (bx nil) (by nil))
      (setq ax (vector-ref xs i))
      (setq ay (vector-ref ys i))
      (setq bx (vector-ref xs j))
      (setq by (vector-ref ys j))
      (cond ((= ax bx) (push-range vlines ax (min ay by) (max ay by)))
            ((= ay by) (push-range hlines ay (min ax bx) (max ax bx)))
            (#t (error "collect-edges: edge is not axis parallel."))))))

;; Merge overlapping ranges. Ranges that merely touch ("1-5" and "6-9") are
;; kept apart, which is what makes a rectangle side spanning the gap invalid.
(defun merge-ranges (ranges)
  (if (null? ranges)
      nil
      (let ((sorted (sort ranges (lambda (a b) (< (car a) (car b)))))
            (merged nil)
            (current nil))
        (setq current (car sorted))
        (dolist (range (cdr sorted))
          (if (> (car range) (cdr current))
              (progn (setq merged (cons current merged))
                     (setq current range))
              (when (> (cdr range) (cdr current))
                (setq current (cons (car current) (cdr range))))))
        (nreverse (cons current merged)))))

;; Does one of RANGES cross the line at POS? Ranges are half open, so an edge
;; ending exactly at POS does not count -- that is what makes the even-odd rule
;; come out right at the loop's corners.
(defun crosses? (pos ranges)
  (dolist (range ranges)
    (when (and (<= (car range) pos) (< pos (cdr range)))
      (return #t)))
  #f)

(defun sub-range? (lo hi ranges)
  (dolist (range ranges)
    (when (and (<= (car range) lo) (>= (cdr range) hi))
      (return #t)))
  #f)

;; The interior spans along the line at POS: the loop edges lying on it, plus
;; everything between an odd and an even crossing of the perpendicular edges.
(defun inside-spans (pos own-edges crossing-table crossing-keys)
  (let ((spans own-edges)
        (crossings 0)
        (entered 0))
    (dolist (key crossing-keys)
      (when (crosses? pos (ranges-at crossing-table key))
        (when (zero? (mod crossings 2))
          (setq entered key))
        (incq crossings)
        (when (zero? (mod crossings 2))
          (setq spans (cons (cons entered key) spans)))))
    (merge-ranges spans)))

;; Interior spans for every line that holds a tile, keyed by coordinate.
(defun spans-by-line (coords own-lines crossing-table crossing-keys)
  (let ((spans (make-dict)))
    (dolist (pos coords)
      (let ((k (number->string pos)))
        (when (null? (dict-get spans k))
          (dict-set spans k (inside-spans pos (ranges-at own-lines pos)
                                          crossing-table crossing-keys)))))
    spans))

(defun solve (input-lines)
  (let ((tiles (read-tiles input-lines))
        (xs nil) (ys nil) (count nil)
        (vlines (make-dict)) (hlines (make-dict))
        (row-spans nil) (column-spans nil)
        (rows nil) (columns nil)
        (best 0) (i 0))
    (setq xs (car tiles))
    (setq ys (cadr tiles))
    (setq count (caddr tiles))
    (when (< count 2)
      (error "solve: need at least two tiles."))
    (collect-edges xs ys count vlines hlines)
    ;; rows are cut by vertical edges and vice versa
    (setq row-spans (spans-by-line (vector->list ys) hlines vlines (sorted-keys vlines)))
    (setq column-spans (spans-by-line (vector->list xs) vlines hlines (sorted-keys hlines)))
    ;; the spans each tile's row and column can offer
    (setq rows (make-vector count nil))
    (setq columns (make-vector count nil))
    (dotimes (tile count)
      (vector-set rows tile (dict-get row-spans (number->string (vector-ref ys tile))))
      (vector-set columns tile (dict-get column-spans (number->string (vector-ref xs tile)))))
    (while (< i count)
      (let ((xi (vector-ref xs i))
            (yi (vector-ref ys i))
            (j (+ i 1)))
        (while (< j count)
          (let ((xj (vector-ref xs j))
                (yj (vector-ref ys j))
                (area 0))
            (setq area (* (+ (abs (- xi xj)) 1) (+ (abs (- yi yj)) 1)))
            ;; only worth validating a rectangle that would actually win
            (when (and (> area best)
                       (sub-range? (min xi xj) (max xi xj) (vector-ref rows i))
                       (sub-range? (min xi xj) (max xi xj) (vector-ref rows j))
                       (sub-range? (min yi yj) (max yi yj) (vector-ref columns i))
                       (sub-range? (min yi yj) (max yi yj) (vector-ref columns j)))
              (setq best area)))
          (incq j)))
      (incq i))
    best))

(princ (solve (lines)))
(princ "\n")
