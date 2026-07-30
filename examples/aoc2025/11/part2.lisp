;;; Advent of Code 2025 -- Day 11, Part 2
;;;
;;; Count the paths from "svr" to "out" that pass through both "fft" and "dac".
;;;
;;; Same forward pass over the topological order as part 1, but a path now also
;;; carries which of the two junctions it has already seen, so each junction
;;; keeps four counts instead of one.
;;;
;;; Usage: bamboo-lisp part2.lisp < input

(defvar source "svr")
(defvar sink "out")
(defvar must-visit-a "fft")
(defvar must-visit-b "dac")

;; tag -> list of tags it points at
(defun read-graph (input-lines)
  (let ((successors (make-dict)))
    (dolist (line input-lines)
      (let ((text (strip-string line)))
        (when (plus? (string-length text))
          (let ((tokens (filter (lambda (s) (plus? (string-length s)))
                                (split-string text #\space)))
                (tag nil))
            (setq tag (car tokens))
            ;; drop the ':' after the junction's own name
            (when (char= (string-ref tag (- (string-length tag) 1)) #\:)
              (setq tag (substring tag 0 (- (string-length tag) 1))))
            (dict-set successors tag (cdr tokens))))))
    successors))

;; Every tag mentioned anywhere, and how many wires arrive at it.
(defun in-degrees (successors)
  (let ((degrees (make-dict)))
    (dolist (tag (dict-keys successors))
      (when (null? (dict-get degrees tag))
        (dict-set degrees tag 0))
      (dolist (next (dict-get successors tag))
        (dict-set degrees next (+ 1 (let ((d (dict-get degrees next)))
                                      (if (null? d) 0 d))))))
    degrees))

;; Kahn's algorithm: tags in an order where every wire points forward.
(defun topological-order (successors)
  (let ((degrees (in-degrees successors))
        (ready nil)
        (order nil)
        (count 0))
    (dolist (tag (dict-keys degrees))
      (when (zero? (dict-get degrees tag))
        (setq ready (cons tag ready))))
    (while (not (null? ready))
      (let ((tag (car ready)))
        (setq ready (cdr ready))
        (setq order (cons tag order))
        (incq count)
        (dolist (next (dict-get successors tag))
          (let ((left (- (dict-get degrees next) 1)))
            (dict-set degrees next left)
            (when (zero? left)
              (setq ready (cons next ready)))))))
    (unless (= count (length (dict-keys degrees)))
      (error "topological-order: the graph has a cycle."))
    (nreverse order)))

;; state index: bit 1 = "fft" seen, bit 0 = "dac" seen
(defun state-after (tag state)
  (let ((next state))
    (when (string= tag must-visit-a)
      (setq next (logior next 2)))
    (when (string= tag must-visit-b)
      (setq next (logior next 1)))
    next))

(defun counts-for (paths tag)
  (let ((counts (dict-get paths tag)))
    (when (null? counts)
      (setq counts (make-vector 4 0))
      (dict-set paths tag counts))
    counts))

(defun solve (input-lines)
  (let ((successors (read-graph input-lines))
        (paths (make-dict)))
    (vector-set (counts-for paths source) (state-after source 0) 1)
    (dolist (tag (topological-order successors))
      (let ((here (dict-get paths tag)))
        (unless (null? here)
          (let ((next-tags (dict-get successors tag)))
            (when (and (null? next-tags) (not (string= tag sink)))
              (error "solve: dead end that is not the sink."))
            (dolist (next next-tags)
              (let ((there (counts-for paths next)))
                (dotimes (state 4)
                  (let ((count (vector-ref here state)))
                    (when (plus? count)
                      (let ((moved (state-after next state)))
                        (vector-set there moved
                                    (+ count (vector-ref there moved)))))))))))))
    ;; only paths that saw both junctions count
    (vector-ref (counts-for paths sink) 3)))

(princ (solve (lines)))
(princ "\n")
