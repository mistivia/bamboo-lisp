;;; Advent of Code 2025 -- Day 11, Part 1
;;;
;;; Every line "abc: def ghi" lists the wires leaving junction abc. Count the
;;; distinct paths from "you" to "out".
;;;
;;; The reference implementation recurses with a memo table. The graph is a DAG,
;;; so we can instead sort it topologically (Kahn) and push the path counts
;;; forward, which needs no recursion at all -- the interpreter's recursion
;;; limit is well below this graph's depth.
;;;
;;; Usage: bamboo-lisp part1.lisp < input

(defvar source "you")
(defvar sink "out")

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

(defun solve (input-lines)
  (let ((successors (read-graph input-lines))
        (paths (make-dict)))
    (dict-set paths source 1)
    (dolist (tag (topological-order successors))
      (let ((here (dict-get paths tag)))
        (unless (null? here)
          (let ((next-tags (dict-get successors tag)))
            (when (and (null? next-tags) (not (string= tag sink)))
              (error "solve: dead end that is not the sink."))
            (dolist (next next-tags)
              (dict-set paths next (+ here (let ((n (dict-get paths next)))
                                            (if (null? n) 0 n)))))))))
    (let ((total (dict-get paths sink)))
      (if (null? total) 0 total))))

(princ (solve (lines)))
(princ "\n")
