;;; Advent of Code 2025 -- Day 12, Part 1
;;;
;;; The input first defines six 3x3 pieces (a '#' is one filled cell), then a
;;; list of orders "WIDTHxHEIGHT: n0 n1 n2 n3 n4 n5". Count the orders whose
;;; sheet is strictly larger than the area their pieces take up -- those are the
;;; ones that might be cuttable at all.
;;;
;;; Usage: bamboo-lisp part1.lisp < input

(defvar piece-count 6)

(defun contains-char? (text c)
  (dotimes (i (string-length text))
    (when (char= (string-ref text i) c)
      (return #t)))
  #f)

;; Order lines are the ones shaped like "12x34: ...", piece headers like "0:".
(defun order-line? (text)
  (and (contains-char? text #\:)
       (contains-char? text #\x)))

;; A piece is a header line, then three rows of '#' and '.'. Returns the list of
;; filled-cell counts, in input order.
(defun read-pieces (input-lines)
  (let ((sizes nil)
        (filled 0)
        (rows 0))
    (dolist (line input-lines)
      (let ((text (strip-string line)))
        (cond ((zero? (string-length text)) nil)         ; blank separator
              ((order-line? text) (break))               ; pieces are done
              ((contains-char? text #\:)
               (progn (setq filled 0)                    ; a new piece starts
                      (setq rows 0)))
              (#t
               (progn
                 (dotimes (i (string-length text))
                   (when (char= (string-ref text i) #\#)
                     (incq filled)))
                 (incq rows)
                 (when (= rows 3)
                   (setq sizes (cons filled sizes))))))))
    (unless (= (length sizes) piece-count)
      (error "read-pieces: expected six pieces."))
    (nreverse sizes)))

;; element-wise product of two equally long lists
(defun products (as bs)
  (let ((out nil))
    (while (and (not (null? as)) (not (null? bs)))
      (setq out (cons (* (car as) (car bs)) out))
      (setq as (cdr as))
      (setq bs (cdr bs)))
    (nreverse out)))

;; "41x38: 26 26 29 23 21 30" -> (sheet-area piece-counts...)
(defun parse-order (text)
  (let ((halves (split-string text #\:))
        (size nil)
        (counts nil))
    (unless (= (length halves) 2)
      (error "parse-order: bad order."))
    (setq size (split-string (car halves) #\x))
    (unless (= (length size) 2)
      (error "parse-order: bad sheet size."))
    (setq counts (map #'string->number
                      (filter (lambda (s) (plus? (string-length s)))
                              (split-string (cadr halves) #\space))))
    (unless (= (length counts) piece-count)
      (error "parse-order: wrong number of piece counts."))
    (cons (* (string->number (car size)) (string->number (cadr size)))
          counts)))

(defun solve (input-lines)
  (let ((sizes (read-pieces input-lines))
        (fits 0))
    (dolist (line input-lines)
      (let ((text (strip-string line)))
        (when (order-line? text)
          (let ((order (parse-order text)))
            (when (> (car order) (foldl #'+ 0 (products sizes (cdr order))))
              (incq fits))))))
    fits))

(princ (solve (lines)))
(princ "\n")
