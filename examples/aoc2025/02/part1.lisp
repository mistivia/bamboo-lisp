;;; Advent of Code 2025 -- Day 2, Part 1
;;;
;;; The input is one line of comma separated ranges. An id is invalid when its
;;; first half of digits equals its second half ("1212", "457457"). Sum every
;;; invalid id inside the ranges.
;;;
;;; The ranges cover a couple of million ids, which is too much for a
;;; tree-walking interpreter to test one by one. Instead we *build* the invalid
;;; ids: an id of 2d digits whose halves are equal is exactly
;;;
;;;     block * (10^d + 1)     with block in [10^(d-1), 10^d - 1]
;;;
;;; which grows with `block`, so the blocks landing inside a range form one
;;; contiguous interval and their contribution is an arithmetic series.
;;;
;;; Usage: bamboo-lisp part1.lisp < input

(defun ipow10 (k)
  (let ((res 1))
    (dotimes (i k)
      (setq res (* res 10)))
    res))

(defun num-digits (n)
  (let ((count 1))
    (while (>= n 10)
      (setq n (i/ n 10))
      (incq count))
    count))

;; Sum of the multiples of R whose value lies in [lo, hi] and whose quotient
;; (the repeated block) lies in [block-lo, block-hi].
(defun pattern-sum (r lo hi block-lo block-hi)
  (let ((first-block (max block-lo (i/ (+ lo r -1) r)))   ; ceil(lo / r)
        (last-block (min block-hi (i/ hi r))))
    (if (> first-block last-block)
        0
        (* r (i/ (* (+ first-block last-block)
                    (+ (- last-block first-block) 1))
                 2)))))

;; Sum of the invalid ids in [a, b], one digit-length at a time.
(defun sum-invalid (a b)
  (let ((total 0)
        (len (num-digits a))
        (maxlen (num-digits b)))
    (while (<= len maxlen)
      (when (zero? (mod len 2))
        (let ((lo (max a (ipow10 (- len 1))))
              (hi (min b (- (ipow10 len) 1)))
              (half (i/ len 2)))
          (when (<= lo hi)
            (setq total (+ total (pattern-sum (+ (ipow10 half) 1)
                                              lo hi
                                              (ipow10 (- half 1))
                                              (- (ipow10 half) 1)))))))
      (incq len))
    total))

(defun parse-ranges (text)
  (let ((ranges nil))
    (dolist (field (split-string (strip-string text) #\,))
      (let ((ends (split-string field #\-)))
        (when (= (length ends) 2)
          (setq ranges (cons (cons (string->number (car ends))
                                   (string->number (cadr ends)))
                             ranges)))))
    (nreverse ranges)))

(defun solve (input-lines)
  (let ((total 0))
    (dolist (line input-lines)
      (dolist (range (parse-ranges line))
        (setq total (+ total (sum-invalid (car range) (cdr range))))))
    total))

(princ (solve (lines)))
(princ "\n")
