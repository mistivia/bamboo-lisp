;;; Advent of Code 2025 -- Day 2, Part 2
;;;
;;; Now an id is invalid when it is *any* shorter block of digits repeated to
;;; fill it: "1212", "123123123", "7777". Sum every invalid id in the ranges.
;;;
;;; Same trick as part 1, one digit-length `len` at a time: an id that repeats a
;;; d-digit block (d divides len, d < len) is
;;;
;;;     block * repunit(d, len/d)   with block in [10^(d-1), 10^d - 1]
;;;
;;; so each (len, d) contributes an arithmetic series. But the sets overlap --
;;; 111111 repeats a block of 1, 2 and 3 digits -- so summing them all would
;;; double count. We therefore split each set by *minimal* period: the sum for
;;; period exactly d is the sum of the period-d set minus the sums already
;;; accounted for by every proper divisor of d.
;;;
;;; Usage: bamboo-lisp part2.lisp < input

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

;; Divisors of N smaller than N, increasing.
(defun proper-divisors (n)
  (let ((divisors nil)
        (d 1))
    (while (< d n)
      (when (zero? (mod n d))
        (setq divisors (cons d divisors)))
      (incq d))
    (nreverse divisors)))

;; 1 + 10^d + 10^2d + ... (REPS terms): the number that turns a d-digit block
;; into that block repeated REPS times.
(defun repunit (d reps)
  (let ((total 0)
        (place 1)
        (step (ipow10 d)))
    (dotimes (i reps)
      (setq total (+ total place))
      (setq place (* place step)))
    total))

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

;; Sum of the ids in [lo, hi] (all of them LEN digits long) that repeat a block
;; of exactly D digits, given the sums for the shorter periods as an alist
;; ((period . sum) ...).
(defun exact-period-sum (len d lo hi shorter)
  (let ((total (pattern-sum (repunit d (i/ len d))
                            lo hi
                            (ipow10 (- d 1))
                            (- (ipow10 d) 1))))
    (dolist (entry shorter)
      (when (zero? (mod d (car entry)))
        (setq total (- total (cdr entry)))))
    total))

;; Sum of the invalid ids in [a, b], one digit-length at a time.
(defun sum-invalid (a b)
  (let ((total 0)
        (len (num-digits a))
        (maxlen (num-digits b)))
    (while (<= len maxlen)
      (let ((lo (max a (ipow10 (- len 1))))
            (hi (min b (- (ipow10 len) 1)))
            (by-period nil))
        (when (<= lo hi)
          (dolist (d (proper-divisors len))
            (let ((sum (exact-period-sum len d lo hi by-period)))
              (setq by-period (cons (cons d sum) by-period))
              (setq total (+ total sum))))))
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
