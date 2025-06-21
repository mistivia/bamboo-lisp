(assert (and #t))
(assert (and #t #t))
(assert (and #t #t #t))
(assert (not (and #t #f #t)))
(assert (not (and #f #t #t)))
(assert (not (and #f #t #t)))
(assert (not (or #f)))
(assert (not (or #f #f)))
(assert (not (or #f #f #f)))
(assert (or #t #f #f))
(assert (or #f #t #f))
(assert (or #t #t #f))

(let ((t #f))
  (or #t
      (setq t #t))
  (assert (not t)))

(let ((t #f))
  (and #t
      (setq t #t))
  (assert t))

(let ((t #t))
  (and (setq t #f)
      (setq t #t))
  (assert (not t)))
