(assert (= 16 (logand 31 16)))
(assert (= 24 (logand 31 25 24)))

(assert (= 25 (logior 24 16 1)))
(assert (= 25 (logior 8 16 1)))

(assert (= 678 (logxor 123 456 789)))

(assert (= -124 (lognot 123)))

(assert (= 246 (lsh 123 1)))

(assert (= 30 (ash 123 2)))
(assert (= -31 (ash -123 2)))

(assert (= 30 (ash 123 2)))
(assert (= -31 (ash -123 2)))

