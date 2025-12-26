(defvar fp (open-file "./tests/io-test-input.txt" "r"))
(assert (char= #\a (read-char fp)))
(assert (char= #\b (read-char fp)))
(assert (char= #\c (read-char fp)))
(stream-close fp)

(defvar fp (open-file "./tests/io-test-input.txt" "r"))
(assert (string= "abc" (read-line fp)))
(stream-close fp)

(defvar fp (open-file "./tests/io-test-input.txt" "r"))
(assert (equal? (list "abc" "1234" "123") (lines fp)))
(stream-close fp)