(loadext "io.so")

(defvar fp (open-file "./test_input.txt" "r"))
(read-char fp)
(read-char fp)
(read-char fp)
(stream-close fp)

(defvar fp (open-file "./test_input.txt" "r"))
(read-line fp)
(stream-close fp)

(defvar fp (open-file "./test_input.txt" "r"))
(lines fp) 
(stream-close fp)