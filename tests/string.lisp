(assert (equal? "abc" (string #\a #\b #\c)))
(assert (equal? "ABC" (string 65 66 67)))

(assert (string= "abc" (string #\a #\b #\c)))
(assert (string= "ABC" (string 65 66 67)))

(defvar s1 "a1s")
(defvar s2 "a2s")

(assert (string= s1 s1))
(assert (string>= s1 s1))
(assert (string<= s1 s1))
(assert (string> s2 s1))
(assert (string>= s2 s1))
(assert (string< s1 s2))
(assert (string<= s1 s2))
(assert (string/= s1 s2))

(assert (not (string/= s1 s1)))
(assert (not (string< s1 s1)))
(assert (not (string> s1 s1)))
(assert (not (string<= s2 s1)))
(assert (not (string< s2 s1)))
(assert (not (string>= s1 s2)))
(assert (not (string> s1 s2)))

(assert (string= "abc" (strip-string "\n\tabc \t\n")))
(assert (equal? (list "a" "b" "c") (split-string "a,b,c" #\,)))

