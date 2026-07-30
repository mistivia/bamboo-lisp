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


(assert (= 3 (string-length "abc")))
(assert (= 0 (string-length "")))
(assert (char= #\b (string-ref "abc" 1)))
(assert-error (string-ref "abc" 3))
(assert-error (string-ref "abc" -1))

(assert (string= "bc" (substring "abcd" 1 3)))
(assert (string= "bcd" (substring "abcd" 1)))
(assert (string= "" (substring "abcd" 2 2)))
(assert-error (substring "abcd" 2 5))
(assert-error (substring "abcd" 3 2))

(assert (equal? (list #\a #\b) (string->list "ab")))
(assert (null? (string->list "")))
(assert (string= "ab" (list->string (list #\a #\b))))
(assert (string= "" (list->string nil)))
(assert-error (list->string (list #\a 1)))

(assert (= 42 (string->number "42")))
(assert (= -42 (string->number "  -42  ")))
(assert (= 3.5 (string->number "3.5")))
(assert (integer? (string->number "7")))
(assert (float? (string->number "7.0")))
(assert (null? (string->number "12x")))
(assert (null? (string->number "")))
(assert (null? (string->number "  ")))

(assert (string= "42" (number->string 42)))
(assert (string= "-42" (number->string -42)))
