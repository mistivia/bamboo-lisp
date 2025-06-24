(assert (eq? 'a (intern "a")))
(assert (eq? (intern "ab") (intern (concat "a" "b"))))
(assert (equal? "abc" (symbol->string 'abc)))
(assert (not (eq? (gensym) (gensym))))
