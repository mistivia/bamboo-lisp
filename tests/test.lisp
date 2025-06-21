(defmacro test-module (name)
  `(progn
    (princ (format "[TEST] %s\n" ,name))
    (load (format "%s.lisp" ,name))
    (princ (format "[PASS] %s\n" ,name))))

(test-module "arithmetic")
(test-module "error")
(test-module "logic")
(test-module "tailcall")
(test-module "control-flow")
(test-module "lambda")
(test-module "comment")
(test-module "macro")
(test-module "let-binding")

(exit)
