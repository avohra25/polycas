;;;; run-tests.lisp -- sbcl --script run-tests.lisp [-v]
;;;; Loads the system from this directory and runs the whole suite.
;;;; Exits 0 when every test passes, 1 otherwise.

(require :asdf)
(let ((here (directory-namestring (or *load-truename* *default-pathname-defaults*))))
  (asdf:load-asd (merge-pathnames "polycas.asd" here)))
(handler-bind ((warning #'muffle-warning))
  (asdf:load-system "polycas/tests"))
(let ((ok (uiop:symbol-call '#:polycas-tests '#:run-all
                            :verbose (member "-v" (uiop:command-line-arguments) :test #'string=))))
  (uiop:quit (if ok 0 1)))
