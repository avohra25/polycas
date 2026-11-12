;;;; polycas.asd

(defsystem "polycas"
  :description "A small computer algebra system in portable Common Lisp."
  :version "0.1.0"
  :author "Aryan Vohra"
  :license "MIT"
  :serial t
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "expr")
                             (:file "simplify")
                             (:file "parse")
                             (:file "print"))))
  :in-order-to ((test-op (test-op "polycas/tests"))))

(defsystem "polycas/tests"
  :depends-on ("polycas")
  :serial t
  :components ((:module "tests"
                :serial t
                :components ((:file "harness")
                             (:file "test-core"))))
  :perform (test-op (o c)
             (unless (uiop:symbol-call '#:polycas-tests '#:run-all)
               (error "polycas tests failed"))))
