;;;; package.lisp -- package definitions for polycas

(defpackage #:polycas-vars
  (:use)
  (:documentation "Home package for user variables, interned case-sensitively."))

(defpackage #:polycas
  (:use #:cl)
  (:documentation "A small computer algebra system: canonical simplification,
calculus, exact polynomial arithmetic, factoring and Groebner bases.")
  (:export
   ;; conditions
   #:cas-error #:parse-error* #:not-polynomial
   ;; expressions
   #:var #:var-p #:sum-p #:product-p #:power-p #:function-p
   #:expr< #:expr= #:free-of-p #:variables
   ;; parsing / printing
   #:parse #:parse-equation #:read-expr #:to-infix
   ;; simplification
   #:simp #:simp-sum #:simp-product #:simp-power #:simp-function
   ;; calculus
   #:diff #:substitute* #:evaluate #:expand
   ;; polynomials
   #:poly #:poly-vars #:poly-order #:poly-terms #:make-poly
   #:expr->poly #:poly->expr #:poly-zero-p #:poly+ #:poly- #:poly*
   #:poly-scale #:poly-lt #:poly-lm #:poly-lc #:poly-monic
   #:poly-divide #:poly-normal-form #:poly= #:mono-compare
   ;; univariate
   #:expr->upoly #:upoly->expr #:upoly-divmod #:upoly-gcd #:upoly-derivative
   #:upoly-eval #:upoly-mul #:upoly-squarefree #:upoly-rational-roots
   #:factor #:solve #:poly-gcd-expr
   ;; groebner
   #:s-polynomial #:groebner #:reduced-groebner #:groebner-basis-p
   #:ideal-member-p))
