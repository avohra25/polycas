;;;; test-core.lisp -- parser, simplifier and printer

(in-package #:polycas-tests)

(defun s (string) (to-infix (read-expr string)))
(defun v (name) (var name))

(deftest tokenizer-and-numbers
  (check-equal 5/4 (read-expr "1.25"))
  (check-equal 1/2 (read-expr ".5"))
  (check-equal 7 (read-expr "3 + 4"))
  (check-equal -1/6 (read-expr "1/3 - 1/2"))
  (check-equal 1024 (read-expr "2^10"))
  (check-equal 1/8 (read-expr "2^-3"))
  (check-equal 512 (read-expr "2^3^2") "power is right associative")
  (check-equal -9 (read-expr "-3^2") "unary minus binds looser than ^"))

(deftest variables-are-case-sensitive
  (check (not (eq (v "x") (v "X"))))
  (check-equal (list :+ (v "X") (v "x")) (read-expr "x + X")))

(deftest parse-errors
  (check-error parse-error* (read-expr ""))
  (check-error parse-error* (read-expr "(x + 1"))
  (check-error parse-error* (read-expr "x + "))
  (check-error parse-error* (read-expr "x $ y"))
  (check-error parse-error* (read-expr "foo(x)"))
  (check-error parse-error* (read-expr "x )"))
  (check-error cas-error (read-expr "1/0")))

(deftest implicit-multiplication
  (check-equal (read-expr "2*x*y") (read-expr "2x y"))
  (check-equal (read-expr "3*(x+1)") (read-expr "3(x+1)"))
  (check-equal (read-expr "2*x^2") (read-expr "2x^2")))

(deftest like-terms-and-factors
  (check-equal "3*x" (s "x + 2x"))
  (check-equal 0 (read-expr "x - x"))
  (check-equal 0 (read-expr "a*b - b*a"))
  (check-equal "x^5" (s "x^2 * x^3"))
  (check-equal 1 (read-expr "x/x"))
  (check-equal "x^2*y" (s "x*y*x"))
  (check-equal "2*x*y" (s "x*y + y*x"))
  (check-equal "x^(a + b)" (s "x^a * x^b"))
  (check-equal "x^6" (s "(x^2)^3"))
  (check-equal "8*x^3*y^3" (s "(2*x*y)^3"))
  (check-equal "x" (s "sqrt(x)^2"))
  (check-equal "x^(1/2)" (s "x^(1/4) * x^(1/4)"))
  (check-equal 0 (read-expr "0 * (x + y)")))

(deftest canonical-order-is-independent-of-input-order
  (check-equal (read-expr "c + b*a + 3 + a^2") (read-expr "a^2 + 3 + a*b + c"))
  (check-equal (read-expr "z*y*x*2") (read-expr "2*x*y*z"))
  (check-equal (read-expr "(y + x)*(b + a)") (read-expr "(a + b)*(x + y)")))

(deftest numeric-powers
  (check-equal 2 (read-expr "sqrt(4)"))
  (check-equal 4 (read-expr "8^(2/3)"))
  (check-equal 2/3 (read-expr "(4/9)^(1/2)"))
  (check-equal "2^(1/2)" (s "sqrt(2)"))
  (check-equal 2 (read-expr "sqrt(2)*sqrt(2)"))
  (check-equal "2*2^(1/2)" (s "2^(3/2)"))
  (check-equal (read-expr "2*sqrt(2)") (read-expr "2^(3/2)"))
  (check-equal "2^(1/2)/2" (s "2^(-1/2)"))
  (check-equal 12345678901234567890 (read-expr "sqrt(152415787532388367501905199875019052100)"))
  (check-equal 1 (read-expr "x^0"))
  (check-equal 0 (read-expr "0^3")))

(deftest function-identities
  (check-equal 0 (read-expr "sin(0)"))
  (check-equal 1 (read-expr "cos(0)"))
  (check-equal 1 (read-expr "exp(0)"))
  (check-equal 0 (read-expr "log(1)"))
  (check-equal "x + 1" (s "exp(log(x + 1))"))
  (check-equal "x^2" (s "ln(exp(x^2))"))
  (check-equal "2*sin(x)" (s "sin(x) + sin(x)")))

(deftest printer
  (check-equal "x^2 - 2*x + 1" (s "1 - 2x + x^2"))
  (check-equal "-x" (s "-x"))
  (check-equal "1/x" (s "1/x"))
  (check-equal "-3/(2*x^2)" (s "-3/(2x^2)"))
  (check-equal "x/(x + 1)" (s "x/(x+1)"))
  (check-equal "3*x/4" (s "0.75 x"))
  (check-equal "-3/4" (s "-0.75"))
  (check-equal "x - 2*(x + 1)" (s "x - 2(x+1)"))
  (check-equal "(-1)^(1/2)" (s "(-1)^(1/2)"))
  (check-equal "x^(-y)" (s "x^(-y)"))
  (check-equal "sin(x)^2 + cos(x)^2" (s "cos(x)^2 + sin(x)^2")))

;;; ---------------------------------------------------- random properties

(defparameter *names* '("x" "y" "z"))

(defun random-expr (depth &key (functions nil))
  "A random unsimplified expression with exact semantics at nonzero points."
  (if (or (zerop depth) (< (rnd 10) 2))
      (if (< (rnd 2) 1)
          (/ (rnd-between -6 6) (rnd-between 1 3))
          (v (nth (rnd 3) *names*)))
      (case (rnd (if functions 6 5))
        ((0 1) (list :+ (random-expr (1- depth) :functions functions)
                     (random-expr (1- depth) :functions functions)))
        (2 (list :* (random-expr (1- depth) :functions functions)
                 (random-expr (1- depth) :functions functions)))
        (3 (list :* -1 (random-expr (1- depth) :functions functions)))
        (4 (list :^ (random-expr (1- depth) :functions functions) (rnd-between -2 3)))
        (t (list (nth (rnd 3) '(:sin :cos :exp)) (random-expr (1- depth) :functions functions))))))

(defun exact-eval (e env)
  "Evaluate an expression built from + * integer ^ at a rational point.
Returns :undefined on division by zero."
  (catch 'undefined
    (labels ((ev (x)
               (cond ((numberp x) x)
                     ((symbolp x) (cdr (assoc x env)))
                     ((eq (car x) :+) (reduce #'+ (mapcar #'ev (cdr x))))
                     ((eq (car x) :*) (reduce #'* (mapcar #'ev (cdr x))))
                     ((eq (car x) :^)
                      (let ((b (ev (second x))) (k (third x)))
                        (if (and (zerop b) (minusp k)) (throw 'undefined :undefined) (expt b k))))
                     (t (error "not exact: ~S" x)))))
      (ev e))))

(defun random-point ()
  (loop for n in *names*
        collect (cons (v n) (let ((num (rnd-between -5 5)))
                              (/ (if (zerop num) 7 num) (rnd-between 1 4))))))

(deftest simplification-preserves-value
  (seed 1)
  (let ((compared 0))
    (dotimes (i 600)
      (let* ((e (random-expr 5)) (p (random-point))
             (before (exact-eval e p)))
        (unless (eq before :undefined)
          (let ((after (handler-case (simp e) (cas-error () :error))))
            (unless (eq after :error)
              (incf compared)
              (check (equal before (exact-eval after p))
                     (format nil "value changed for ~S" e)))))))
    (check (> compared 400) "enough comparisons")))

(deftest simplification-is-idempotent
  (seed 2)
  (dotimes (i 600)
    (let ((e (handler-case (simp (random-expr 5 :functions t)) (cas-error () nil))))
      (when e
        (check (equal e (simp e)) (format nil "not idempotent on ~S" e))))))

(deftest printer-round-trips
  (seed 3)
  (dotimes (i 800)
    (let ((e (handler-case (simp (random-expr 5 :functions t)) (cas-error () nil))))
      (when e
        (let ((text (to-infix e)))
          (check (equal e (read-expr text))
                 (format nil "~S printed as ~S re-reads as ~S" e text (read-expr text))))))))

(deftest operand-order-does-not-matter
  (seed 4)
  (dotimes (i 300)
    (let* ((parts (loop repeat (rnd-between 2 5) collect (random-expr 3)))
           (shuffled (shuffle parts)))
      (handler-case
          (progn
            (check (equal (simp (cons :+ parts)) (simp (cons :+ shuffled))))
            (check (equal (simp (cons :* parts)) (simp (cons :* shuffled)))))
        (cas-error () nil)))))
