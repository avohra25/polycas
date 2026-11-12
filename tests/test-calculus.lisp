;;;; test-calculus.lisp -- differentiation, substitution, evaluation, expansion

(in-package #:polycas-tests)

(defun d (string var &optional (n 1)) (to-infix (diff (read-expr string) (v var) n)))
(defun ex (string) (to-infix (expand (read-expr string))))

(deftest derivative-rules
  (check-equal "0" (d "5" "x"))
  (check-equal "1" (d "x" "x"))
  (check-equal "0" (d "y" "x"))
  (check-equal "3*x^2" (d "x^3" "x"))
  (check-equal "-1/x^2" (d "1/x" "x"))
  (check-equal "1/(2*x^(1/2))" (d "sqrt(x)" "x"))
  (check-equal "6*x + 2*y" (d "3x^2 + 2x y + y^2" "x"))
  (check-equal "cos(x)" (d "sin(x)" "x"))
  (check-equal "-sin(x)" (d "cos(x)" "x"))
  (check-equal "tan(x)^2 + 1" (d "tan(x)" "x"))
  (check-equal "exp(x)" (d "exp(x)" "x"))
  (check-equal "1/x" (d "log(x)" "x"))
  (check-equal "2*x*cos(x^2)" (d "sin(x^2)" "x"))
  (check-equal "x*cos(x) + sin(x)" (d "x*sin(x)" "x"))
  (check-equal "x^x*(log(x) + 1)" (d "x^x" "x"))
  (check-equal "2^x*log(2)" (d "2^x" "x"))
  (check-equal "6" (d "x^3" "x" 3))
  (check-equal "0" (d "x^3" "x" 4))
  (check-equal "-sin(x)" (d "sin(x)" "x" 2)))

(deftest substitution
  (check-equal "9" (to-infix (substitute* (read-expr "x^2") (list (cons (v "x") 3)))))
  (check-equal "y^2 + 2*y + 1"
               (to-infix (expand (substitute* (read-expr "x^2") (list (cons (v "x") (read-expr "y + 1")))))))
  (check-equal "x + y" (to-infix (substitute* (read-expr "x + y")
                                              (list (cons (v "x") (v "y")) (cons (v "y") (v "x")))))
               "simultaneous swap")
  (check-equal 0 (substitute* (read-expr "sin(x)") (list (cons (v "x") 0)))))

(deftest evaluation
  (check-equal 25/4 (evaluate (substitute* (read-expr "x^2 + 1/4") (list (cons (v "x") (read-expr "sqrt(6)"))))))
  (check-equal 5/2 (evaluate (read-expr "x^2 + 1/4") (list (cons (v "x") 3/2))))
  (check-equal 7/2 (evaluate (read-expr "x/2 + y") (list (cons (v "x") 3) (cons (v "y") 2))))
  (check-equal 3 (evaluate (read-expr "sqrt(x)") (list (cons (v "x") 9))))
  (check (< (abs (- (evaluate (read-expr "sin(x)^2 + cos(x)^2") (list (cons (v "x") 7/3))) 1d0)) 1d-12))
  (check (< (abs (- (evaluate (read-expr "exp(1)")) (exp 1d0))) 1d-12))
  (check-error cas-error (evaluate (read-expr "x + 1")))
  (check-error cas-error (evaluate (read-expr "1/x") (list (cons (v "x") 0)))))

(deftest expansion
  (check-equal "x^2 + 2*x*y + y^2" (ex "(x + y)^2"))
  (check-equal "x^2 - 1" (ex "(x - 1)(x + 1)"))
  (check-equal "x^3 + 3*x^2 + 3*x + 1" (ex "(x + 1)^3"))
  (check-equal "a*c + a*d + b*c + b*d" (ex "(a + b)(c + d)"))
  (check-equal "2*x + 2" (ex "2(x + 1)"))
  (check-equal "x + 1" (ex "sqrt(x + 1)^2"))
  (check-equal "x^2*sin(y + 1) + x*sin(y + 1)" (ex "x*(x + 1)*sin(y + 1)"))
  (check-equal 0 (expand (read-expr "(x + y)^2 - x^2 - 2x y - y^2"))))

(deftest binomial-theorem
  ;; coefficients of (x + 1)^n are binomial coefficients
  (loop for n from 1 to 12
        do (let ((e (expand (list :^ (list :+ (v "x") 1) n))))
             (check (= (evaluate e (list (cons (v "x") 1))) (expt 2 n)))
             (check (= (length (cdr e)) (1+ n))))))

(deftest expansion-preserves-value
  (seed 11)
  (dotimes (i 400)
    (let* ((e (random-expr 4)) (p (random-point)) (before (exact-eval e p)))
      (unless (eq before :undefined)
        (let ((after (handler-case (expand e) (cas-error () :error))))
          (unless (eq after :error)
            (check (equal before (exact-eval after p))
                   (format nil "expand changed value of ~S" e))))))))

(defun numeric-derivative (e var point)
  "Central difference in double precision."
  (let* ((h 1d-6) (x (cdr (assoc var point))))
    (flet ((at (val) (evaluate e (acons var val (remove var point :key #'car)))))
      (/ (- (at (+ x h)) (at (- x h))) (* 2 h)))))

(deftest derivatives-match-finite-differences
  (seed 12)
  (let ((compared 0))
    (dotimes (i 400)
      (let* ((e (handler-case (simp (random-expr 4 :functions t)) (cas-error () nil)))
             (p (loop for n in *names* collect (cons (v n) (+ 0.5d0 (/ (rnd 1000) 400d0))))))
        (when e
          (handler-case
              (let ((num (numeric-derivative e (v "x") p))
                    (sym (evaluate (diff e (v "x")) p)))
                (when (and (realp num) (realp sym) (< (abs sym) 1d6))
                  (incf compared)
                  (check (< (abs (- num sym)) (* 1d-4 (max 1d0 (abs sym))))
                         (format nil "d/dx ~A: symbolic ~A numeric ~A" (to-infix e) sym num))))
            (arithmetic-error () nil)
            (cas-error () nil)))))
    (check (> compared 250) "enough comparisons")))

(deftest polynomial-derivatives-are-exact
  (seed 13)
  ;; d/dx of an expanded polynomial, checked term by term against n*c*x^(n-1)
  (dotimes (i 100)
    (let* ((coeffs (loop repeat (rnd-between 1 8) collect (rnd-between -9 9)))
           (poly (simp (cons :+ (loop for c in coeffs for k from 0
                                      collect (list :* c (list :^ (v "x") k))))))
           (expected (simp (cons :+ (loop for c in coeffs for k from 0
                                          when (> k 0)
                                            collect (list :* (* k c) (list :^ (v "x") (1- k))))))))
      (check-equal expected (diff poly (v "x"))))))
