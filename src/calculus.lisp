;;;; calculus.lisp -- differentiation, substitution, evaluation and expansion

(in-package #:polycas)

(defun s+ (&rest args) (simp-sum args))
(defun s* (&rest args) (simp-product args))
(defun s^ (b e) (simp-power b e))
(defun s- (a b) (simp-sum (list a (simp-product (list -1 b)))))
(defun s/ (a b) (simp-product (list a (simp-power b -1))))

;;; -------------------------------------------------------- differentiation

(defun diff (e v &optional (n 1))
  "The N-th derivative of the simplified expression E with respect to variable V."
  (loop repeat n do (setf e (diff1 e v)))
  e)

(defun diff1 (e v)
  (cond ((numberp e) 0)
        ((symbolp e) (if (eq e v) 1 0))
        ((free-of-p e v) 0)
        ((sum-p e) (simp-sum (mapcar (lambda (x) (diff1 x v)) (cdr e))))
        ((product-p e)
         ;; product rule: sum over i of f_i' * prod_{j /= i} f_j
         (let ((factors (cdr e)))
           (simp-sum
            (loop for i from 0 below (length factors)
                  collect (simp-product
                           (loop for f in factors
                                 for j from 0
                                 collect (if (= i j) (diff1 f v) f)))))))
        ((power-p e)
         (let ((b (second e)) (x (third e)))
           (if (free-of-p x v)
               ;; d/dv b^x = x b^(x-1) b'
               (s* x (s^ b (s+ x -1)) (diff1 b v))
               ;; d/dv b^x = b^x (x' log b + x b'/b)
               (s* e (s+ (s* (diff1 x v) (simp-function :log b))
                         (s* x (diff1 b v) (s^ b -1)))))))
        ((function-p e)
         (let* ((a (second e)) (da (diff1 a v)))
           (s* da
               (ecase (car e)
                 (:sin (simp-function :cos a))
                 (:cos (s* -1 (simp-function :sin a)))
                 (:tan (s+ 1 (s^ (simp-function :tan a) 2)))
                 (:exp e)
                 (:log (s^ a -1))))))
        (t (error "cannot differentiate ~S" e))))

;;; ------------------------------------------------------------ substitution

(defun substitute* (e bindings)
  "Replace variables according to BINDINGS, an alist (var . expr), and simplify.
All replacements happen simultaneously."
  (simp (sublis bindings e :test #'eq)))

;;; -------------------------------------------------------------- evaluation

(defun float-or-rational (x)
  (if (and (complexp x) (zerop (imagpart x))) (realpart x) x))

(defun evaluate (e &optional bindings)
  "Numerically evaluate E. BINDINGS is an alist (var . number). The result is
an exact rational when E only needs field operations and integer powers,
otherwise a double-float (or complex double when a real result is impossible)."
  (labels ((ev (x)
             (cond ((numberp x) x)
                   ((symbolp x)
                    (let ((b (assoc x bindings)))
                      (unless b (cas-error 'cas-error "no value for variable ~A" (symbol-name x)))
                      (cdr b)))
                   ((sum-p x) (reduce #'+ (mapcar #'ev (cdr x))))
                   ((product-p x) (reduce #'* (mapcar #'ev (cdr x))))
                   ((power-p x)
                    (let ((b (ev (second x))) (k (ev (third x))))
                      (cond ((and (zerop b) (realp k) (minusp k))
                             (cas-error 'cas-error "division by zero"))
                            ((integerp k) (expt b k))
                            ((and (rationalp k) (rationalp b) (plusp b))
                             (let ((exact (rational-power b k)))
                               (if (numberp exact) exact
                                   (expt (coerce b 'double-float) (coerce k 'double-float)))))
                            (t (float-or-rational
                                (expt (coerce b 'double-float)
                                      (if (rationalp k) (coerce k 'double-float) k)))))))
                   (t (let ((a (ev (second x))))
                        (when (and (eq (car x) :log) (realp a) (zerop a))
                          (cas-error 'cas-error "log of zero"))
                        (let ((a (if (rationalp a) (coerce a 'double-float) a)))
                          (float-or-rational
                           (ecase (car x)
                             (:sin (sin a)) (:cos (cos a)) (:tan (tan a))
                             (:exp (exp a)) (:log (log a))))))))))
    (ev e)))

;;; --------------------------------------------------------------- expansion

(defun expand (e)
  "Distribute products over sums and expand positive integer powers of sums."
  (simp (expand* (simp e))))

(defun expand* (e)
  (cond ((atom e) e)
        ((sum-p e) (simp-sum (mapcar #'expand* (cdr e))))
        ((product-p e) (reduce #'expand-mul (mapcar #'expand* (cdr e)) :initial-value 1))
        ((power-p e)
         (let ((b (expand* (second e))) (x (third e)))
           (if (and (sum-p b) (integerp x) (> x 0))
               (expand-power b x)
               (simp-power b x))))
        (t (simp-function (car e) (expand* (second e))))))

(defun expand-mul (a b)
  (cond ((sum-p a) (simp-sum (mapcar (lambda (term) (expand-mul term b)) (cdr a))))
        ((sum-p b) (simp-sum (mapcar (lambda (term) (expand-mul a term)) (cdr b))))
        (t (let ((p (simp-product (list a b))))
             ;; merging factors can expose a sum, e.g. (x+1)^(1/2)*(x+1)^(1/2)
             (if (and (product-p p) (some #'sum-p (cdr p)))
                 (reduce #'expand-mul (cdr p) :initial-value 1)
                 p)))))

(defun expand-power (b n)
  "Expand the sum B raised to the positive integer N by binary powering."
  (cond ((= n 1) b)
        ((evenp n) (let ((h (expand-power b (/ n 2)))) (expand-mul h h)))
        (t (expand-mul b (expand-power b (1- n))))))
