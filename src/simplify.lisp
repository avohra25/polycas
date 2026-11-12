;;;; simplify.lisp -- automatic simplification to a canonical form
;;;;
;;;; SIMP maps any expression to a canonical representative:
;;;;   * sums and products are flattened, their operands sorted by EXPR<,
;;;;   * numeric parts are folded exactly (a product carries at most one
;;;;     leading rational coefficient, a sum at most one leading constant),
;;;;   * like terms (same non-numeric part) and like factors (same base)
;;;;     are collected,
;;;;   * integer powers of products and of powers are distributed,
;;;;   * rational powers of positive rationals are reduced: exact roots are
;;;;     taken and b^(k+r) with 0<r<1 becomes b^k * b^r,
;;;;   * a few function identities (sin 0, exp(log x), ...) are applied.
;;;; Two expressions that differ only by these rules simplify to EQUAL trees.

(in-package #:polycas)

(defun simp (e)
  (if (atom e)
      e
      (let ((op (car e))
            (args (mapcar #'simp (cdr e))))
        (case op
          (:+ (simp-sum args))
          (:* (simp-product args))
          (:^ (simp-power (first args) (second args)))
          (t (simp-function op (first args)))))))

;;; ------------------------------------------------------------------ sums

(defun split-term (term)
  "Split TERM into (values coefficient rest) where rest has no numeric factor."
  (if (and (product-p term) (numberp (second term)))
      (let ((rest (cddr term)))
        (values (second term) (if (cdr rest) (cons :* rest) (car rest))))
      (values 1 term)))

(defun attach-coefficient (c rest)
  (cond ((= c 1) rest)
        ((product-p rest) (list* :* c (cdr rest)))
        (t (list :* c rest))))

(defun simp-sum (args)
  "Simplify a sum whose operands are already simplified."
  (let ((constant 0)
        (table (make-hash-table :test #'equal))
        (keys '()))
    (labels ((add (a)
               (cond ((numberp a) (incf constant a))
                     ((sum-p a) (mapc #'add (cdr a)))
                     (t (multiple-value-bind (c rest) (split-term a)
                          (multiple-value-bind (old found) (gethash rest table)
                            (unless found (push rest keys))
                            (setf (gethash rest table) (+ c (if found old 0)))))))))
      (mapc #'add args))
    (let ((terms (loop for k in keys
                       for c = (gethash k table)
                       unless (zerop c) collect (attach-coefficient c k))))
      (setf terms (sort terms #'expr<))
      (unless (zerop constant) (push constant terms))
      (cond ((null terms) 0)
            ((null (cdr terms)) (car terms))
            (t (cons :+ terms))))))

;;; -------------------------------------------------------------- products

(defun split-power (factor)
  (if (power-p factor)
      (values (second factor) (third factor))
      (values factor 1)))

(defun simp-product (args)
  "Simplify a product whose operands are already simplified."
  (let ((coef 1)
        (table (make-hash-table :test #'equal))
        (keys '()))
    (labels ((add (a)
               (cond ((numberp a) (setf coef (* coef a)))
                     ((product-p a) (mapc #'add (cdr a)))
                     (t (multiple-value-bind (base ex) (split-power a)
                          (multiple-value-bind (old found) (gethash base table)
                            (unless found (push base keys))
                            (setf (gethash base table) (cons ex (if found old '())))))))))
      (mapc #'add args)
      (when (zerop coef) (return-from simp-product 0))
      (let ((factors '()) (redo '()))
        (dolist (base keys)
          (let ((p (simp-power base (simp-sum (gethash base table)))))
            (cond ((numberp p) (setf coef (* coef p)))
                  ((product-p p) (setf redo (append (cdr p) redo)))
                  (t (push p factors)))))
        (when redo
          (return-from simp-product (simp-product (list* coef (append factors redo)))))
        (when (zerop coef) (return-from simp-product 0))
        (setf factors (sort factors #'expr<))
        (cond ((null factors) coef)
              ((and (= coef 1) (null (cdr factors))) (car factors))
              ((= coef 1) (cons :* factors))
              (t (list* :* coef factors)))))))

;;; ---------------------------------------------------------------- powers

(defun exact-root (n k)
  "Integer K-th root of the non-negative integer N, or NIL if N is not a perfect power."
  (cond ((< n 2) n)
        (t (let ((r (round (expt (coerce n 'double-float) (/ 1d0 k)))))
             ;; correct the floating estimate, which may be off for big N
             (loop while (> (expt r k) n) do (decf r))
             (loop while (<= (expt (1+ r) k) n) do (incf r))
             (when (= (expt r k) n) r)))))

(defun rational-power (b e)
  "B a positive rational, E a non-integer rational."
  (let* ((m (numerator e)) (n (denominator e))
         (pr (exact-root (numerator b) n))
         (qr (exact-root (denominator b) n)))
    (if (and pr qr)
        (expt (/ pr qr) m)
        (let* ((k (floor e)) (r (- e k)))
          (if (zerop k)
              (list :^ b e)
              (simp-product (list (expt b k) (list :^ b r))))))))

(defun simp-power (b e)
  "Simplify B^E where B and E are already simplified."
  (cond ((eql e 0) 1)
        ((eql e 1) b)
        ((numberp b)
         (cond ((zerop b)
                (cond ((and (numberp e) (plusp e)) 0)
                      ((numberp e) (cas-error 'cas-error "division by zero"))
                      (t (list :^ b e))))
               ((= b 1) 1)
               ((integerp e) (expt b e))
               ((and (rationalp e) (plusp b)) (rational-power b e))
               (t (list :^ b e))))
        ((and (power-p b) (integerp e))
         (simp-power (second b) (simp-product (list (third b) e))))
        ((and (product-p b) (integerp e))
         (simp-product (mapcar (lambda (f) (simp-power f e)) (cdr b))))
        (t (list :^ b e))))

;;; ------------------------------------------------------------- functions

(defun simp-function (op a)
  (case op
    (:sin (if (eql a 0) 0 (list :sin a)))
    (:cos (if (eql a 0) 1 (list :cos a)))
    (:tan (if (eql a 0) 0 (list :tan a)))
    (:exp (cond ((eql a 0) 1)
                ((and (consp a) (eq (car a) :log)) (second a))
                (t (list :exp a))))
    (:log (cond ((eql a 1) 0)
                ((and (consp a) (eq (car a) :exp)) (second a))
                (t (list :log a))))
    (t (cas-error 'cas-error "unknown function ~(~A~)" op))))
