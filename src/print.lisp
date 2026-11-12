;;;; print.lisp -- expressions to readable infix text
;;;;
;;;; The output re-parses to the same canonical expression:
;;;;   (equal (read-expr (to-infix e)) e) for every simplified E.

(in-package #:polycas)

;;; Sums are displayed in "polynomial order": higher total degree first and,
;;; within a degree, lexicographically by variable name (x^2, x*y, y^2),
;;; with other terms before the constant. Only display is affected.

(defun display-monomial (e)
  "Alist (variable-name . exponent) of the polynomial part of term E."
  (let ((acc '()))
    (labels ((walk (f)
               (cond ((var-p f) (push (cons (symbol-name f) 1) acc))
                     ((and (power-p f) (var-p (second f)) (integerp (third f)) (plusp (third f)))
                      (push (cons (symbol-name (second f)) (third f)) acc))
                     ((product-p f) (mapc #'walk (cdr f))))))
      (walk e))
    (sort acc #'string< :key #'car)))

(defun display-before-p (a b)
  "True when sum term A should be printed before term B."
  (cond ((numberp a) nil)
        ((numberp b) t)
        (t (let* ((ma (display-monomial a)) (mb (display-monomial b))
                  (da (reduce #'+ ma :key #'cdr)) (db (reduce #'+ mb :key #'cdr)))
             (if (/= da db)
                 (> da db)
                 ;; lexicographic comparison of exponent vectors
                 (loop for name in (sort (remove-duplicates (mapcar #'car (append ma mb))
                                                            :test #'string=)
                                         #'string<)
                       for ea = (or (cdr (assoc name ma :test #'string=)) 0)
                       for eb = (or (cdr (assoc name mb :test #'string=)) 0)
                       when (/= ea eb) return (> ea eb)
                       finally (return nil)))))))

(defun negative-term-p (e)
  (or (and (numberp e) (minusp e))
      (and (product-p e) (numberp (second e)) (minusp (second e)))))

(defun negate-term (e)
  (if (numberp e) (- e) (simp-product (list -1 e))))

(defun paren (s) (concatenate 'string "(" s ")"))

(defun number-string (n) (format nil "~A" n))

(defun to-infix (e)
  "Render expression E as infix text."
  (fmt e 0))

(defun fmt (e prec)
  "Render E, parenthesising when its own precedence is below PREC.
Precedences: sum 1, product/quotient 2, power 3, atom 4."
  (multiple-value-bind (s own) (fmt-raw e)
    (if (< own prec) (paren s) s)))

(defun fmt-raw (e)
  (cond ((numberp e)
         (values (number-string e) (if (and (integerp e) (>= e 0)) 4 1)))
        ((var-p e) (values (symbol-name e) 4))
        ((sum-p e) (values (fmt-sum e) 1))
        ((product-p e) (values (fmt-product (second e) (cddr e) (cdr e)) 2))
        ((power-p e)
         (if (and (numberp (third e)) (minusp (third e)))
             (values (fmt-product 1 (list e) (list e)) 2)
             (values (format nil "~A^~A" (fmt (second e) 4) (fmt (third e) 4)) 3)))
        ((function-p e)
         (values (format nil "~(~A~)(~A)" (car e) (fmt (second e) 0)) 4))
        (t (error "cannot print ~S" e))))

(defun fmt-sum (e)
  (let ((terms (stable-sort (copy-list (cdr e)) #'display-before-p))
        (out (make-string-output-stream)))
    (loop for term in terms
          for first = t then nil
          do (cond (first (write-string (fmt term 1) out))
                   ((negative-term-p term)
                    (write-string " - " out)
                    (write-string (fmt (negate-term term) 2) out))
                   (t (write-string " + " out)
                      (write-string (fmt term 2) out))))
    (get-output-stream-string out)))

(defun display-base (f) (if (power-p f) (second f) f))

(defun fmt-product (head tail all)
  "Render a product. HEAD/TAIL are its first operand and the rest; ALL is
the full operand list (used when there is no numeric coefficient)."
  (let* ((coef (if (numberp head) head 1))
         ;; display factors by base so that x^2*y reads naturally
         (factors (stable-sort (copy-list (if (numberp head) tail all))
                               #'expr< :key #'display-base))
         (num '()) (den '()))
    (dolist (f factors)
      (if (and (power-p f) (numberp (third f)) (minusp (third f)))
          (push (simp-power (second f) (- (third f))) den)
          (push f num)))
    (setf num (nreverse num) den (nreverse den))
    (let* ((sign (if (minusp coef) "-" ""))
           (p (abs (numerator coef))) (q (denominator coef))
           (num-parts (append (when (or (/= p 1) (null num)) (list (number-string p)))
                              (mapcar (lambda (f) (fmt f 2)) num)))
           (den-parts (append (when (/= q 1) (list (number-string q)))
                              (mapcar (lambda (f) (fmt f 3)) den)))
           (num-s (format nil "~{~A~^*~}" num-parts)))
      (cond ((null den-parts) (concatenate 'string sign num-s))
            ((null (cdr den-parts))
             (format nil "~A~A/~A" sign num-s (first den-parts)))
            (t (format nil "~A~A/(~{~A~^*~})" sign num-s den-parts))))))
