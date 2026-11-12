;;;; expr.lisp -- expression representation, predicates and the canonical order
;;;;
;;;; An expression is one of
;;;;   * a rational number (integers and ratios; floats never appear),
;;;;   * a variable: a symbol interned in POLYCAS-VARS,
;;;;   * (:+ a b ...), (:* a b ...), (:^ base exponent),
;;;;   * (fn arg) where fn is one of :sin :cos :tan :exp :log.
;;;; Subtraction and division are encoded as (:* -1 x) and (:^ x -1).

(in-package #:polycas)

(define-condition cas-error (error)
  ((message :initarg :message :reader cas-error-message))
  (:report (lambda (c s) (write-string (cas-error-message c) s))))

(define-condition parse-error* (cas-error) ()
  (:documentation "Malformed input text."))

(define-condition not-polynomial (cas-error) ()
  (:documentation "An expression is not a polynomial in the requested variables."))

(defun cas-error (type fmt &rest args)
  (error type :message (apply #'format nil fmt args)))

(defparameter +functions+ '(:sin :cos :tan :exp :log)
  "Elementary functions understood by the simplifier and differentiator.")

(defun var (name)
  "Return the variable named NAME (a string, case preserved)."
  (intern name '#:polycas-vars))

(defun var-p (e)
  (and (symbolp e) (eq (symbol-package e) (find-package '#:polycas-vars))))

(defun sum-p (e) (and (consp e) (eq (car e) :+)))
(defun product-p (e) (and (consp e) (eq (car e) :*)))
(defun power-p (e) (and (consp e) (eq (car e) :^)))
(defun function-p (e) (and (consp e) (member (car e) +functions+) t))

(defun expr= (a b) (equal a b))

(defun op-rank (op)
  (or (position op '(:+ :* :^ :sin :cos :tan :exp :log))
      (error "unknown operator ~S" op)))

(defun expr< (a b)
  "A strict total order on expressions, used to sort operands canonically.
Numbers come first (by value), then variables (by name), then compound
expressions ordered by operator and then lexicographically by operands."
  (cond ((numberp a) (if (numberp b) (< a b) t))
        ((numberp b) nil)
        ((symbolp a) (if (symbolp b) (string< (symbol-name a) (symbol-name b)) t))
        ((symbolp b) nil)
        ((/= (op-rank (car a)) (op-rank (car b)))
         (< (op-rank (car a)) (op-rank (car b))))
        (t (let ((xs (cdr a)) (ys (cdr b)))
             (loop
               (cond ((null xs) (return (not (null ys))))
                     ((null ys) (return nil))
                     ((not (expr= (car xs) (car ys)))
                      (return (expr< (car xs) (car ys)))))
               (setf xs (cdr xs) ys (cdr ys)))))))

(defun free-of-p (e v)
  "True when variable V does not occur in E."
  (cond ((atom e) (not (eq e v)))
        (t (every (lambda (x) (free-of-p x v)) (cdr e)))))

(defun variables (e)
  "Sorted list of the distinct variables occurring in E."
  (let ((acc '()))
    (labels ((walk (x)
               (cond ((var-p x) (pushnew x acc))
                     ((consp x) (mapc #'walk (cdr x))))))
      (walk e))
    (sort acc #'string< :key #'symbol-name)))
