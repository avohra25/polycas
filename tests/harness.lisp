;;;; harness.lisp -- a tiny dependency-free test harness

(defpackage #:polycas-tests
  (:use #:cl #:polycas)
  (:export #:run-all))

(in-package #:polycas-tests)

(defvar *tests* '() "Registered tests, newest first, as (name . function).")
(defvar *failures*)
(defvar *checks*)

(defmacro deftest (name &body body)
  `(progn
     (setf *tests* (cons (cons ',name (lambda () ,@body))
                         (remove ',name *tests* :key #'car)))
     ',name))

(defun record-failure (fmt &rest args)
  (push (apply #'format nil fmt args) *failures*))

(defmacro check (form &optional description)
  `(progn (incf *checks*)
          (unless ,form
            (record-failure "check failed: ~A" ,(or description `(quote ,form))))))

(defmacro check-equal (expected form &optional description)
  (declare (ignore description))
  (let ((e (gensym)) (v (gensym)))
    `(let ((,e ,expected) (,v ,form))
       (incf *checks*)
       (unless (equal ,e ,v)
         (record-failure "~S~%      expected ~S~%      got      ~S" ',form ,e ,v)))))

(defmacro check-error (type form)
  `(progn (incf *checks*)
          (handler-case (progn ,form (record-failure "expected ~A from ~S" ',type ',form))
            (,type () t))))

;;; deterministic pseudo-random numbers (xorshift64*) so failures reproduce
(defvar *rng* 88172645463325252)

(defun seed (n) (setf *rng* (logior 1 (ldb (byte 64 0) (* n 2685821657736338717)))))

(defun rnd (n)
  "Pseudo-random integer in [0, N)."
  (let ((x *rng*))
    (setf x (logxor x (ldb (byte 64 0) (ash x 12))))
    (setf x (logxor x (ash x -25)))
    (setf x (logxor x (ldb (byte 64 0) (ash x 27))))
    (setf *rng* x)
    (mod (ash (ldb (byte 64 0) (* x 2685821657736338717)) -11) n)))

(defun rnd-between (lo hi) (+ lo (rnd (1+ (- hi lo)))))

(defun shuffle (list)
  "A Fisher-Yates shuffled copy of LIST."
  (let ((v (coerce list 'vector)))
    (loop for i from (1- (length v)) downto 1
          do (rotatef (aref v i) (aref v (rnd (1+ i)))))
    (coerce v 'list)))

(defun run-all (&key (verbose nil))
  "Run every registered test; return T when all pass."
  (let ((total-checks 0) (failed '()))
    (dolist (entry (reverse *tests*))
      (let ((*failures* '()) (*checks* 0))
        (handler-case (funcall (cdr entry))
          (error (c) (record-failure "unhandled error: ~A" c)))
        (incf total-checks *checks*)
        (cond (*failures*
               (push (car entry) failed)
               (format t "FAIL ~(~A~)~%" (car entry))
               (dolist (f (reverse *failures*)) (format t "    ~A~%" f)))
              (verbose (format t "ok   ~(~A~) (~D checks)~%" (car entry) *checks*)))))
    (format t "~&~D tests, ~D checks, ~D failed~%" (length *tests*) total-checks (length failed))
    (null failed)))
