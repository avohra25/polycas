;;;; parse.lisp -- infix text to expressions
;;;;
;;;; Grammar (lowest to highest precedence):
;;;;   equation := expr [ '=' expr ]
;;;;   expr     := term { ('+' | '-') term }
;;;;   term     := unary { ('*' | '/') unary | <implicit product> }
;;;;   unary    := ('-' | '+') unary | power
;;;;   power    := primary [ '^' unary ]          ; right associative
;;;;   primary  := number | name | name '(' expr ')' | '(' expr ')'
;;;; Numbers may be decimals ("1.25" reads as the exact rational 5/4).
;;;; Juxtaposition multiplies: "2x y" is 2*x*y, "3(x+1)" is 3*(x+1).

(in-package #:polycas)

(defparameter +function-names+
  '(("sin" . :sin) ("cos" . :cos) ("tan" . :tan) ("exp" . :exp)
    ("log" . :log) ("ln" . :log) ("sqrt" . :sqrt)))

(defun tokenize (string)
  "Return a list of tokens: rationals, (:name . string), or single characters."
  (let ((tokens '()) (i 0) (n (length string)))
    (flet ((peek () (and (< i n) (char string i))))
      (loop
        (let ((c (peek)))
          (cond ((null c) (return))
                ((member c '(#\Space #\Tab #\Newline #\Return)) (incf i))
                ((or (digit-char-p c)
                     (and (char= c #\.) (< (1+ i) n) (digit-char-p (char string (1+ i)))))
                 (let ((start i) (dot nil))
                   (loop while (and (peek) (or (digit-char-p (peek))
                                               (and (char= (peek) #\.) (not dot))))
                         do (when (char= (peek) #\.) (setf dot i)) (incf i))
                   (push (parse-decimal (subseq string start i)) tokens)))
                ((or (alpha-char-p c) (char= c #\_))
                 (let ((start i))
                   (loop while (and (peek) (or (alphanumericp (peek)) (char= (peek) #\_)))
                         do (incf i))
                   (push (cons :name (subseq string start i)) tokens)))
                ((find c "+-*/^()=,") (push c tokens) (incf i))
                (t (cas-error 'parse-error* "unexpected character ~S at position ~D" c i))))))
    (nreverse tokens)))

(defun parse-decimal (text)
  (let ((dot (position #\. text)))
    (if (null dot)
        (parse-integer text)
        (let ((int (subseq text 0 dot)) (frac (subseq text (1+ dot))))
          (+ (if (string= int "") 0 (parse-integer int))
             (if (string= frac "") 0 (/ (parse-integer frac) (expt 10 (length frac)))))))))

(defvar *tokens*)

(defun next-token () (first *tokens*))
(defun take-token () (pop *tokens*))

(defun expect (char)
  (let ((tok (take-token)))
    (unless (eql tok char)
      (cas-error 'parse-error* "expected '~A' but found ~A" char (describe-token tok)))))

(defun describe-token (tok)
  (cond ((null tok) "end of input")
        ((characterp tok) (format nil "'~A'" tok))
        ((numberp tok) (format nil "number ~A" tok))
        (t (format nil "name ~A" (cdr tok)))))

(defun parse-expr ()
  (let ((left (parse-term)))
    (loop
      (case (next-token)
        (#\+ (take-token) (setf left (list :+ left (parse-term))))
        (#\- (take-token) (setf left (list :+ left (list :* -1 (parse-term)))))
        (t (return left))))))

(defun starts-primary-p (tok)
  (or (numberp tok) (and (consp tok) (eq (car tok) :name)) (eql tok #\()))

(defun parse-term ()
  (let ((left (parse-unary)))
    (loop
      (let ((tok (next-token)))
        (cond ((eql tok #\*) (take-token) (setf left (list :* left (parse-unary))))
              ((eql tok #\/) (take-token) (setf left (list :* left (list :^ (parse-unary) -1))))
              ((starts-primary-p tok) (setf left (list :* left (parse-power))))
              (t (return left)))))))

(defun parse-unary ()
  (case (next-token)
    (#\- (take-token) (list :* -1 (parse-unary)))
    (#\+ (take-token) (parse-unary))
    (t (parse-power))))

(defun parse-power ()
  (let ((base (parse-primary)))
    (if (eql (next-token) #\^)
        (progn (take-token) (list :^ base (parse-unary)))
        base)))

(defun parse-primary ()
  (let ((tok (take-token)))
    (cond ((numberp tok) tok)
          ((eql tok #\()
           (prog1 (parse-expr) (expect #\))))
          ((and (consp tok) (eq (car tok) :name))
           (let ((fn (cdr (assoc (cdr tok) +function-names+ :test #'string=))))
             (cond ((and fn (eql (next-token) #\())
                    (take-token)
                    (let ((arg (parse-expr)))
                      (expect #\))
                      (if (eq fn :sqrt) (list :^ arg 1/2) (list fn arg))))
                   ((eql (next-token) #\()
                    (cas-error 'parse-error* "unknown function ~A" (cdr tok)))
                   (t (var (cdr tok))))))
          (t (cas-error 'parse-error* "unexpected ~A" (describe-token tok))))))

(defun parse (string)
  "Parse STRING into an (unsimplified) expression."
  (let ((*tokens* (tokenize string)))
    (when (null *tokens*) (cas-error 'parse-error* "empty expression"))
    (let ((e (parse-expr)))
      (when *tokens*
        (cas-error 'parse-error* "unexpected ~A after expression" (describe-token (next-token))))
      e)))

(defun parse-equation (string)
  "Parse \"lhs = rhs\" (or a bare expression, meaning expr = 0) and return
the simplified expression lhs - rhs."
  (let ((*tokens* (tokenize string)))
    (when (null *tokens*) (cas-error 'parse-error* "empty equation"))
    (let* ((lhs (parse-expr))
           (rhs (if (eql (next-token) #\=) (progn (take-token) (parse-expr)) 0)))
      (when *tokens*
        (cas-error 'parse-error* "unexpected ~A in equation" (describe-token (next-token))))
      (simp (list :+ lhs (list :* -1 rhs))))))

(defun read-expr (string)
  "Parse and simplify STRING."
  (simp (parse string)))
