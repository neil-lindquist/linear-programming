
(uiop:define-package :linear-programming/expressions
  (:use :cl
        :iterate
        :linear-programming/conditions)
  (:import-from :alexandria
                #:if-let
                #:plist-alist)
  (:export #:scale-linear-expression
           #:sum-linear-expressions
           #:parse-linear-expression
           #:format-linear-expression
           #:+constant+)
  (:documentation "Contains functions for processing linear expressions."))

(in-package :linear-programming/expressions)

(declaim (inline linear-constant-p))
(defun linear-constant-p (expr)
  "A predicate for whether a linear expression is constant."
  (and (= 1 (length expr))
       (eq (car (first expr)) '+constant+)))

(declaim (inline sum-linear-expressions))
(defun sum-linear-expressions (&rest exprs)
  "Takes linear expressions and reduces it into a single expression."
  (let ((sum (copy-alist (first exprs))))
    (iter (for expr in (rest exprs))
      (iter (for term in expr)
        (if-let (pair (assoc (car term) sum))
          (incf (cdr pair) (cdr term))
          (push (cons (car term) (cdr term)) sum))))
    sum))


(declaim (inline scale-linear-expression))
(defun scale-linear-expression (expr scalar)
  "Multiplies the linear expression by the given scalar."
  (mapcar #'(lambda (x) (cons (car x) (* scalar (cdr x))))
          expr))


(defun parse-linear-expression (expr)
  "Parses the expression into a alist mapping variables to coefficients. Any
constant values are mapped to `+constant+`."
  (cond
    ; atoms
    ((symbolp expr)
     (list (cons expr 1)))
    ((numberp expr)
     (list (cons '+constant+ expr)))

    ; lists
    ((listp expr)
     (case (first expr)
       ; low-level specifiers
       ((:alist)
        (rest expr))
       ((:plist)
        (plist-alist (rest expr)))

       ; arithmetic
       ((+)
        (sum-linear-expressions-list (mapcar 'parse-linear-expression (rest expr))))

       ((*)
        (let ((factors (mapcar #'parse-linear-expression (rest expr)))
              (variable nil)
              (constant 1))
          (dolist (fact factors)
            (cond
              ((linear-constant-p fact)
               (setf constant (* constant (cdar fact))))
              (variable
               (error 'nonlinear-error :expression expr))
              (t
               (setf variable fact))))
          (if variable
            (scale-linear-expression variable constant)
            `((+constant+ . ,constant)))))

       ((-)
        (if (= 2 (length expr))
          (scale-linear-expression (parse-linear-expression (second expr))
                                   -1)
          (sum-linear-expressions (parse-linear-expression (second expr))
                                  (scale-linear-expression
                                    (parse-linear-expression (list* '+ (nthcdr 2 expr)))
                                    -1))))

       ((/)
        (if (= 2 (length expr))
          (let ((val (parse-linear-expression (second expr))))
            (unless (linear-constant-p val)
              (error 'nonlinear-error :expression expr))
            `((+constant+ . ,(/ (cdar val)))))
          (let ((divisors (mapcar #'parse-linear-expression (nthcdr 2 expr)))
                (dividend (parse-linear-expression (second expr))))
            (unless (every #'linear-constant-p divisors)
              (error 'nonlinear-error :expression expr))
            (scale-linear-expression dividend (/ (reduce #'* divisors :key #'cdar))))))

       ; Unknown functions
       (t (error 'nonlinear-error :expression expr))))

    ; Unknown objecects
    (t (error 'parsing-error
              :description (format nil "~S is not a symbol, number, or an expression" expr)))))


(defun format-linear-expression (alist)
  "Formats a linear expression as a sexp."
  (cons '+
       (mapcar (lambda (pair)
                 (if (eq (car pair) '+constant+)
                   (cdr pair)
                   (list '* (cdr pair) (car pair))))
               alist)))
