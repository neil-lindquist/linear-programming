
(uiop:define-package :linear-programming/problem
  (:use :cl
         :alexandria
         :iterate
         :linear-programming/conditions
         :linear-programming/expressions)
  (:export #:make-linear-problem
           #:parse-linear-problem

           #:parsing-error

           #:min
           #:max
           #:signed
           #:integer
           #:binary
           #:<=
           #:>=
           #:<
           #:>
           #:=
           #:+
           #:*

           #:linear-problem
           #:lp-type
           #:variables
           #:objective-variable
           #:objective-function
           #:signed-vars
           #:integer-vars
           #:constraints)
  (:documentation "Handles the representation of linear programming problems."))

(in-package :linear-programming/problem)

(defclass linear-problem ()
  ((type :reader lp-type
         :initarg :type
         :type (member max min))
   (variables :reader variables
              :initarg :variables
              :type (vector symbol)
              :documentation "A vector with the decision variables of the problem")
   (objective-var :reader objective-variable
                  :initarg :objective-variable
                  :initform (gensym "z")
                  :type symbol
                  :documentation "A symbol for the variable representing the objective function")
   (objective :reader objective-function
              :initarg :objective
              :type list
              :documentation "A linear expression for the objective function")
   (signed-vars :reader signed-vars
                :initarg :signed
                :type list
                :documentation "A list of variables that are non negative")
   (integer-vars :reader integer-vars
                 :initarg :integer
                 :type list
                 :documentation "A list of variables that are integer valued")
   (constraints :reader constraints
                :initarg :constraints
                :type list
                :documentation "A list of simple inequality contraints"))
  (:documentation "The representation of a linear programming problem."))

(setf (documentation 'lp-type 'function) "Whether the problem is a `min` or `max` problem."
      (documentation 'variables 'function) "An array of the variables specified in the problem."
      (documentation 'objective-variable 'function) "The name of the objective function."
      (documentation 'objective-function 'function) "The objective function as a linear expression alist."
      (documentation 'signed-vars 'function) "A list of variables without positivity constraints."
      (documentation 'integer-vars 'function) "A list of variables with integer constraints."
      (documentation 'constraints 'function) "A list of (in)equality constraints.")

(declaim (inline simple-eq))
(defun simple-eq (op exp1 exp2)
  "Takes the rhs and lhs of an in/equality and moves any constant to the rhs
   as a number and any non-constant values to the lhs as a linear expression."
  (let* ((lin-exp (sum-linear-expressions
                      exp1 (scale-linear-expression exp2 -1)))
         (const (cdr (assoc '+constant+ lin-exp :test 'eq)))
         (sum (delete '+constant+ lin-exp :test 'eq :key 'car)))
    (ecase op
      (<= (cond
            ((null const) (list '<= sum 0))
            ((> const 0) (list '>= (scale-linear-expression sum -1) const))
            (t (list '<= sum (- const)))))
      (= (list '= sum (if const (- const) 0))))))

(defun parse-linear-constraints (exprs)
  "Parses the list of constraints and returns a list containing a list of simple
   inequalities, a list of signed variables, and a list of integer variables"
  (iter (for expr in exprs)
    (case (first expr)
      ((<= <)
       (collect (cons '<= (mapcar 'parse-linear-expression (rest expr)))
                into equalities))
      ((>= >)
       (collect (cons '<= (reverse (mapcar 'parse-linear-expression (rest expr))))
                into equalities))
      ((=)
       (collect (cons '= (mapcar 'parse-linear-expression (rest expr)))
                into equalities))
      ((signed)
       (unioning (rest expr)
                 into signed))
      ((integer)
       (unioning (rest expr)
                 into integer))
      ((binary)
       (unioning (rest expr)
                 into integer)
       (appending (mapcar (lambda (var) `(<= ((,var . 1)) ((+constant+ . 1))))
                          (rest expr))
               into equalities))
      (t (error 'parsing-error :description (format nil "~A is not a valid constraint" expr))))
    (finally
      (let ((simple-eqs
             (reduce 'append
                      equalities
                      :key #'(lambda (constraint)
                               (iter (for i from 2 below (length constraint))
                                 (collect (simple-eq (first constraint)
                                                     (nth (1- i) constraint)
                                                     (nth i constraint))))))))
        (return (list simple-eqs signed integer))))))


(defun parse-linear-problem (objective-exp constraints)
  "Parses the expressions into a linear programming problem"
  (let* ((objective-var-p (eq (first objective-exp) '=))
         (objective (if objective-var-p
                      (third objective-exp)
                      objective-exp))
         (objective-var (if objective-var-p
                          (second objective-exp)
                          (gensym "z"))))
    (when (and (not objective-var-p)
               (eq (first (second objective)) '=))
      (setf objective-var (second (second objective)))
      (setf objective (list (first objective) (third (second objective))))
      (setf objective-var-p t))
    (unless (member (first objective) '(min max) :test 'eq)
      (error 'parsing-error
             :description (format nil "~A is neither min nor max in objective function ~A"
                                      (first objective) objective)))
    (let* ((type (first objective))
           (objective-function (parse-linear-expression (second objective)))
           (parsed-constraints (parse-linear-constraints constraints))
           (eq-constraints (first parsed-constraints))
           (signed-constraints (second parsed-constraints))
           (integer-constraints (third parsed-constraints))
           ;collect all of the variables referenced
           (var-list (remove-duplicates (mapcar #'car objective-function)))
           (var-list (union var-list signed-constraints))
           (var-list (union var-list integer-constraints))
           (var-list (union var-list
                            (reduce (lambda (l1 l2) (union l1 (mapcar #'car l2)))
                                    eq-constraints
                                    :key 'second
                                    :initial-value nil)))
           (variables (make-array (length var-list)
                                  :initial-contents var-list
                                  :element-type 'symbol)))
      (make-instance 'linear-problem
                     :type type
                     :variables variables
                     :objective-variable objective-var
                     :objective objective-function
                     :signed signed-constraints
                     :integer integer-constraints
                     :constraints eq-constraints))))


(defmacro make-linear-problem (objective &rest constraints)
  "Creates a linear problem from the expressions in the body"
  `(parse-linear-problem ',objective ',constraints))
