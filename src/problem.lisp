
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

           #:problem
           #:problem-type
           #:problem-vars
           #:problem-objective-var
           #:problem-objective-func
           #:problem-signed-vars
           #:problem-integer-vars
           #:problem-constraints)
  (:documentation "Handles the representation of linear programming problems."))

(in-package :linear-programming/problem)

(defstruct problem
  "The representation of a linear programming problem."
  (type 'max :read-only t :type (member max min))
  (vars #() :read-only t :type (vector symbol))
  (objective-var '#:z :read-only t :type symbol)
  (objective-func nil :read-only t :type list)
  (signed-vars nil :read-only t :type list)
  (integer-vars nil :read-only t :type list)
  (constraints nil :read-only t :type list))

(setf (documentation 'problem-type 'function) "Whether the problem is a `min` or `max` problem."
      (documentation 'problem-vars 'function) "An array of the variables specified in the problem."
      (documentation 'problem-objective-var 'function) "The name of the objective function."
      (documentation 'problem-objective-func 'function) "The objective function as a linear expression alist."
      (documentation 'problem-signed-vars 'function) "A list of variables without positivity constraints."
      (documentation 'problem-integer-vars 'function) "A list of variables with integer constraints."
      (documentation 'problem-constraints 'function) "A list of (in)equality constraints.")

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
           (objective-func (parse-linear-expression (second objective)))
           (parsed-constraints (parse-linear-constraints constraints))
           (eq-constraints (first parsed-constraints))
           (signed-constraints (second parsed-constraints))
           (integer-constraints (third parsed-constraints))
           ;collect all of the variables referenced
           (var-list (remove-duplicates (mapcar #'car objective-func)))
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
      (make-problem :type type
                    :vars variables
                    :objective-var objective-var
                    :objective-func objective-func
                    :signed-vars signed-constraints
                    :integer-vars integer-constraints
                    :constraints eq-constraints))))


(defmacro make-linear-problem (objective &rest constraints)
  "Creates a linear problem from the expressions in the body"
  `(parse-linear-problem ',objective ',constraints))
