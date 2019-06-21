
(uiop:define-package :linear-programming/problem
  (:use :cl
         :alexandria
         :iterate
         :linear-programming/expressions)
  (:export #:make-linear-problem
           #:parse-linear-problem

           #:min
           #:max
           #:signed
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
           #:constraints))

(in-package :linear-programming/problem)

(defclass linear-problem ()
  ((type :reader lp-type
         :initarg :type
         :type (member (max min)))
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
   (constraints :reader constraints
                :initarg :constraints
                :type list
                :documentation "A list of simple inequality contraints")))

(declaim (inline simple-eq))
(defun simple-eq (op exp1 exp2)
  "Takes the rhs and lhs of an in/equality and moves any constant to the rhs
   as a number and any non-constant values to the lhs as a linear expression."
  (let* ((lin-exp (sum-linear-expressions
                      (list exp1
                            (scale-linear-expression exp2 -1))))
         (const (cdr (assoc '+constant+
                            lin-exp
                            :test 'eq)))
         (sum (delete '+constant+
                      lin-exp
                      :test 'eq
                      :key 'car)))
    (case op
      (<= (cond
            ((null const)
             (list '<= sum 0))
            ((> const 0)
             (list '>= (scale-linear-expression sum -1) const))
            (t
             (list '<= sum (- const)))))
      (= (if (null const)
           (list '= sum 0)
           (list '= sum (- const)))))))

(defun parse-linear-constraints (exprs)
  "Parses the list of constraints and returns a list containing a list of simple
   inequalities and a list of signed variables"
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
      (t (error "~A is not a valid constraint" expr)))
    (finally
      (let ((simple-eqs
             (reduce 'append
                      equalities
                      :key #'(lambda (constraint)
                               (iter (for i from 2 below (length constraint))
                                 (collect (simple-eq (first constraint)
                                                     (nth (1- i) constraint)
                                                     (nth i constraint))))))))
        (return (list simple-eqs signed))))))


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
      (error "~A is not min or max in objective function ~A" (first objective) objective))
    (let* ((type (first objective))
           (objective-function (parse-linear-expression (second objective)))
           (parsed-constraints (parse-linear-constraints constraints))
           (eq-constraints (first parsed-constraints))
           (signed-constraints (second parsed-constraints))
           (var-list (union
                       (union (mapcar #'car objective-function)
                              signed-constraints)
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
                     :constraints eq-constraints))))


(defmacro make-linear-problem (objective &rest constraints)
  "Creates a linear problem from the expressions in the body"
  `(parse-linear-problem ',objective ',constraints))
