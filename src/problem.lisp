
(uiop:define-package :linear-programming/problem
  (:use :cl
         :alexandria
         :iterate
         :linear-programming/expressions)
  (:export #:make-linear-problem
           #:parse-linear-problem

           #:non-neg
           #:<=
           #:>=
           #:<
           #:>
           #:+
           #:*

           #:linear-problem
           #:lp-type
           #:variables
           #:objective-function
           #:non-neg-vars
           #:constraints))

(in-package :linear-programming/problem)

(defclass linear-problem ()
  ((type :reader lp-type
         :initarg :type
         :type (member (:max :min)))
   (variables :reader variables
              :initarg :variables
              :type (vector symbol)
              :documentation "A vector with the variables in the problem")
   (objective :reader objective-function
              :initarg :objective
              :type list
              :documentation "A linear expression for the objective function")
   (non-neg-vars :reader non-neg-vars
                 :initarg :non-neg
                 :type list
                 :documentation "A list of variables that are non negative")
   (constraints :reader constraints
                :initarg :constraints
                :type list
                :documentation "A list of simple inequality contraints")))

(defun simplify-equality (eq)
  "Takes an <= equality and makes any constant the rhs as a number and the
   non-constant values the lhs as a linear expression"
  (let* ((lin-exp (sum-linear-expressions
                    (list (second eq)
                          (scale-linear-expression (third eq) -1))))
         (const (cdr (assoc '+constant+
                            lin-exp
                            :test 'eq)))
         (sum (delete '+constant+
                      lin-exp
                      :test 'eq
                      :key 'car)))
    (cond
      ((null const)
       (list '<= sum 0))
      ((> const 0)
       (list '>= (scale-linear-expression sum -1) (- const)))
      (t
       (list '<= sum (- const))))))

(defun parse-linear-constraints (exprs)
  "Parses the list of constraints and returns a list containing a list of simple
   inequalities and a list of non-negative variables"
  (iter (for expr in exprs)
    (case (first expr)
      ((<= <)
       (collect (cons '<= (mapcar 'parse-linear-expression (rest expr)))
                into equalities))
      ((>= >)
       (collect (cons '<= (reverse (mapcar 'parse-linear-expression (rest expr))))
                into equalities))
      ((non-neg)
       (unioning (rest expr)
                 into non-negs))
      (t (error "~A is not a valid constraint" expr)))
    (finally
      (let* ((split-eqs (reduce 'append
                                 equalities
                                 :key #'(lambda (constraint)
                                          (iter (for i from 2 below (length constraint))
                                            (collect (list '<=
                                                           (nth (1- i) constraint)
                                                           (nth i constraint)))))))
             (simple-eqs (mapcar 'simplify-equality
                                 split-eqs)))
        (return (list simple-eqs non-negs))))))


(defun parse-linear-problem (objective constraints)
  "Parses the expressions into a linear programming problem"
  (unless (member (first objective) '(min max) :test 'eq)
    (error "~A is not min or max in objective function ~A" (first objective) objective))
  (let* ((type (first objective))
         (objective-function (parse-linear-expression (second objective)))
         (parsed-constraints (parse-linear-constraints constraints))
         (eq-constraints (first parsed-constraints))
         (bound-constraints (second parsed-constraints))
         (var-list (union
                     (union (mapcar 'car objective-function)
                            bound-constraints)
                     (reduce (lambda (l1 l2) (union l1 (mapcar 'car l2)))
                             eq-constraints
                             :key (compose 'second)
                             :initial-value nil)))
         (variables (make-array (length var-list)
                                :initial-contents var-list
                                :element-type 'symbol)))
    (make-instance 'linear-problem
                   :type type
                   :variables variables
                   :objective objective-function
                   :non-neg bound-constraints
                   :constraints eq-constraints)))


(defmacro make-linear-problem (objective &rest constraints)
  "Creates a linear problem from the expressions in the body"
  `(parse-linear-problem ',objective ',constraints))
