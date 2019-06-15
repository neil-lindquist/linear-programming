
(uiop:define-package :linear-programming/problem
  (:use :cl
         :alexandria
         :iterate
         :linear-programming/expressions)
  (:export #:make-linear-problem))

(in-package :linear-programming/problem)

(defclass linear-problem ()
  ((type :reader lp-type
         :initarg :type
         :type (member (:max :min)))
   (variables :reader variables
              :initarg :variables
              :type (vector symbol))
   (objective-coefs :reader objective-coefs
                    :initarg :objective-coefs
                    :type (vector real))))

(defun simplify-equality (eq)
  "Takes an <= equality and makes any constant the rhs as a number and the
   non-constant values the lhs as a linear expression"
  (let* ((lin-exp (combine-linear-expressions
                    (list (second eq)
                          (scale-linear-expression eq -1))))
         (const (cdr (assoc '+constant+
                            lin-exp
                            :test 'eq)))
         (sum (delete '+constant+
                      lin-exp
                      :test 'eq
                      :key 'car)))
    (list '<= sum
              (if const
                (- const)
                0))))

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
         (bound-constraints (first parsed-constraints))
         (eq-constraints (second parsed-constraints))
         (var-list (union
                     (union (mapcar 'car objective-function)
                            (mapcar 'car bound-constraints))
                     (reduce 'union
                             eq-constraints
                             :key (compose 'car 'second)
                             :initial-value nil)))
         (variables (make-array 1 :initial-contents var-list
                                  :element-type 'symbol)))
    (make-instance 'linear-problem
                   :lp-type type
                   :variables variables
                   :objective-coefs (map 'vector (rcurry 'assoc objective-function :test 'eq)
                                                 variables))))


(defun make-linear-problem (objective &rest constraints)
  "Creates a linear problem from the expressions in the body"
  (parse-linear-problem objective constraints))
