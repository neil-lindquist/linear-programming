
(uiop:define-package :linear-programming/problem
  (:uses :cl
         :alexandria
         :iterate)
  (:export #:make-linear-problem
           #:+constant+))

(in-package :linear-programming/problem)

(defclass linear-problem ()
  ((type :reader type
         :initarg :type
         :type (member (:max :min)))
   (variables :reader variables
              :initarg :variables
              :type (vector symbol))
   (objective-coefs :reader objective-coefs
                    :initarg :objective-coefs
                    :type (vector real))))

(defun combine-linear-expressions (exprs)
  "Takes a list of linear expressions and reduces it into a single expression"
  (reduce (lambda (collected next)
            (if-let (pair (assoc (car next) collected))
              (progn
                (incf (cdr pair) (cdr next))
                collected)
              (cons next collected)))
          exprs
          :initial-value nil))

(defun parse-linear-expression (expr)
  "Parses the expression into a alist mapping variables to coefficients.
   Any constant values are mapped to +constant+"
  (cond
    ((symbolp expr)
     (list (cons expr 1)))
    ((numberp expr)
     (list (cons '+constant+ expr)))
    ((not (and (listp expr) (< 1 (length expr))))
     (error "~S is not a symbol, number, or an expression" expr))
    ((eq (first expr) '+)
     (combine-linear-expressions (mapcar parse-objective-function (rest expr))))
    ((eq (first expr) '*)
     (unless (= 3 (length expr))
       (error "Multiplication in linear expressions only supports two products"))
     (let ((prod1 (second expr))
           (prod2 (third expr)))
       (cond
         ((and (numberp prod1) (numberp prod2))
          (list (cons '+constant+ (* prod1 prod2))))
         ((and (numberp prod1) (symbolp prod2))
          (list (cons prod2 prod1)))
         ((and (symbolp prod1) (numberp prod2))
          (list (cons prod1 prod2)))
         (t (error "Cannot multiple ~A and ~A in a linear expression" prod1 prod2)))))))
    ;TODO implement subtraction
    ;TODO implememnt dividing coefficient

(defun parse-linear-constraint (expr)
  "Converts the constraint into a list of simple constraints"
  (let ((simplified (case (first expr)
                      ((<= >=)
                       (list (cons (first expr) (parse-linear-expression (second expr)))))
                      ((non-neg)
                       (mapcar (lambda (val) (cons (<= 0 val)))
                               (parse-linear-expression (second expr))))
                      (t (error "~A is not a valid contraint type" (first expr))))))
    (reduce 'append
            simplified
            :key #'(lambda (constraint)
                     (iter (for i from 2 below (length constraint))
                       (if (eq (first constraint) '<=)
                         (collect (list '<=
                                        (nth (1- i) constraint)
                                        (nth i constraint)))
                         (collect (list '<=
                                        (nth i constraint)
                                        (nth (1- i) constraint)))))))))
    ;TODO consider doing some checking for redundant/contradictory constaints


(defun parse-linear-problem (objective constraints)
  "Parses the expressions into a linear programming problem"
  (unless (member (first objective) '(min max) :test 'eq)
    (error "~A is not min or max in objective function ~A" (first objective) objective))
  (let* ((variable-list nil)
         (type (first objective))
         (objective-function (parse-linear-epression (second objective)))
         (simple-constraints (parse-linear-constraints constraints))
         (var-list (reduce 'union
                           simple-constraints
                           :key (lambda (c) (mapcar 'car
                                                    (cons (second c) (third c))))
                           :initial-value (mapcar 'car objective-function)))
         (variables (make-array 1 :initial-contents var-list
                                  :element-type 'symbol))))
  (make-instance 'linear-problem
                 :type type
                 :variables variables
                 :objective-coefs (map 'vector (rcurry 'assoc objective-function :test 'eq)
                                               variables)))


(defun make-linear-problem (objective &rest constrants)
  "Creates a linear problem from the expressions in the body"
  (parse-linear-problem objective constraints))
