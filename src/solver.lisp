
(uiop:define-package :linear-programming/solver
  (:documentation "High level linear programming problem solving functions")
  (:use :cl
        :alexandria
        :iterate
        :linear-programming/conditions
        :linear-programming/problem
        :linear-programming/simplex)
  (:export #:with-solved-problem
           #:shadow-price
           #:solve-problem
           #:solution
           #:solution-problem
           #:solution-objective-value
           #:solution-variable
           #:solution-shadow-price))
(in-package :linear-programming/solver)

(defstruct solution
  (problem nil :read-only t :type linear-problem)
  (objective-value 0 :read-only t :type real)
  (variables #() :read-only t :type (simple-array real (*)))
  (shadow-prices #() :read-only t :type (simple-array real (*))))

(declaim (inline solution-variable))
(defun solution-variable (solution var)
  "Gets the value of the given variable in the solution"
  (aref (solution-variables solution)
        (position var (variables (solution-problem solution)))))'

(declaim (inline solution-shadow-price))
(defun solution-shadow-price (solution var)
  "Gets the shadow price of the given variable in the solution"
  (aref (solution-shadow-prices solution)
        (position var (variables (solution-problem solution)))))

(defun solve-problem (problem)
  "Solves the given linear problem"
  (let* ((tableau (solve-tableau (build-tableau problem)))
         (num-vars (length (variables problem)))
         (variables (make-array (list num-vars) :element-type 'real
                                                :initial-element 0))
         (shadow-prices (make-array (list num-vars) :element-type 'real
                                                    :initial-element 0)))
    (iter (for var in-vector (variables problem))
          (for i from 0)
      (setf (aref variables i) (tableau-variable var tableau))
      (setf (aref shadow-prices i) (tableau-shadow-price var tableau)))
    (make-solution :problem problem
                   :objective-value (tableau-objective-value tableau)
                   :variables variables
                   :shadow-prices shadow-prices)))


(defmacro with-solved-problem ((objective-func &rest constraints) &body body)
  "Takes the problem description, and evaluates `body` with the variables of
   the problem bound to their solution values.  Additionally, a macro
   `(shadow-price var)` is bound to get the shadow price of `var`."
  (let ((problem (parse-linear-problem objective-func constraints)))
    (with-gensyms (solution)
      `(let ((,solution (solve-problem ,problem)))
         (let ((,(objective-variable problem) (solution-objective-value ,solution))
               ,@(iter (for var in-vector (variables problem))
                       (for i from 0)
                   (collect `(,var (aref (solution-variables ,solution) ,i)))))
           (macrolet ((shadow-price (var)
                        `(solution-shadow-price ,',solution ',var)))
             ,@body))))))
