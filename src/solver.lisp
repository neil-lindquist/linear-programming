
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
  (let ((tableau (solve-tableau (build-tableau problem))))
    (if (every (lambda (var)
                    (integerp (tableau-variable var tableau)))
               (integer-vars problem))
      (form-solution problem tableau)
      (branch-and-bound problem tableau))))


;;;;;;;;;; Branch and Bound ;;;;;;;;;;

(defun gen-entries (tableau entry)
  "Generates new entries to correct one of the integer constraints"
  (let* ((split-var (violated-integer-constraint tableau))
         (split-var-val (tableau-variable split-var tableau)))
    (list (list* `(<= ((,split-var . 1)) ,(floor split-var-val))
                 entry)
          (list* `(>= ((,split-var . 1)) ,(ceiling split-var-val))
                 entry))))

(defun branch-and-bound (problem first-tableau)
  "Applies the branch and bound method to the given problem and initial tableau
   and returns a solution object with the final result"
  (let ((current-best nil)
        (current-solution nil)
        (stack (gen-entries first-tableau '()))
        (comparator (if (eq (lp-type problem) 'max) '< '>)))
    (iter (while stack)
      (let* ((entry (pop stack))
             (tab (build-and-solve problem entry)))
           (cond
             ((eq tab :infeasible)) ;NO OP
             ((violated-integer-constraint tab)
              (setf stack (append (gen-entries tab entry) stack)))
             ((or (not current-best)
                  (funcall comparator current-best (tableau-objective-value tab)))
              (setf current-best (tableau-objective-value tab)
                    current-solution tab)))))
    (form-solution problem current-solution)))


(defun violated-integer-constraint (tableau)
  "Gets a variable that is required to be an integer but is not"
  (iter (for var in (integer-vars (tableau-problem tableau)))
    (unless (integerp (tableau-variable var tableau))
      (return var))))

(defun build-and-solve (problem extra-constraints)
  "Builds and solves a tableau with the extra constrains added to the problem"
  ;if problem becomes infeasible, just return :infeasible
  (handler-case
    (solve-tableau
      (build-tableau
        (make-instance 'linear-problem
                       :type (lp-type problem)
                       :variables (variables problem)
                       :objective-variable (objective-variable problem)
                       :objective (objective-function problem)
                       :signed (signed-vars problem)
                       :integer (integer-vars problem)
                       :constraints (append extra-constraints
                                            (constraints problem)))))
    (infeasible-problem-error () :infeasible)))

;;;;;;;;;; End Branch and Bound ;;;;;;;;;;

(defun form-solution (problem tableau)
  "Creates the solution object from a problem and a solved tableau"
  (let* ((num-vars (length (variables problem)))
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
