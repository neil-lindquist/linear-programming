
(uiop:define-package :linear-programming/solver
  (:use :cl
        :alexandria
        :iterate
        :linear-programming/conditions
        :linear-programming/problem
        :linear-programming/simplex)
  (:export #:solve-problem

           #:solution
           #:solution-problem
           #:solution-objective-value
           #:solution-variable
           #:solution-shadow-price

           #:with-solved-problem
           #:with-solution-variables
           #:shadow-price)
  (:documentation "The high level linear programming solver interface.  This
                   package abstracts away some of the complexities of the
                   simplex method, including integer constraints.  See
                   [LINEAR-PROGRAMMING/SIMPLEX](#package-linear-programming/simplex)
                   for lower level control of the solver."))

(in-package :linear-programming/solver)

(defstruct solution
  "Represents a solution to a linear programming problem."
  (problem nil :read-only t :type problem)
  (objective-value 0 :read-only t :type real)
  (variables #() :read-only t :type (simple-array real (*)))
  (shadow-prices #() :read-only t :type (simple-array real (*))))

(setf (documentation 'solution-problem 'function) "The problem that resulted in this solution."
      (documentation 'solution-objective-value 'function) "The value of the objective function.")


(declaim (inline solution-variable))
(defun solution-variable (solution var)
  "Gets the value of the given variable in the solution"
  (let ((problem (solution-problem solution)))
    (if (eq var (problem-objective-var problem))
      (solution-objective-value solution)
      (let ((i (position var (problem-vars (solution-problem solution)))))
        (if i
          (aref (solution-variables solution) i)
          (error "~S is not a variable in the problem" var))))))


(declaim (inline solution-shadow-price))
(defun solution-shadow-price (solution var)
  "Gets the shadow price of the given variable in the solution"
  (let ((i (position var (problem-vars (solution-problem solution)))))
    (if i
     (aref (solution-shadow-prices solution) i)
     (error "~S is not a variable in the problem" var))))


(defun solve-problem (problem)
  "Solves the given linear problem"
  (let ((tableau (solve-tableau (build-tableau problem))))
    (if (every (lambda (var)
                    (integerp (tableau-variable tableau var)))
               (problem-integer-vars problem))
      (form-solution problem tableau)
      (branch-and-bound problem tableau))))


;;;;;;;;;; Branch and Bound ;;;;;;;;;;

(defun gen-entries (tableau entry)
  "Generates new entries to correct one of the integer constraints"
  (let* ((split-var (violated-integer-constraint tableau))
         (split-var-val (tableau-variable tableau split-var)))
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
        (comparator (if (eq (problem-type problem) 'max) '< '>)))
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
  (iter (for var in (problem-integer-vars (tableau-problem tableau)))
    (unless (integerp (tableau-variable tableau var))
      (return var))))

(defun build-and-solve (problem extra-constraints)
  "Builds and solves a tableau with the extra constrains added to the problem"
  ;if problem becomes infeasible, just return :infeasible
  (handler-case
    (solve-tableau
      (build-tableau
        (linear-programming/problem::make-problem
                      :type (problem-type problem)
                      :vars (problem-vars problem)
                      :objective-var (problem-objective-var problem)
                      :objective-func (problem-objective-func problem)
                      :integer-vars (problem-integer-vars problem)
                      :constraints (append extra-constraints
                                           (problem-constraints problem)))))
    (infeasible-problem-error () :infeasible)))

;;;;;;;;;; End Branch and Bound ;;;;;;;;;;

(defun form-solution (problem tableau)
  "Creates the solution object from a problem and a solved tableau"
  (let* ((num-vars (length (problem-vars problem)))
         (variables (make-array (list num-vars) :element-type 'real
                                                :initial-element 0))
         (shadow-prices (make-array (list num-vars) :element-type 'real
                                                    :initial-element 0)))
    (iter (for var in-vector (problem-vars problem))
          (for i from 0)
      (setf (aref variables i) (tableau-variable tableau var))
      (setf (aref shadow-prices i) (tableau-shadow-price tableau var)))
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
         (with-solution-variables ,problem ,solution
           ,@body)))))

(defmacro with-solution-variables (var-list solution &body body)
  "Evaluates the body with the variables in `var-list` bound to their values in
   the solution.  If a linear problem is instead passed as `var-list`, all
   of the problem's variables are bound."
  (once-only (solution)
    (let ((body (list `(macrolet ((shadow-price (var)
                                    `(solution-shadow-price ,',solution ',var)))
                          ,@body))))
      (if (typep var-list 'problem)
        (let* ((problem var-list) ;alias for readability
               (vars (problem-vars problem)))
          `(let ((,(problem-objective-var problem) (solution-objective-value ,solution))
                 ,@(iter (for var in-vector vars)
                         (for i from 0)
                     (collect `(,var (aref (solution-variables ,solution) ,i)))))
             (declare (ignorable ,(problem-objective-var problem) ,@(map 'list #'identity vars)))
             ,@body))
        `(let (,@(iter (for var in-sequence var-list)
                   (collect `(,var (solution-variable ,solution ',var)))))
           ,@body)))))
