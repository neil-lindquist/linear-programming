
(uiop:define-package :linear-programming/solver
  (:use :cl
        :iterate)
  (:import-from :alexandria
                #:once-only
                #:with-gensyms)
  (:import-from :linear-programming/problem
                #:problem
                #:problem-vars
                #:problem-objective-var
                #:parse-linear-problem)
  (:import-from :linear-programming/simplex
                #:simplex-solver
                #:tableau
                #:tableau-problem
                #:tableau-objective-value
                #:tableau-variable
                #:tableau-shadow-price)
  (:export #:*solver*
           #:solve-problem

           #:solution-problem
           #:solution-objective-value
           #:solution-variable
           #:solution-shadow-price

           #:with-solved-problem
           #:with-solution-variables
           #:shadow-price)
  (:documentation "The high level linear programming solver interface. This interface is able to
wrap multiple backends. The backend can be adjusted by setting the `*solver*`
variable. The default backend is the `simplex-solver` in the
`linear-programming/simplex` package."))

(in-package :linear-programming/solver)


(defvar *solver* 'simplex-solver
  "The function that should be used by solve-problem. The function should take a
problem, and any backend specific keyword arguments and returns some form of
solution object. The solution object should support the following methods
`solution-problem`, `solution-objective-value`, `solution-variable`, and
`solution-shadow-price`.")

;;; Solution object

(defun solve-problem (problem &rest args &key &allow-other-keys)
  "Solves the given problem using the function stored by `*solver*`. Any keyword
arguments are passed to the solver function."
  (apply *solver* problem args))

;; Note that the simplex implementations are here in order to avoid a circlar dependency
(defgeneric solution-problem (solution)
  (:documentation "Gets the original problem for the solution")
  (:method ((solution tableau))
    (tableau-problem solution)))

(defgeneric solution-objective-value (solution)
  (:documentation "Gets the value of the objective function")
  (:method ((solution tableau))
    (tableau-objective-value solution)))

(defgeneric solution-variable (solution variable)
  (:documentation "Gets the value of the specified variable")
  (:method ((solution tableau) variable)
    (tableau-variable solution variable)))

(defgeneric solution-shadow-price (solution variable)
  (:documentation "Gets the shadow price of the specified variable")
  (:method ((solution tableau) variable)
    (tableau-shadow-price solution variable)))


;;; with-* methods

(defmacro with-solved-problem ((objective-func &rest constraints) &body body)
  "Takes the problem description, and evaluates `body` with the variables of the
problem bound to their solution values. Additionally, the macro `shadow-price`
is locally bound that takes a variable name and provides it's shadow price."
  (let ((problem (parse-linear-problem objective-func constraints)))
    (with-gensyms (solution)
      `(let ((,solution (solve-problem ,problem)))
         (with-solution-variables ,problem ,solution
           ,@body)))))

(defmacro with-solution-variables (var-list solution &body body)
  "Evaluates the body with the variables in `var-list` bound to their values in the
solution."
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
                     (collect `(,var (solution-variable ,solution ',var)))))
             (declare (ignorable ,(problem-objective-var problem) ,@(map 'list #'identity vars)))
             ,@body))
        `(let (,@(iter (for var in-sequence var-list)
                   (collect `(,var (solution-variable ,solution ',var)))))
           ,@body)))))
