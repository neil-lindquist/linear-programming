(uiop:define-package :linear-programming/conditions
  (:use :cl)
  (:export #:parsing-error
           #:solver-error
           #:infeasible-problem-error
           #:unbounded-problem-error))
(in-package :linear-programming/conditions)


(define-condition parsing-error (error)
  ((description :reader description
                :initarg :description))
  (:report (lambda (err stream) (format stream (description err))))
  (:documentation "Indicates an error occured while parsing a linear problem.
                   Includes a textual description of the issue."))


(define-condition solver-error (error)
  ()
  (:documentation "The base class for errors that occur with the solving algorithm."))

(define-condition unbounded-problem-error (solver-error)
  ()
  (:report (lambda (err stream)
             (declare (ignore err))
             (format stream "Problem is unbounded")))
  (:documentation "Indicates the feasible region is unbounded such that the
                   optimal objective value is infinite."))

(define-condition infeasible-problem-error (solver-error)
  ()
  (:report (lambda (err stream)
             (declare (ignore err))
             (format stream "Problem has no feasible region")))
  (:documentation "Indicates the there is no feasible region."))
