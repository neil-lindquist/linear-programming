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
  (:report (lambda (err stream) (format stream (description err)))))


(define-condition solver-error (error)
  ())

(define-condition unbounded-problem-error (solver-error)
  ()
  (:report (lambda (err stream)
             (declare (ignore err))
             (format stream "Problem is unbounded"))))

(define-condition infeasible-problem-error (solver-error)
  ()
  (:report (lambda (err stream)
             (declare (ignore err))
             (format stream "Problem has no feasible region"))))
