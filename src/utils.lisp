
(uiop:define-package :linear-programming/utils
  (:use :cl)
  (:import-from :linear-programming/conditions
                #:invalid-bounds-error)
  (:export #:lb-min
           #:lb-max
           #:ub-min
           #:ub-max
           #:validate-bounds)
  (:documentation "Various internal utilities"))

(in-package :linear-programming/utils)



;;; Boundary management
;; boundary values are represented as a number, or nil for +/- infinity

(deftype lb () '(or null real))
(deftype ub () '(or null real))

(declaim (inline lb-min lb-max ub-min ub-max))

(defun lb-min (x y)
  "Computes the minimum value where nil is negative infinity"
  (declare (type lb x y))
  (cond
    ((null x) x)
    ((null y)y)
    (t (min x y))))

(defun lb-max (x y)
  "Computes the maximum value where nil is negative infinity"
  (declare (type lb x y))
  (cond
    ((null x) y)
    ((null y) x)
    (t (max x y))))

(defun ub-min (x y)
  "Computes the minimum value where nil is positive infinity"
  (declare (type (or null real) x y))
  (cond
    ((null x) y)
    ((null y) x)
    (t (min x y))))

(defun ub-max (x y)
  "Computes the maximum value where nil is positive infinity"
  (declare (type lb x y))
  (cond
    ((null x) x)
    ((null y) y)
    (t (max x y))))

(declaim (inline validate-bounds))
(defun validate-bounds (lb ub var)
  "Checks that the bounds represent a non empty range"
  (declare (type lb lb) (type ub ub))
  (when (and lb ub (< ub lb))
     (error 'invalid-bounds-error
            :var var
            :ub ub
            :lb lb)))
