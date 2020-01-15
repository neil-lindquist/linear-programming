
(uiop:define-package :linear-programming/utils
  (:use :cl)
  (:import-from :alexandria
                #:once-only)
  (:import-from :linear-programming/conditions
                #:invalid-bounds-error)
  (:import-from :linear-programming/system-info
                #:optimization-type
                #:float-contagion)
  (:export #:lb-min
           #:lb-max
           #:ub-min
           #:ub-max
           #:validate-bounds

           #:fp=
           #:fp<=
           #:fp>=
           #:fp<
           #:fp>)
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



;;; Floating point (in)equality

(declaim (inline fp=))

(defun fp= (a b &optional (factor 16))
  "Tests for equality taking into account floating point error. `factor` is the
acceptable multiple of unit round off that the two values can differ by."

  (case (float-contagion (optimization-type a) (optimization-type b))
    (rational (= a b))
    (short-float  (<= (abs (- a b)) (* factor short-float-epsilon)))
    (single-float (<= (abs (- a b)) (* factor single-float-epsilon)))
    (double-float (<= (abs (- a b)) (* factor double-float-epsilon)))
    (long-float   (<= (abs (- a b)) (* factor long-float-epsilon)))))


(defmacro fp-inequality (name op eps-mod)
  (let ((neg-eps-mod (if (eq eps-mod '+) '- '+)))
    `(progn
      (declaim (inline ,name))
      (defun ,name (a b &optional (factor 16))
        "Tests for inequality taking into account floating point error. `factor` is the
acceptable multiple of unit round off that the two values can differ by."
        (case (float-contagion (optimization-type a) (optimization-type b))
          (rational (,op a b))
          (short-float  (,op a (,eps-mod b (* factor short-float-epsilon))))
          (single-float (,op a (,eps-mod b (* factor single-float-epsilon))))
          (double-float (,op a (,eps-mod b (* factor double-float-epsilon))))
          (long-float   (,op a (,eps-mod b (* factor long-float-epsilon))))))

      (define-compiler-macro ,name (&whole form a b &optional (factor 16))
        (if (typep a 'real)
          (once-only (b)
            `(case (float-contagion ',(optimization-type a) (optimization-type ,b))
               (rational (,',op ,a ,b))
               (short-float  (,',op (,',neg-eps-mod ,a (* ,factor short-float-epsilon))  ,b))
               (single-float (,',op (,',neg-eps-mod ,a (* ,factor single-float-epsilon)) ,b))
               (double-float (,',op (,',neg-eps-mod ,a (* ,factor double-float-epsilon)) ,b))
               (long-float   (,',op (,',neg-eps-mod ,a (* ,factor long-float-epsilon))   ,b))))
          form)))))

(fp-inequality fp<= <= +)
(fp-inequality fp>= >= -)
(fp-inequality fp<  <  -)
(fp-inequality fp>  >  +)
