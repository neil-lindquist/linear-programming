(uiop:define-package :linear-programming/system-info
  (:use :cl
        :iterate)
  (:import-from :alexandria
                #:define-constant)
  (:export #:+supported-floats+
           #:optimization-type
           #:float-contagion)
  (:documentation "Utilities for inspecting how certain implmenetation-dependant features behave."))
(in-package :linear-programming/system-info)

(define-constant +supported-floats+
  (let ((floats nil))
    ;; sbcl only supports single and double floats, so some branches are unused
    #+sbcl (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
    ;; types that don't have their own representation will get reported as a different type
    (when (eq (type-of 0l0) 'long-float)
      (push 'long-float floats))
    (when (eq (type-of 0d0) 'double-float)
       (push 'double-float floats))
    (when (eq (type-of 0f0) 'single-float)
      (push 'single-float floats))
    (when (eq (type-of 0s0) 'short-float)
      (push 'short-float floats))
    floats)
  :test 'equal ; lists have to be compared with equal
  :documentation "Contains the distinct floating point representations supported.")

(declaim (inline optimization-type))
(defun optimization-type (x)
  "Gets the type of `x` to optimize for. If `x` is a rational, returns `rational`.
Otherwise, returns the type of `x`."
  (if (typep x 'rational)
    'rational
    (type-of x)))

(declaim (inline float-contagion))
(defun float-contagion (t1 t2)
  "Computes the representation type using the rules for float contagion."
  (macrolet ((body ()
              `(locally
                 (declare (type (member rational ,@+supported-floats+) t1 t2))
                 (cond
                   ((eq t1 t2) t1) ; if the same, it doesn't matter
                   ((eq t1 'rational) t2) ; if one is rational, then the type of the other is used
                   ((eq t2 'rational) t1)
                   ,@(case (length +supported-floats+)
                       (1 ; if only 1 float type, then the previous cases are sufficient
                        nil)
                       (2 ; if 2 float types, at this point there is one of each type
                        `((t ',(second +supported-floats+))))
                       (3 ; if 3 types, need more checking
                        `(((eq t1 ',(first +supported-floats+)) t2)
                          ((eq t2 ',(first +supported-floats+)) t1)
                          (t ',(third +supported-floats+))))
                       (4 ; if 4 types, we don't need to pull from +supported-floats+
                        `(((eq t1 'short-float) t2)
                          ((eq t2 'short-float) t1)
                          ((eq t1 'single-float) t2)
                          ((eq t2 'single-float) t1)
                          (t 'long-float))))))))
    (body)))
