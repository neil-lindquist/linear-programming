
(uiop:define-package :linear-programming-test/utils
  (:use :cl
        :fiveam
        :linear-programming-test/base
        :linear-programming/utils)
  (:import-from :linear-programming/conditions
                #:invalid-bounds-error)
  (:export #:utils))

(in-package :linear-programming-test/utils)

(def-suite utils
  :in linear-programming
  :description "The suite to test linear-programming/utils")
(in-suite utils)


;;; Boundary management
(test lb-min
  (declare (notinline lb-min))
  (is (null (lb-min nil nil)))
  (is (null (lb-min nil -5)))
  (is (null (lb-min nil 6.78)))
  (is (null (lb-min -5 nil)))
  (is (null (lb-min 6.78 nil)))
  (is (eql -4 (lb-min -4 3)))
  (is (eql 6 (lb-min 6 80.0))))

(test lb-max
  (declare (notinline lb-max))
  (is (null (lb-max nil nil)))
  (is (eql -5 (lb-max nil -5)))
  (is (eql 6.78 (lb-max nil 6.78)))
  (is (eql -5 (lb-max -5 nil)))
  (is (eql 6.78 (lb-max 6.78 nil)))
  (is (eql 3 (lb-max -4 3)))
  (is (eql 80.0 (lb-max 6 80.0))))

(test ub-min
  (declare (notinline ub-min))
  (is (null (ub-min nil nil)))
  (is (eql -5 (ub-min nil -5)))
  (is (eql 6.78 (ub-min nil 6.78)))
  (is (eql -5 (ub-min -5 nil)))
  (is (eql 6.78 (ub-min 6.78 nil)))
  (is (eql -4 (ub-min -4 3)))
  (is (eql 6 (ub-min 6 80.0))))

(test ub-max
  (declare (notinline ub-max))
  (is (null (ub-max nil nil)))
  (is (null (ub-max nil -5)))
  (is (null (ub-max nil 6.78)))
  (is (null (ub-max -5 nil)))
  (is (null (ub-max 6.78 nil)))
  (is (eql 3 (ub-max -4 3)))
  (is (eql 80.0 (ub-max 6 80.0))))


(test validate-bounds
  (declare (notinline validate-bounds))
  (signals invalid-bounds-error (validate-bounds 5 -4 'x))
  (validate-bounds nil -4 'x)
  (fiveam:pass)
  (validate-bounds nil nil 'x)
  (fiveam:pass)
  (validate-bounds 5 nil 'x)
  (fiveam:pass)
  (validate-bounds 5 6 'x)
  (fiveam:pass))


(test fp=
  (declare (notinline fp=))
  (is (fp= 0 0))
  (is (fp= 8 8))
  (is (not (fp= 0 1)))
  (is (not (fp= 0 (expt 2 -128))))

  (is (fp= 0.0s0 0.0s0))
  (is (fp= 0.0s0 (* 4 short-float-epsilon)))
  (is (not (fp= 0.0s0 (* 4 short-float-epsilon) 1)))
  (is (not (fp= (/ 1s0 100) 0.0s0)))

  (is (fp= 0.0f0 0.0f0))
  (is (fp= 0.0f0 (* 4 single-float-epsilon)))
  (is (not (fp= 0.0f0 (* 4 single-float-epsilon) 1)))
  (is (not (fp= (/ 1f0 100) 0.0f0)))

  (is (fp= 0.0d0 0.0d0))
  (is (fp= 0.0d0 (* 4 double-float-epsilon)))
  (is (not (fp= 0.0d0 (* 4 double-float-epsilon) 1)))
  (is (not (fp= (/ 1d0 100) 0.0d0)))

  (is (fp= 0.0l0 0.0l0))
  (is (fp= 0.0l0 (* 4 long-float-epsilon)))
  (is (not (fp= 0.0l0 (* 4 long-float-epsilon) 1)))
  (is (not (fp= (/ 1l0 100) 0.0l0))))


(test fp>
  (declare (notinline fp>))
  (is (fp> 0 -1))
  (is (fp> 8 0))
  (is (fp> (expt 2 -128) 0))
  (is (not (fp> 0 0)))
  (is (not (fp>= 0 (expt 2 -128))))

  (is (fp> 0.0s0 -1.0s0))
  (is (not (fp> 0.0s0 0.0s0)))
  (is (not (fp> (* 4 short-float-epsilon) 0.0s0)))
  (is (fp> (* 4 short-float-epsilon) 0.0s0 1))

  (is (fp> 0.0f0 -1.0f0))
  (is (not (fp> 0.0f0 0.0f0)))
  (is (not (fp> (* 4 single-float-epsilon) 0.0f0)))
  (is (fp> (* 4 single-float-epsilon) 0.0f0 1))

  (is (fp> 0.0d0 -1.0d0))
  (is (not (fp> 0.0d0 0.0d0)))
  (is (not (fp> (* 4 double-float-epsilon) 0.0d0)))
  (is (fp> (* 4 double-float-epsilon) 0.0d0 1))

  (is (fp> 0.0l0 -1.0l0))
  (is (not (fp> 0.0l0 0.0l0)))
  (is (not (fp> (* 4 long-float-epsilon) 0.0l0)))
  (is (fp> (* 4 long-float-epsilon) 0.0l0 1)))


(test fp<
  (declare (notinline fp<))
  (is (fp< -1 0))
  (is (fp< 0 8))
  (is (fp< 0 (expt 2 -128)))
  (is (not (fp< 0 0)))
  (is (not (fp< (expt 2 -128) 0)))

  (is (fp< -1.0s0 0.0s0))
  (is (not (fp< 0.0s0 0.0s0)))
  (is (not (fp< 0.0s0 (* 4 short-float-epsilon))))
  (is (fp< 0.0s0 (* 4 short-float-epsilon) 1))

  (is (fp< -1.0f0 0.0f0))
  (is (not (fp< 0.0f0 0.0f0)))
  (is (not (fp< 0.0f0 (* 4 single-float-epsilon))))
  (is (fp< 0.0f0 (* 4 single-float-epsilon) 1))

  (is (fp< -1.0d0 0.0d0))
  (is (not (fp< 0.0d0 0.0d0)))
  (is (not (fp< 0.0d0 (* 4 double-float-epsilon))))
  (is (fp< 0.0d0 (* 4 double-float-epsilon) 1))

  (is (fp< -1.0l0 0.0l0))
  (is (not (fp< 0.0l0 0.0l0)))
  (is (not (fp< 0.0l0 (* 4 long-float-epsilon))))
  (is (fp< 0.0l0 (* 4 long-float-epsilon) 1)))
