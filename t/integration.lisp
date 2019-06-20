
(uiop:define-package :linear-programming-test/integration
  (:use :cl
        :fiveam
        :linear-programming-test/base
        :linear-programming)
  (:export #:integration))

(in-package :linear-programming-test/integration)

(def-suite integration
  :in linear-programming
  :description "The suite for integration tests")
(in-suite integration)

(test basic-problem
  ; An Assembly-balancing problem
  ; "A company has 4 departments. Each department uses a different method to
  ; produce components A and B from raw materials 1 and 2.
  ; Dept    input per run       output per run
  ; ___       _1_   _2_           _A_     _B_
  ;  1         8     6             7       5
  ;  2         5     9             6       9
  ;  3         3     8             8       4
  ; Finished widgets are assembled from 4 units of A and 3 units of B.  100
  ; units of material 1 and 200 units of material 2 are avalible.  Maximize the
  ; number of widgets produced."
  ;
  ; Solution: 0 runs of department 1
  ;           0 runs of department 2
  ;           25 runs of department 3
  ;           50 total widgets
  (let* ((problem (make-linear-problem (max (* 3 widgets))
                                       ; construction of widgets
                                       (<= (+ (* 4 widgets) (* -7 d1)
                                              (* -6 d2) (* -8 d3))
                                           0)
                                       (<= (+ (* 3 widgets) (* -5 d1)
                                              (* -9 d2) (* -4 d3))
                                           0)
                                       ; resource constraints
                                       (<= (+ (* 8 d1) (* 5 d2) (* 3 d3)) 100)
                                       (<= (+ (* 6 d1) (* 9 d2) (* 8 d3)) 200)))
         (tableau (solve-tableau (build-tableau problem))))
    (with-tableau-variables (widgets d1 d2 d3) tableau

      (is (<= 136.08 (tableau-objective-value tableau) 136.11)
          (format nil "Computed objective value of ~A, instead of 136.08-136.11"
                      (float (tableau-objective-value tableau))))
      (is (<= 45.36 widgets 45.37)
          (format nil "Computed ~A widgets, instead of 45.36-45.37" (float widgets)))
      (is (<= 2.37 d1 2.38)
          (format nil "Computed ~A dept 1 runs, instead of 2.37-2.38" (float d1)))
      (is (<= 6.96 d2 6.97)
          (format nil "Computed ~A dept 2 runs, instead of 6.96-6.97" (float d2)))
      (is (<= 15.37 d3 15.38)
          (format nil "Computed ~A dept 2 runs, instead of 15.37-15.38" (float d3))))))
