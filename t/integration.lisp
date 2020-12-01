
(uiop:define-package :linear-programming-test/integration
  (:use :cl
        :fiveam
        :linear-programming-test/base
        :linear-programming)
  (:import-from :linear-programming/utils
                #:fp=)
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
  ; Additionally, widgets are sold at $3 per unit to increase the coverage of
  ; this test.
  (with-solved-problem ((= revenue (max (* 3 widgets)))
                        ; construction of widgets
                        (<= (+ (* 4 widgets) (* -7 d1)
                               (* -6 d2) (* -8 d3))
                            0)
                        (<= (+ (* 3 widgets) (* -5 d1)
                               (* -9 d2) (* -4 d3))
                            0)
                        ; resource constraints
                        (<= (+ (* 8 d1) (* 5 d2) (* 3 d3)) 100)
                        (<= (+ (* 6 d1) (* 9 d2) (* 8 d3)) 200))

    (is (<= 136.08 revenue 136.11)
        (format nil "Computed revenue of ~A, instead of 136.08-136.11"
                    (float revenue)))
    (is (<= 45.36 widgets 45.37)
        (format nil "Computed ~A widgets, instead of 45.36-45.37" (float widgets)))
    (is (= 0 (reduced-cost widgets)))
    (is (<= 2.37 d1 2.38)
        (format nil "Computed ~A dept 1 runs, instead of 2.37-2.38" (float d1)))
    (is (= 0 (reduced-cost d1)))
    (is (<= 6.96 d2 6.97)
        (format nil "Computed ~A dept 2 runs, instead of 6.96-6.97" (float d2)))
    (is (= 0 (reduced-cost d2)))
    (is (<= 15.37 d3 15.38)
        (format nil "Computed ~A dept 3 runs, instead of 15.37-15.38" (float d3)))
    (is (= 0 (reduced-cost d3)))))


(test excessive-constraints
  ; This problem caused some issues when trying to solve it.
  (with-solved-problem ((min a)
                        (<= 0 (+ 148 (* 49 a)) (* 255 a))
                        (<= 0 (+ 135 (* 49 a)) (* 255 a))
                        (<= 0 (+ 134 (* 49 a)) (* 255 a))
                        (<= 0 a 1))
    (is (= 74/103 a))
    (is (= 0 (reduced-cost a)))))


(test numerial-issue
  ;; This problem exposed a bug with floating point round off
  (with-solved-problem ((= z (min (+ b (* 0.6861807 a))))
                        (>=  (+ b (* 0.6861807 a)) 0.9372585)
                        (>=  (+ b (* 0.7776901 a)) 0.7461006)
                        (>=  (+ b (* 0.14247864 a)) 0.38555977))
    (is (fp= 0.9372585 z))
    ;; Note that there are multiple solutions
    (is (fp= z (+ b (* 0.6861807 a))))))

(test ilp-bugs
  (with-solved-problem
         ((min w)
          (INTEGER x T185 E T184 D T183 C T182 B T181 A T180 w)
          (bounds (1 x 1))
          (= (+ (* -1 x) (* 1 T185)) 0) (= (+ (* -1 E) (* 1 T184)) 0)
          (= (+ (* -1 D) (* 1 T183)) 0) (= (+ (* -1 C) (* 1 T182)) 0)
          (= (+ (* -1 B) (* 1 T181)) 0) (= (+ (* -1 A) (* 10 T180)) 0)
          (<= (+ (* -1 E) (* 1 T185)) 0) (<= (+ (* -1 D) (* 1 T184)) 0)
          (<= (+ (* -1 C) (* 1 T183)) 0) (<= (+ (* -1 B) (* 1 T182)) 0)
          (<= (+ (* -1 A) (* 7 T182) (* 7 T183) (* 7 T184) (* 7 T185)) 0)
          (<=
           (+ (* -1 w) (* 171 T1) (* 114 T3) (* 189 T10) (* 121 T15) (* 156 T18)
              (* 185 T52) (* 111 T54) (* 141 T63) (* 156 T72) (* 185 T106) (* 111 T108)
              (* 141 T117) (* 156 T126) (* 185 T160) (* 111 T162) (* 141 T171)
              (* 10 T180) (* 1 T181))
           0))
    (is (= 31 w)))

  (with-solved-problem ((min (+ x y z))
                        (integer x y z)
                        (>= (+ x y (* 9 z)) 30/16)
                        (>= (+ (* 3/2 x) (* 78/64 y) z) 32/11))
    (is (= 2 x))
    (is (= 0 y))
    (is (= 0 z))))
