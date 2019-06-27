
(uiop:define-package :linear-programming-test/solver
  (:use :cl
        :fiveam
        :linear-programming-test/base
        :linear-programming/problem
        :linear-programming/solver)
  (:export #:solver))

(in-package :linear-programming-test/solver)

(def-suite solver
  :in linear-programming
  :description "The suite to test linear-programming/solver")
(in-suite solver)


(test solve-problem
  (let* ((problem (make-linear-problem (max (+ x (* 4 y) (* 3 z)))
                                       (<= (+ (* 2 x) y) 8)
                                       (<= (+ y z) 7)))
         (solution (solve-problem problem)))
    (is (eq problem (solution-problem solution)))
    (is (= 57/2 (solution-objective-value solution)))
    (is (= 1/2 (solution-variable solution 'x)))
    (is (= 7 (solution-variable solution 'y)))
    (is (= 0 (solution-variable solution 'z)))
    (is (= 0 (solution-shadow-price solution 'x)))
    (is (= 0 (solution-shadow-price solution 'y)))
    (is (= 1/2 (solution-shadow-price solution 'z)))))


(test with-solved-problem
  (with-solved-problem ((max (= w (+ x (* 4 y) (* 3 z))))
                        (<= (+ (* 2 x) y) 8)
                        (<= (+ y z) 7))
    (is (= 57/2 w))
    (is (= 1/2 x))
    (is (= 0 (shadow-price x)))
    (is (= 7 y))
    (is (= 0 (shadow-price y)))
    (is (= 0 z))
    (is (= 1/2 (shadow-price z)))))
