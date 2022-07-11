
(uiop:define-package :linear-programming-test/solver
  (:use :cl
        :fiveam
        :linear-programming-test/base
        :linear-programming/problem
        :linear-programming/solver)
  (:import-from :linear-programming/conditions
                #:infeasible-problem-error)
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
    (is (= 0 (solution-reduced-cost solution 'x)))
    (is (= 0 (solution-reduced-cost solution 'y)))
    (is (= 1/2 (solution-reduced-cost solution 'z))))

  ; Integer problem

  (signals infeasible-problem-error (solve-problem
                                     (make-linear-problem (max (+ x y))
                                                          (<= y x)
                                                          (>= y (* 1.2 (+ x .9)))
                                                          (integer x y))))

  ; Rock of Gibralter problem
  (let* ((problem (make-linear-problem (max (+ (* 240 x) (* 120 y)))
                                       (<= (+ x y) 5)
                                       (<= (+ (* -1 x) y) 0)
                                       (<= (+ (* 6 x) (* 2 y)) 21)
                                       (integer x y)))
         (solution (solve-problem problem)))
    (is (eq problem (solution-problem solution)))
    (is (= 840 (solution-objective-value solution)))
    (is (= 3 (solution-variable solution 'x)))
    (is (= 1 (solution-variable solution 'y)))
    (is (= 0 (solution-reduced-cost solution 'x)))
    (is (= 0 (solution-reduced-cost solution 'y))))

  ; test min problem by making objective coefficients negative
  (let* ((problem (make-linear-problem (min (+ (* -240 x) (* -120 y)))
                                       (<= (+ x y) 5)
                                       (<= (+ (* -1 x) y) 0)
                                       (<= (+ (* 6 x) (* 2 y)) 21)
                                       (integer x y)))
         (solution (solve-problem problem)))
    (is (eq problem (solution-problem solution)))
    (is (= -840 (solution-objective-value solution)))
    (is (= 3 (solution-variable solution 'x)))
    (is (= 1 (solution-variable solution 'y)))
    (is (= 0 (solution-reduced-cost solution 'x)))
    (is (= 0 (solution-reduced-cost solution 'y)))

    ; test variable bounds
    (let* ((problem (make-linear-problem (max (+ x (* 4 y) (* 3 z)))
                                         (<= (+ (* 2 x) y) 8)
                                         (<= (+ y z) 7)
                                         (>= x 1)))
           (solution (solve-problem problem)))
      (is (eq problem (solution-problem solution)))
      (is (= 28 (solution-objective-value solution)))
      (is (= 1 (solution-variable solution 'x)))
      (is (= 6 (solution-variable solution 'y)))
      (is (= 1 (solution-variable solution 'z)))
      (is (= 1 (solution-reduced-cost solution 'x)))
      (is (= 0 (solution-reduced-cost solution 'y)))
      (is (= 0 (solution-reduced-cost solution 'z))))))

(test solution-variable
  (declare (notinline solution-variable))
  (declare (notinline solution-reduced-cost))

  (let* ((problem (make-linear-problem (max (= w (+ x (* 4 y) (* 3 z))))
                                       (<= (+ (* 2 x) y) 8)
                                       (<= (+ y z) 7)))
         (solution (solve-problem problem)))
    (is (= 57/2 (solution-variable solution 'w)))
    (is (= 1/2 (solution-variable solution 'x)))
    (is (= 7 (solution-variable solution 'y)))
    (is (= 0 (solution-variable solution 'z)))
    (signals error (solution-variable solution 'v))

    (signals error (solution-reduced-cost solution 'w))
    (is (= 0 (solution-reduced-cost solution 'x)))
    (is (= 0 (solution-reduced-cost solution 'y)))
    (is (= 1/2 (solution-reduced-cost solution 'z)))
    (signals error (solution-reduced-cost solution 'v))))

(test with-solved-problem
  (with-solved-problem ((max (= w (+ x (* 4 y) (* 3 z))))
                        (<= (+ (* 2 x) y) 8)
                        (<= (+ y z) 7))
    (is (= 57/2 w))
    (is (= 1/2 x))
    (is (= 0 (reduced-cost x)))
    (is (= 7 y))
    (is (= 0 (reduced-cost y)))
    (is (= 0 z))
    (is (= 1/2 (reduced-cost z)))))

(test with-solution-variables
  (let* ((problem (make-linear-problem (max (= w (+ x (* 4 y) (* 3 z))))
                                       (<= (+ (* 2 x) y) 8)
                                       (<= (+ y z) 7)))
         (solution (solve-problem problem)))
    (with-solution-variables (w x z) solution
      (is (= 57/2 w))
      (is (= 1/2 x))
      (is (= 0 (reduced-cost x)))
      (is (= 0 z))
      (is (= 1/2 (reduced-cost z))))))
