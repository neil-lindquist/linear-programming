
(uiop:define-package :linear-programming-test/simplex
  (:use :cl
        :fiveam
        :linear-programming-test/base
        :linear-programming/problem
        :linear-programming/simplex)
  (:export #:simplex))

(in-package :linear-programming-test/simplex)

(def-suite simplex
  :in linear-programming
  :description "The suite to test linear-programming/simplex")
(in-suite simplex)

(test build-tableau
  (let* ((problem (make-linear-problem (max (+ x (* 4 y) (* 3 z)))
                                       (<= (+ (* 2 x) y) 8)
                                       (<= (+ y z) 7)
                                       (non-neg x y z)))
         (tableau (build-tableau problem)))
    (is-true (tableau-p tableau))
    (is (eq problem (tableau-problem tableau)))
    (is (equal 5 (tableau-var-count tableau)))
    (is (equal 2 (tableau-constraint-count tableau)))
    (is (equalp #2A((2 0 -1) (1 1 -4) (0 1 -3) (1 0 0) (0 1 0) (8 7 0)) (tableau-matrix tableau)))
    (is (equalp #(3 4) (tableau-basis-columns tableau)))))


(test solve-tableau
  (let* ((problem (make-linear-problem (max (+ x (* 4 y) (* 3 z)))
                                       (<= (+ (* 2 x) y) 8)
                                       (<= (+ y z) 7)
                                       (non-neg x y z)))
         (tableau (build-tableau problem)))
    (is (eq tableau (solve-tableau tableau)))
    (is (eq problem (tableau-problem tableau)))
    (is (equal 5 (tableau-var-count tableau)))
    (is (equal 2 (tableau-constraint-count tableau)))
    (is (equalp #2A((1 0 0) (0 1 0) (-1/2 1 1/2) (1/2 0 1/2) (-1/2 1 7/2) (1/2 7 57/2)) (tableau-matrix tableau)))
    (is (equalp #(0 1) (tableau-basis-columns tableau)))))
