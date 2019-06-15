
(uiop:define-package :linear-programming-test/problem
  (:use :cl
        :fiveam
        :linear-programming-test/base
        :linear-programming-test/test-utils
        :linear-programming/problem)
  (:export #:problem))

(in-package :linear-programming-test/problem)

(def-suite problem
  :in linear-programming
  :description "The suite to test linear-programming/problem")
(in-suite problem)

(test parse-linear-problem
  (let ((problem (parse-linear-problem '(max (+ x (* 4 y) (* 8 z)))
                                       '((<= (+ x y) 8)
                                         (<= (+ y z) 7)
                                         (non-neg x y z)))))
    (is (typep problem 'linear-problem))
    (is (eq 'max (lp-type problem)))
    (is (set-equal '((x . 1) (y . 4) (z . 8))
                   (objective-function problem)))
    (is (set-equal '(x y z)
                   (non-neg-vars problem)))
    (is (set-equal '((<= ((x . 1) (y . 1)) 8) (<= ((y . 1) (z . 1)) 7))
                   (constraints problem)))))
