(uiop:define-package :linear-programming-test/test-utils
  (:use :cl)
  (:export #:set-equal
           #:simple-linear-constraint-set-equal))
(in-package :linear-programming-test/test-utils)

(defun set-equal (s1 s2 &key (test #'equal))
  "Helper method to test for set equality"
  (null (set-exclusive-or s1 s2 :test test)))


(defun simple-linear-constraint-set-equal (s1 s2)
  "Helper method to test that two sets of simplified linear constraints are equal"
  (set-equal s1 s2
             :test (lambda (c1 c2)
                     (and (= 3 (length c1) (length c2))
                          (eq (first c1) (first c2))
                          (set-equal (second c1) (second c2))
                          (= (third c1) (third c2))))))
