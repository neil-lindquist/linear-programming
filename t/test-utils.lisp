(uiop:define-package :linear-programming-test/test-utils
  (:use :cl)
  (:export #:set-equal))
(in-package :linear-programming-test/test-utils)

(defun set-equal (s1 s2 &key (test #'equal))
  "Helper method to test for set equality"
  (null (set-exclusive-or s1 s2 :test test)))
