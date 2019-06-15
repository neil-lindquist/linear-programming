
(uiop:define-package :linear-programming-test/base
  (:use :cl
        :fiveam))

(in-package :linear-programming-test/base)

(def-suite* linear-programming
  :description "The base suite for the linear programming library tests")
