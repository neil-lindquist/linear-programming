
(uiop:define-package :linear-programming/all
  (:nicknames :linear-programming)
  (:use-reexport :linear-programming/problem
                 :linear-programming/solver
                 :linear-programming/conditions)
  (:documentation "The overall package for the linear programming library.
                   It contains only the reexported symbols of
                   [LINEAR-PROGRAMMING/PROBLEM](#package-linear-programming/problem),
                   [LINEAR-PROGRAMMING/SOLVER](#package-linear-programming/solver), and
                   [LINEAR-PROGRAMMING/CONDITIONS](#package-linear-programming/conditions)."))
