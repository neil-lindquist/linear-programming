
(uiop:define-package :linear-programming/all
  (:nicknames :linear-programming)
  (:use-reexport :linear-programming/problem
                 :linear-programming/solver
                 :linear-programming/conditions
                 :linear-programming/external-formats)
  (:documentation "The overall package for the linear programming library.
                   It contains only the reexported symbols of
                   [LINEAR-PROGRAMMING/PROBLEM](#package-linear-programming/problem),
                   [LINEAR-PROGRAMMING/SOLVER](#package-linear-programming/solver),
                   [LINEAR-PROGRAMMING/CONDITIONS](#package-linear-programming/conditions), and
                   [LINEAR-PROGRAMMING/EXTERNAL-FORMATS](#package-linear-programming/external-formats)."))
