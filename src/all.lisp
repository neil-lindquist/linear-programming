
(uiop:define-package :linear-programming/all
  (:nicknames :linear-programming)
  (:use-reexport :linear-programming/problem
                 :linear-programming/solver
                 :linear-programming/conditions
                 :linear-programming/external-formats)
  (:import-from :linear-programming/simplex
                #:simplex-solver)
  (:export #:simplex-solver)
  (:documentation "The overall package for the linear programming library. It contains only the
reexported symbols of `linear-programming/problem`, `linear-programming/solver`,
`linear-programming/conditioner`, and `linear-programming/external-formats`,
plus `simplex-solver` from `linear-programming/simplex`."))
