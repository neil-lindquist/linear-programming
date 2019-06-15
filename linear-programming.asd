
(defsystem "linear-programming"
  :description "A library for solving linear programming problems"
  :version "0.0.1"
  :author "Neil Lindquist <NeilLindquist5@gmail.com>"
  :licence "MIT"
  :class :package-inferred-system
  :pathname "src"
  :depends-on ("linear-programming/all")
  :in-order-to ((test-op (test-op "linear-programming-test"))))
