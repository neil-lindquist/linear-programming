
(defsystem "linear-programming"
  :description "A library for solving linear programming problems"
  :version "0.1.0"
  :author "Neil Lindquist <NeilLindquist5@gmail.com>"
  :licence "MIT"
  :class :package-inferred-system
  :pathname "src"
  :depends-on ((:version "asdf" "3.1.6")
               "linear-programming/all")
  :in-order-to ((test-op (test-op "linear-programming-test"))))
