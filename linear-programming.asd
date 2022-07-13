
(defsystem "linear-programming"
  :description "A library for solving linear programming problems"
  :version "2.3.0"
  :author "Neil Lindquist <NeilLindquist5@gmail.com>"
  :licence "MIT"
  :homepage "https://neil-lindquist.github.io/linear-programming/"
  :bug-tracker "https://github.com/neil-lindquist/linear-programming/issues"
  :mailto "NeilLindquist5@gmail.com"
  :source-control (:git "https://github.com/neil-lindquist/linear-programming.git")

  :class :package-inferred-system
  :pathname "src"
  :depends-on ((:version "asdf" "3.1.6")
               "linear-programming/all")
  :in-order-to ((test-op (test-op "linear-programming-test"))))
