
(uiop:define-package :linear-programming-test/system-info
  (:use :cl
        :fiveam
        :linear-programming-test/base
        :linear-programming/system-info
        :iterate)
  (:export #:system-info))

(in-package :linear-programming-test/system-info)

(def-suite system-info
  :in linear-programming
  :description "The suite to test linear-programming/system-info")
(in-suite system-info)

(test +supported-floats+
  (is (<= 1 (length +supported-floats+) 4))
  (is (member (type-of 1.0s0) +supported-floats+))
  (is (member (type-of 1.0f0) +supported-floats+))
  (is (member (type-of 1.0d0) +supported-floats+))
  (is (member (type-of 1.0l0) +supported-floats+)))

(test optimization-type
  (is (eq (optimization-type 1) 'rational))
  (is (eq (optimization-type 99) 'rational))
  (is (eq (optimization-type (+ 6 most-positive-fixnum)) 'rational))
  (is (eq (optimization-type 4/5) 'rational))

  (is (eq (optimization-type 1.0s0) (type-of 1.0s0)))
  (is (eq (optimization-type 1.0f0) (type-of 1.0f0)))
  (is (eq (optimization-type 1.0d0) (type-of 1.0d0)))
  (is (eq (optimization-type 1.0l0) (type-of 1.0l0))))

(test float-contagion
  (iter (for types on (append +supported-floats+ '(rational)))
        (for t1 = (first types))
    (iter (for t2 in types)
      (is (float-contagion t1 t2) t1)
      (is (float-contagion t2 t1) t1))))
