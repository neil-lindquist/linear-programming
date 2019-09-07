
(uiop:define-package :linear-programming-test/system-info
  (:use :cl
        :fiveam
        :iterate
        :linear-programming-test/base
        :linear-programming/system-info)
  (:export #:system-info))

(in-package :linear-programming-test/system-info)

(def-suite system-info
  :in linear-programming
  :description "The suite to test linear-programming/system-info")
(in-suite system-info)


(test +supported-floats+
  (is-true (<= 1 (length +supported-floats+) 4) (format nil "There are somehow ~S float formats" (length +supported-floats+)))
  (is-true (member (type-of 0l0) +supported-floats+))
  (is-true (member (type-of 0d0) +supported-floats+))
  (is-true (member (type-of 0f0) +supported-floats+))
  (is-true (member (type-of 0s0) +supported-floats+)))


(test +float-array-specializable+
  (is-true (member +float-array-specializable+ '(nil t)))
  (when +float-array-specializable+
    (iter (for type in +supported-floats+)
      (is (typep (make-array '(2 2) :element-type type) `(simple-array ,type (2 2)))))))


(test float-contagion
  (iter (for types on (list* 'rational +supported-floats+))
        (for type1 = (first types))
    (is (eq type1 (float-contagion type1 type1)))
    (iter (for type2 in (rest types))
      (is (eq type2 (float-contagion type1 type2)))
      (is (eq type2 (float-contagion type2 type1))))))
