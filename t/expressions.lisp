
(uiop:define-package :linear-programming-test/expressions
  (:use :cl
        :fiveam
        :linear-programming-test/base
        :linear-programming-test/test-utils
        :linear-programming/conditions
        :linear-programming/expressions)
  (:export #:expressions))

(in-package :linear-programming-test/expressions)

(def-suite expressions
  :in linear-programming
  :description "The suite to test linear-programming/expressions")
(in-suite expressions)

(test linear-constant-p
  (declare (notinline linear-programming/expressions::linear-constant-p))
  (is-true (linear-programming/expressions::linear-constant-p
            (parse-linear-expression 5)))
  (is-true (linear-programming/expressions::linear-constant-p
            (parse-linear-expression 2/3)))
  (is-true (linear-programming/expressions::linear-constant-p
            (parse-linear-expression -1.0)))

  (is-false (linear-programming/expressions::linear-constant-p
             (parse-linear-expression 'x)))
  (is-false (linear-programming/expressions::linear-constant-p
             (parse-linear-expression '(+ x 5)))))

(test scale-linear-expression
  (declare (notinline scale-linear-expression))
  (is (equal '((a . 24) (b . 7/2) (c . 4.5))
             (scale-linear-expression '((a . 8) (b . 7/6) (c . 1.5))
                                      3)))
  (is (equal nil
             (scale-linear-expression nil 3)))
  (is (equal '((a . 4) (b . 7/12) (c . .75))
             (scale-linear-expression '((a . 8) (b . 7/6) (c . 1.5))
                                      1/2))))

(test sum-linear-expressions
  (declare (notinline sum-linear-expressions))
  (is (eq nil (sum-linear-expressions)))
  (is (eq nil (sum-linear-expressions nil)))
  (is (eq nil (sum-linear-expressions nil nil)))
  (is (eq nil (sum-linear-expressions nil nil nil)))

  (is (set-equal '((a . 8) (b . 7/6) (c . 1.5))
                 (sum-linear-expressions '((a . 8) (b . 7/6) (c . 1.5)))))
  (is (set-equal '((a . 12) (b . 19/6) (c . 2.0) (d . 6) (e . 7/4))
                 (sum-linear-expressions '((a . 8) (b . 7/6) (c . 1.5) (d . 6))
                                         '((a . 4) (b . 2) (c . 1/2) (e . 7/4))))))

(test parse-linear-expression
  (is (set-equal '((a . 1) (+constant+ . 5) (b . 8) (c . 1))
                 (parse-linear-expression '(+ a 5 (* 8 b) (+ c)))))
  (is (set-equal '((+constant+ . 6) (x . 2))
                 (parse-linear-expression '(+ (* 2 3) (* x 2)))))
  (is (set-equal '((x . -1) (y . 1) (z . -3))
                 (parse-linear-expression '(+ (- x) (- y (* 3 z))))))
  (is (set-equal '((+constant+ . 116) (x . 14))
                 (parse-linear-expression '(+ (* 3 4 5) (* 1 (+ 3 4) 2 (+ x 4))))))
  (is (set-equal '((+constant+ . 1/2))
                 (parse-linear-expression '(/ 2))))
  (is (set-equal '((x . 1/2) (+constant+ . 3/2))
                 (parse-linear-expression '(/ (+ 3 x) 2))))
  (is (set-equal '((a . 1) (+constant+ . 5) (b . 8) (c . 1))
                 (parse-linear-expression '(:alist (a . 1) (+constant+ . 5) (b . 8) (c . 1)))))
  (is (set-equal '((a . 1) (+constant+ . 5) (b . 8) (c . 1))
                 (parse-linear-expression '(:plist a 1 +constant+ 5 b 8 c 1))))
  (is (set-equal '((+constant+ . 5) (x . 6) (y . 1))
                 (parse-linear-expression '(+ (:plist +constant+ 5 x 4) (:alist (x . 2) (y . 1))))))
  (signals nonlinear-error (parse-linear-expression '(* x y)))
  (signals nonlinear-error (parse-linear-expression '(/ x)))
  (signals nonlinear-error (parse-linear-expression '(/ x y)))
  (signals parsing-error (parse-linear-expression '"x + y"))
  (signals nonlinear-error (parse-linear-expression '(log 3))))


(test format-linear-expression
  (is (equal '(+ (* 5 x) (* 2 y) 5)
             (format-linear-expression '((x . 5) (y . 2) (+constant+ . 5))))))
