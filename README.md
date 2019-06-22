# Common Lisp Linear Programming

This is a Common Lisp library for solving linear programming problems.

## Installation
As it is still in early development, the linear-programming library is not on Quicklisp.
Instead, place the contents of this repository into `~/quicklisp/local-projects/linear-programming` (if you are not using quicklisp, place the `linear-programming` directory somewhere that ASDF can find it).
Then, you can load it using `(ql:quickload :linear-programming)`.

## Usage
Consider the following linear programming problem.
> maximize  x + 4y + 3z  
> such that  
> * 2x + y <= 8  
> * y + z <= 7

First, the problem needs to be specified.
```common-lisp
(use-package :linear-programming)

(defvar problem (make-linear-problem (max (= w (+ x (* 4 y) (* 3 z))))
                                          (<= (+ (* 2 x) y) 8)
                                          (<= (+ y z) 7)))
```
Then, once the problem is created, a simplex tableau can be build and solved.
```common-lisp
(defvar tableau (solve-tableau (build-tableau problem)))
```
Finally, the optimal tableau can be inspected to get the resulting objective function, decision variables, and shadow prices.
```common-lisp
(with-tableau-variables (w x y z) tableau
  (format t "Objective value solution: ~A~%" w)
  (format t "x = ~A (shadow price: ~A)~%" x (get-shadow-prices 'x tableau))
  (format t "y = ~A (shadow price: ~A)~%" y (get-shadow-prices 'y tableau))
  (format t "z = ~A (shadow price: ~A)~%" z (get-shadow-prices 'z tableau))))

;; ==>
;; Objective value solution: 57/2
;; x = 1/2 (shadow price: 0)
;; y = 7 (shadow price: 0)
;; z = 0 (shadow price: 1/2)
```
In addition to the `with-tableau-variables` macro, variable values can be obtained using the `get-tableau-variable` function.
