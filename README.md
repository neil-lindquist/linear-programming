# Common Lisp Linear Programming
[![Build Status](https://travis-ci.org/neil-lindquist/linear-programming.svg?branch=master)](https://travis-ci.org/neil-lindquist/linear-programming)
[![Build status](https://ci.appveyor.com/api/projects/status/8rx1x28xxx7tkis4/branch/master?svg=true)](https://ci.appveyor.com/project/neil-lindquist/linear-programming/branch/master)
[![Coverage Status](https://coveralls.io/repos/github/neil-lindquist/linear-programming/badge.svg?branch=master)](https://coveralls.io/github/neil-lindquist/linear-programming?branch=master)

[![GitHub release](https://img.shields.io/github/release/neil-lindquist/linear-programming.svg)](https://github.com/neil-lindquist/linear-programming/releases)
[![Current documentation](https://img.shields.io/badge/docs-current-informational.svg)](https://neil-lindquist.github.io/linear-programming/)


This is a Common Lisp library for solving linear programming problems.

## Installation
As it is still in early development, the linear-programming library is not on Quicklisp.
Instead, place the contents of this repository into `~/quicklisp/local-projects/linear-programming`.
Then, you can load it using `(ql:quickload :linear-programming)`.
If you are not using quicklisp, place the repository somewhere where ASDF can find it, then load it with `(asdf:load-system :linear-programming)`.

## Usage
See [neil-lindquist.github.io/linear-programming/](https://neil-lindquist.github.io/linear-programming/) for further documentation.

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
Once the problem is created, it can be solved with the simplex method.
```common-lisp
(defvar solution (solve-problem problem))
```
Finally, the optimal tableau can be inspected to get the resulting objective function, decision variables, and shadow prices.
```common-lisp
(format t "Objective value solution: ~A~%" (solution-variable solution 'w))
(format t "x = ~A (shadow price: ~A)~%" (solution-variable solution 'x) (solution-shadow-price solution 'x))
(format t "y = ~A (shadow price: ~A)~%" (solution-variable solution 'y) (solution-shadow-price solution 'y))
(format t "z = ~A (shadow price: ~A)~%" (solution-variable solution 'z) (solution-shadow-price solution 'z))

;; ==>
;; Objective value solution: 57/2
;; x = 1/2 (shadow price: 0)
;; y = 7 (shadow price: 0)
;; z = 0 (shadow price: 1/2)
```
Alternatively, the `with-solved-problem` macro combines these steps and binds the solution variables.

```common-lisp
(with-solved-problem ((max (= w (+ x (* 4 y) (* 3 z))))
                      (<= (+ (* 2 x) y) 8)
                      (<= (+ y z) 7))
  (format t "Objective value solution: ~A~%" w)
  (format t "x = ~A (shadow price: ~A)~%" x (shadow-price solution 'x))
  (format t "y = ~A (shadow price: ~A)~%" y (shadow-price solution 'y))
  (format t "z = ~A (shadow price: ~A)~%" z (shadow-price solution 'z)))

;; ==>
;; Objective value solution: 57/2
;; x = 1/2 (shadow price: 0)
;; y = 7 (shadow price: 0)
;; z = 0 (shadow price: 1/2)
```
