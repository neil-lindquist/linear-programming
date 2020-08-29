# Common Lisp Linear Programming
[![Travis Build Status](https://img.shields.io/travis/neil-lindquist/linear-programming/master?logo=travis)](https://travis-ci.org/neil-lindquist/linear-programming?branch=master)
[![Appveyor Build status](https://ci.appveyor.com/api/projects/status/8rx1x28xxx7tkis4/branch/master?svg=true)](https://ci.appveyor.com/project/neil-lindquist/linear-programming/branch/master)
[![Coverage Status](https://coveralls.io/repos/github/neil-lindquist/linear-programming/badge.svg?branch=master)](https://coveralls.io/github/neil-lindquist/linear-programming?branch=master)

![MIT License](https://img.shields.io/github/license/neil-lindquist/linear-programming.svg?color=informational)
[![GitHub release](https://img.shields.io/github/release/neil-lindquist/linear-programming.svg)](https://github.com/neil-lindquist/linear-programming/releases)
[![Current documentation](https://img.shields.io/badge/docs-current-informational.svg)](https://neil-lindquist.github.io/linear-programming/)



This is a Common Lisp library for solving linear programming problems.
It's designed to provide a high-level and ergonomic API for specifying linear programming problems as lisp expressions.

The core library is implemented purely in Common Lisp with only a few community-standard libraries as dependencies (ASDF, Alexandria, Iterate).
However, the solver is designed to support alternative backends without any change to the user's code.
Currently, there is a [backend for the GNU Linear Programming Kit (GLPK)](https://github.com/neil-lindquist/linear-programming-glpk).

## Installation
The linear-programming library is avalible in both the main Quicklisp distribution and Ultralisp, so it can loaded with with `(ql:quickload :linear-programming)`.
You can check that it works by running `(asdf:test-system :linear-programming)`.

If you are not using Quicklisp, place this repository, Alexandria, and Iterate somewhere where ASDF can find them.
Then, it can be loaded with `(asdf:load-system :linear-programming)` and tested as above.


## Usage
See [neil-lindquist.github.io/linear-programming/](https://neil-lindquist.github.io/linear-programming/) for further documentation.

Consider the following linear programming problem.
> maximize  x + 4y + 3z  
> such that  
> * 2x + y &#x2264; 8  
> * y + z &#x2264; 7
> * x, y, z &#x2265; 0

First, the problem needs to be specified.
Problems are specified with a simple DSL, as described in the [syntax reference](https://neil-lindquist.github.io/linear-programming/linear-problem-syntax).
```common-lisp
(use-package :linear-programming)

(defvar problem (parse-linear-problem '(max (= w (+ x (* 4 y) (* 3 z))))
                                      '((<= (+ (* 2 x) y) 8)
                                        (<= (+ y z) 7))))
```
Once the problem is created, it can be solved with the simplex method.
```common-lisp
(defvar solution (solve-problem problem))
```
Finally, the optimal tableau can be inspected to get the resulting objective function, decision variables, and reduced-costs (i.e. the shadow prices for the variable's lower bounds).
```common-lisp
(format t "Objective value solution: ~A~%" (solution-variable solution 'w))
(format t "x = ~A (reduced cost: ~A)~%" (solution-variable solution 'x) (solution-reduced-cost solution 'x))
(format t "y = ~A (reduced cost: ~A)~%" (solution-variable solution 'y) (solution-reduced-cost solution 'y))
(format t "z = ~A (reduced cost: ~A)~%" (solution-variable solution 'z) (solution-reduced-cost solution 'z))

;; ==>
;; Objective value solution: 57/2
;; x = 1/2 (reduced cost: 0)
;; y = 7 (reduced cost: 0)
;; z = 0 (reduced cost: 1/2)
```
Alternatively, the `with-solution-variables` and `with-solved-problem` macros simplify some steps and binds the solution variables in their bodies.

```common-lisp
(with-solution-variables (w x y z) solution
  (format t "Objective value solution: ~A~%" w)
  (format t "x = ~A (reduced cost: ~A)~%" x (reduced-cost solution 'x))
  (format t "y = ~A (reduced cost: ~A)~%" y (reduced-cost solution 'y))
  (format t "z = ~A (reduced cost: ~A)~%" z (reduced-cost solution 'z)))

;; ==>
;; Objective value solution: 57/2
;; x = 1/2 (reduced cost: 0)
;; y = 7 (reduced cost: 0)
;; z = 0 (reduced cost: 1/2)


(with-solved-problem ((max (= w (+ x (* 4 y) (* 3 z))))
                      (<= (+ (* 2 x) y) 8)
                      (<= (+ y z) 7))
  (format t "Objective value solution: ~A~%" w)
  (format t "x = ~A (reduced cost: ~A)~%" x (reduced-cost solution 'x))
  (format t "y = ~A (reduced cost: ~A)~%" y (reduced-cost solution 'y))
  (format t "z = ~A (reduced cost: ~A)~%" z (reduced-cost solution 'z)))

;; ==>
;; Objective value solution: 57/2
;; x = 1/2 (reduced cost: 0)
;; y = 7 (reduced cost: 0)
;; z = 0 (reduced cost: 1/2)
```
