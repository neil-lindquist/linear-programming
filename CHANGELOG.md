## 2.2.0
* Add support for writing problems in standard format
* Change the default objective variable to be uppercase
* Fix an error when a constraint does not have an explicit constant
* Fix an issue with a specific `parsing-error` being throw incorrectly
* Fix bugs with variables not bounded between 0 and infinity

## 2.1.0
* Add configuration for tolerance of floating point round off errors
* Fix some bugs with the handling of floating point round off error tolerances

## 2.0.1
* Fix errors when using floats
* Fix infeasible problems not correctly raising errors solver
* Improve performance of parsing and solving problems

## 2.0.0
* Modify the use of the term "shadow price" to the correct term "reduced cost"
* Add an interface for replacing the solver backend
* Add file input/output for problems
  * Sexp format (as per `make-linear-problem`)
  * MPS format
* Add support for specifying linear expressions as alists and plists
* Add support for specifying bounds for specific variables
* Add support in default backend for signed variables
* Fix the documentation generator script being installed by Roswell
* Improve documentation

## v1.0.1
* Improve bounding of brand-and-bound integer programming
* Improve tableau readability by switching rows & columns
* Fix typo that prevented inlining `copy-tableau`
