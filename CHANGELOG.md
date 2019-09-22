## 2.0.0
* Modify the use of the term "shadow price" to the correct term "reduced cost"
* Add an interface for replacing the solver backend
* Add input/output for reading problem sexps from file
* Add support for specifying linear expressions as alists and plists
* Add support for problems to contain signed variables
  * Note that the default solver doesn't support them though
* Fix the documentation generator script being installed by Roswell
* Improve documentation

## v1.0.1
* Improve bounding of brand-and-bound integer programming
* Improve tableau readability by switching rows & columns
* Fix typo that prevented inlining `copy-tableau`
