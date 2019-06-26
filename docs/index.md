---
layout: page
meta-description: The documentation page for the linear-programming Common Lisp library.
---

This library is designed to solve linear programming problems from common lisp.
It uses a basic DSL to describe the problems, and a basic implementation of the Simplex method to solve the system.  See https://neil-lindquist.github.io/linear-programming/linear-problem-syntax for a description of the DSL.

## Installation
If using Quicklisp, place the repository contents into `~/quicklisp/local-projects/linear-programming`.
Then, load the system using `(ql:quickload :linear-programming)`.

If not using Quicklisp, the dependencies must be manually installed.
Currently, the only dependencies are Alexandria and Iterate.
Place all three libraries on a path searched by ASDF, then load them using `(asdf:load-system :linear-programming)`.

The installation can be tested by running `(asdf:test-system :linear-programming)`.
