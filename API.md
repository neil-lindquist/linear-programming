---
layout: page
title: API Documentation
meta-description: The API Documentation for the linear-programming Common Lisp library.
---


<br>
### <a name="package-linear-programming"></a>**PACKAGE** - LINEAR-PROGRAMMING   
The overall package for the linear programming library. It contains only the
reexported symbols of `linear-programming/problem`, `linear-programming/solver`,
`linear-programming/conditioner`, and `linear-programming/external-formats`,
plus `simplex-solver` from `linear-programming/simplex`.

<br>
### <a name="package-linear-programming/problem"></a>**PACKAGE** - LINEAR-PROGRAMMING/PROBLEM   
Handles the representation of linear programming problems.

<a name="function-linear-programming/problem:problem-constraints"></a>**FUNCTION** - PROBLEM-CONSTRAINTS (INSTANCE)  
A list of (in)equality constraints.

<a name="function-linear-programming/problem:problem-type"></a>**FUNCTION** - PROBLEM-TYPE (INSTANCE)  
Whether the problem is a `min` or `max` problem.

<a name="function-linear-programming/problem:problem-var-bounds"></a>**FUNCTION** - PROBLEM-VAR-BOUNDS (INSTANCE)  
A list of variable bounds, of the form `(var . (lower-bound . upper-bound))`.

<a name="struct-linear-programming/problem:problem"></a>**STRUCT** - PROBLEM   
The representation of a linear programming problem.

<a name="macro-linear-programming/problem:make-linear-problem"></a>**MACRO** - MAKE-LINEAR-PROBLEM (OBJECTIVE &REST CONSTRAINTS)  
Creates a linear problem from the expressions in the body

<a name="function-linear-programming/problem:problem-objective-var"></a>**FUNCTION** - PROBLEM-OBJECTIVE-VAR (INSTANCE)  
The name of the objective function.

<a name="function-linear-programming/problem:problem-vars"></a>**FUNCTION** - PROBLEM-VARS (INSTANCE)  
An array of the variables specified in the problem.

<a name="function-linear-programming/problem:parse-linear-problem"></a>**FUNCTION** - PARSE-LINEAR-PROBLEM (OBJECTIVE-EXP CONSTRAINTS)  
Parses the expressions into a linear programming problem

<a name="function-linear-programming/problem:problem-integer-vars"></a>**FUNCTION** - PROBLEM-INTEGER-VARS (INSTANCE)  
A list of variables with integer constraints.

<a name="function-linear-programming/problem:problem-objective-func"></a>**FUNCTION** - PROBLEM-OBJECTIVE-FUNC (INSTANCE)  
The objective function as a linear expression alist.

<br>
### <a name="package-linear-programming/solver"></a>**PACKAGE** - LINEAR-PROGRAMMING/SOLVER   
The high level linear programming solver interface. This interface is able to
wrap multiple backends. The backend can be adjusted by setting the `*solver*`
variable. The default backend is the `simplex-solver` in the
`linear-programming/simplex` package.

<a name="generic-linear-programming/solver:solution-reduced-cost"></a>**GENERIC** - SOLUTION-REDUCED-COST (SOLUTION VARIABLE)  
Gets the reduced cost of the specified variable.  This is the
amount that the objective coefficient for the variable must increase or
decrease, for maximization and minimization problems respectively, before the
given variable appears in an optimal solution.

<a name="generic-linear-programming/solver:solution-problem"></a>**GENERIC** - SOLUTION-PROBLEM (SOLUTION)  
Gets the original problem for the solution.

<a name="variable-linear-programming/solver:\*solver\*"></a>**VARIABLE** - \*SOLVER\*   
The function that should be used by solve-problem (defaults to
`linear-programming/simplex:simplex-solver`). The function should take a
problem, and any backend specific keyword arguments and returns some form of
solution object. The solution object should support the following methods
`solution-problem`, `solution-objective-value`, `solution-variable`, and
`solution-reduced-cost`.

<a name="generic-linear-programming/solver:solution-variable"></a>**GENERIC** - SOLUTION-VARIABLE (SOLUTION VARIABLE)  
Gets the value of the specified variable.

<a name="macro-linear-programming/solver:with-solution-variables"></a>**MACRO** - WITH-SOLUTION-VARIABLES (VAR-LIST SOLUTION &BODY BODY)  
Evaluates the body with the variables in `var-list` bound to their values in the
solution. Additionally, the macro `reduced-cost` is locally bound that takes a
variable name and provides it's reduced cost.

<a name="generic-linear-programming/solver:solution-objective-value"></a>**GENERIC** - SOLUTION-OBJECTIVE-VALUE (SOLUTION)  
Gets the value of the objective function.

<a name="function-linear-programming/solver:solve-problem"></a>**FUNCTION** - SOLVE-PROBLEM (PROBLEM &REST ARGS &KEY &ALLOW-OTHER-KEYS)  
Solves the given problem using the function stored by `*solver*`. Any keyword
arguments are passed to the solver function.

<a name="macro-linear-programming/solver:with-solved-problem"></a>**MACRO** - WITH-SOLVED-PROBLEM ((OBJECTIVE-FUNC &REST CONSTRAINTS) &BODY BODY)  
Takes the problem description, and evaluates `body` with the variables of the
problem bound to their solution values. Additionally, the macro `reduced-cost`
is locally bound that takes a variable name and provides it's reduced cost.

<br>
### <a name="package-linear-programming/external-formats"></a>**PACKAGE** - LINEAR-PROGRAMMING/EXTERNAL-FORMATS   
Handles reading and writing problems to external formats.

<a name="function-linear-programming/external-formats:read-mps"></a>**FUNCTION** - READ-MPS (STREAM PROBLEM-TYPE &KEY PACKAGE (READ-CASE (READTABLE-CASE \*READTABLE\*))
 (TRIM-NAMES-P T) (NUMBER-TYPE 'RATIONAL) RHS-ID)  
Reads a problem in MPS format from the given stream. Note that a line starting
with `ENDATA` ends the problem, so MPS files can be embedded in streams of other
data. Only the fixed width version of the format is supported, but both the
`OBJSENCE` and `OBJNAME` headers are supported and the `BV`, `LI`, and `UI`
boundaries are supported.

<a name="function-linear-programming/external-formats:write-sexp"></a>**FUNCTION** - WRITE-SEXP (STREAM PROBLEM &KEY PACKAGE)  
Writes the problem as a sexp.  The first element is the objective function and
the rest of the elements are the constraints.

<a name="function-linear-programming/external-formats:write-standard-format"></a>**FUNCTION** - WRITE-STANDARD-FORMAT (STREAM PROBLEM &KEY (UNICODEP T) (AESTHETIC-VARIABLE-NAMES-P T))  
Writes a problem to the given stream in human readable, standard notation.  The
`unicodep` argument controls whether to print comparisons as unicode or ascii.
The `aesthetic-variable-names-p` argument controls whether variable names are
printed aesthetically.

<a name="function-linear-programming/external-formats:read-sexp"></a>**FUNCTION** - READ-SEXP (STREAM &KEY ALLOW-READ-EVAL PACKAGE)  
Loads a problem stored in sexp format.  This is a single sexp with the first
element being the objective function and the rest of the elements being the
constraints.  Note that normally `*read-eval*` is bound to `nil`, but can be
enabled with `allow-read-eval`; however, this should only be done when
parsing trusted data.

<br>
### <a name="package-linear-programming/conditions"></a>**PACKAGE** - LINEAR-PROGRAMMING/CONDITIONS   
Contains the various conditions used by this library.

<a name="condition-linear-programming/conditions:infeasible-integer-constraints-error"></a>**CONDITION** - INFEASIBLE-INTEGER-CONSTRAINTS-ERROR   
Indicates that there is no feasible region due to the integer constraints.

<a name="condition-linear-programming/conditions:unbounded-problem-error"></a>**CONDITION** - UNBOUNDED-PROBLEM-ERROR   
Indicates the feasible region is unbounded such that the optimal objective value
is infinite.

<a name="condition-linear-programming/conditions:unsupported-constraint-error"></a>**CONDITION** - UNSUPPORTED-CONSTRAINT-ERROR   
Indicates there are unsupported constraints or properties in the given problem.

<a name="condition-linear-programming/conditions:parsing-error"></a>**CONDITION** - PARSING-ERROR   
Indicates an error occured while parsing a linear problem. Includes a textual
description of the issue.

<a name="condition-linear-programming/conditions:infeasible-problem-error"></a>**CONDITION** - INFEASIBLE-PROBLEM-ERROR   
Indicates the there is no feasible region.

<a name="condition-linear-programming/conditions:invalid-bounds-error"></a>**CONDITION** - INVALID-BOUNDS-ERROR   
Indicates a problem with a variable's bounds.

<a name="condition-linear-programming/conditions:solver-error"></a>**CONDITION** - SOLVER-ERROR   
The base class for errors that occur with the solving algorithm.

<a name="condition-linear-programming/conditions:nonlinear-error"></a>**CONDITION** - NONLINEAR-ERROR   
Indicates a form was not a linear expression. This includes the use of
nonlinear functions and taking the product of multiple variables

<br>
### <a name="package-linear-programming/simplex"></a>**PACKAGE** - LINEAR-PROGRAMMING/SIMPLEX   
The actual simplex solver implementation for the default solver backend. This
package should be used through the interface provided by the
`linear-programming/solver` package.

<a name="function-linear-programming/simplex:solve-tableau"></a>**FUNCTION** - SOLVE-TABLEAU (TABLEAU)  
Attempts to solve the tableau using the simplex method. If a list of two
tableaus is given, then a two-phase version is used. The original tableau(s) are
unchanged.

<a name="function-linear-programming/simplex:tableau-basis-columns"></a>**FUNCTION** - TABLEAU-BASIS-COLUMNS (INSTANCE)

<a name="struct-linear-programming/simplex:tableau"></a>**STRUCT** - TABLEAU   
Contains the necessary information for a simplex tableau.

<a name="function-linear-programming/simplex:tableau-var-count"></a>**FUNCTION** - TABLEAU-VAR-COUNT (INSTANCE)

<a name="function-linear-programming/simplex:tableau-variable"></a>**FUNCTION** - TABLEAU-VARIABLE (TABLEAU VAR)  
Gets the value of the given variable from the tableau

<a name="function-linear-programming/simplex:copy-tableau"></a>**FUNCTION** - COPY-TABLEAU (TABLEAU)  
Copies the given tableau and it's matrix.

<a name="function-linear-programming/simplex:simplex-solver"></a>**FUNCTION** - SIMPLEX-SOLVER (PROBLEM &REST ARGS)  
The solver interface function for the simplex backend.  The `fp-tolerance`
keyword argument can be used to indicate the tolerance for error on floating
point comparisons (defaults to 1024).

<a name="macro-linear-programming/simplex:with-tableau-variables"></a>**MACRO** - WITH-TABLEAU-VARIABLES (VAR-LIST TABLEAU &BODY BODY)  
Evaluates the body with the variables in `var-list` bound to their values from
the tableau.

<a name="function-linear-programming/simplex:n-pivot-row"></a>**FUNCTION** - N-PIVOT-ROW (TABLEAU ENTERING-COL CHANGING-ROW)  
Destructively applies a single pivot to the table.

<a name="function-linear-programming/simplex:tableau-p"></a>**FUNCTION** - TABLEAU-P (OBJECT)

<a name="function-linear-programming/simplex:tableau-problem"></a>**FUNCTION** - TABLEAU-PROBLEM (INSTANCE)

<a name="function-linear-programming/simplex:tableau-instance-problem"></a>**FUNCTION** - TABLEAU-INSTANCE-PROBLEM (INSTANCE)

<a name="function-linear-programming/simplex:pivot-row"></a>**FUNCTION** - PIVOT-ROW (TABLEAU ENTERING-COL CHANGING-ROW)  
Non-destructively applies a single pivot to the table.

<a name="function-linear-programming/simplex:tableau-objective-value"></a>**FUNCTION** - TABLEAU-OBJECTIVE-VALUE (TABLEAU)  
Gets the objective function value in the tableau.

<a name="function-linear-programming/simplex:n-solve-tableau"></a>**FUNCTION** - N-SOLVE-TABLEAU (TABLEAU)  
A non-consing version of [`solve-tableau`](#function-linear-programming/simplex:solve-tableau).

<a name="function-linear-programming/simplex:tableau-reduced-cost"></a>**FUNCTION** - TABLEAU-REDUCED-COST (TABLEAU VAR)  
Gets the reduced cost (i.e. the shadow price for the lower bound) for the given
variable from the tableau.

<a name="function-linear-programming/simplex:build-tableau"></a>**FUNCTION** - BUILD-TABLEAU (PROBLEM INSTANCE-PROBLEM &KEY (FP-TOLERANCE-FACTOR 1024))  
Creates the tableau from the given linear problem.  If the trivial basis is not
feasible, instead a list is returned containing the two tableaus for a two-phase
simplex method.

<a name="function-linear-programming/simplex:tableau-constraint-count"></a>**FUNCTION** - TABLEAU-CONSTRAINT-COUNT (INSTANCE)

<a name="function-linear-programming/simplex:tableau-matrix"></a>**FUNCTION** - TABLEAU-MATRIX (INSTANCE)

<br>
### <a name="package-linear-programming/expressions"></a>**PACKAGE** - LINEAR-PROGRAMMING/EXPRESSIONS   
Contains functions for processing linear expressions.

<a name="function-linear-programming/expressions:parse-linear-expression"></a>**FUNCTION** - PARSE-LINEAR-EXPRESSION (EXPR)  
Parses the expression into a alist mapping variables to coefficients. Any
constant values are mapped to `+constant+`.

<a name="function-linear-programming/expressions:format-linear-expression"></a>**FUNCTION** - FORMAT-LINEAR-EXPRESSION (ALIST)  
Formats a linear expression as a sexp.

<a name="function-linear-programming/expressions:sum-linear-expressions"></a>**FUNCTION** - SUM-LINEAR-EXPRESSIONS (&REST EXPRS)  
Takes linear expressions and reduces it into a single expression.

<a name="function-linear-programming/expressions:scale-linear-expression"></a>**FUNCTION** - SCALE-LINEAR-EXPRESSION (EXPR SCALAR)  
Multiplies the linear expression by the given scalar.

