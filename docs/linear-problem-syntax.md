---
layout: page
title: Linear Problem DSL
meta-description: The specification of the Linear Programming Problem DSL
---

To effectively describe linear programming problems, `parse-linear-problem` uses a basic DSL.
A linear programming problem is described with an *optimization-function* form, followed by the *constraint* forms, both of which are described by the following grammar.

By default all variables are assumed to be bounded by zero and positive infinity, but the bounds can be adjusted by *simple-bounds* constraints.
Each entry in a *simple-bounds* constraint is a list of three values: the lower bound, the variable name, and the upper bound; infinity can be represented by either `nil` or by omitting the entry.
However, currently the default solver does not supported negative lower bounds, so such variables must be represented by a difference of the positive and negative components, eg `var` would be replaced with `(- var+ var-)`.

+ *objective-function* &#x2192; (min\|max *linear-expression*) \| (= *objective-variable* (min\|max *linear-expression*)) \| (min\|max (= *objective-variable* *linear-expression*))  
+ *constraint* &#x2192; *inequality-constraint* \| *integer-constraint*  \| *simple-bounds*
+ *inequality-constraint* &#x2192; (<=\|<\|>=\|>\|= *linear-expression*\*)  
+ *integer-constraint* &#x2192; (integer *var*\*) \| (binary *var*\*)  
+ *simple-bounds* &#x2192; (bounds (*number*? *var* *number*?)\*)
+ *linear-expression* &#x2192; *var* \| *number* \| (\+\|\-\|\*\|/ *linear-expression*\*) \| (:alist (*var* . *number*)\*) \| (:plist {*var* *number*}\*)


### Example
Consider the following linear programming problem.
> maximize  w = x + 4y + 3z  
> such that  
> * 2x + y &#x2264; 8  
> * y + z &#x2264; 7
> * x, y, z &#x2265; 0

This problem can be represented as follows.
```common-lisp
(parse-linear-problem
  '(max (= w (+ x (* 4 y) (* 3 z))))
  '((<= (+ (* 2 x) y) 8)
    (<= (+ y z) 7)))
```


### Simple Constraints
After a linear problem is parsed, the constraints are stored using a simplified version of the DSL.
The (in)equality constraints are represented by a `<=`, `>=`, or `=` expression with two arguments.
The first argument is an alist mapping the variables with their coefficients for the linear expression.
The second argument is a non-negative constant.
