---
layout: page
title: Linear Problem DSL
meta-description: The specification of the Linear Programming Problem DSL
---

To effectively describe linear programming problems, `parse-linear-problem` and`make-linear-problem` uses a basic DSL.
A linear programming problem is described with an *optimization-function* form, followed by the *constraint* forms, both of which are described by the following grammar.

+ *objective-function* &#x2192; (min\|max *linear-expression*) \| (= *objective-variable* (min\|max *linear-expression*)) \| (min\|max (= *objective-variable* *linear-expression*))  
+ *constraint* &#x2192; *inequality-constraint* \| *integer-constraint*  
+ *inequality-constraint* &#x2192; (<=\|<\|>=\|>\|= *linear-expression*\*)  
+ *integer-constraint* &#x2192; (integer *var*\*) \| (binary *var*\*)  
+ *linear-expression* &#x2192; *var* \| *number* \| (\+\|\-\|\*\|/ *linear-expression*\*)


### Example
Consider the following linear programming problem.
> maximize  w = x + 4y + 3z  
> such that  
> * 2x + y <= 8  
> * y + z <= 7

This problem can be represented as follows.
```
(make-linear-problem
  (max (= w (+ x (* 4 y) (* 3 z))))
  (<= (+ (* 2 x) y) 8)
  (<= (+ y z) 7)))
```
Alternatively, `parse-linear-problem` does the same thing as a function that takes the list representing the objective function and a list of constraints.


### Simple Constraints
After a linear problem is parsed, the constraints are stored using a simplified version of the DSL.
The (in)equality constraints are represented by a `<=`, `>=`, or `=` expression with two arguments.
The first argument is an alist mapping the variables with their coefficients for the linear expression.
The second argument is a non-negative constant.
