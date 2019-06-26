---
layout: page
title: Linear Problem DSL
meta-description: The specification of the Linear Programming Problem DSL
---

`make-linear-problem` uses a basic DSL to describe the linear-problem to assemble.
It takes an *optimization-problem* form, followed by the *constraint* forms, both of which are described by the following grammar.
Additionally, `parse-linear-problem` uses the same grammar for its arguments.

*optimization-problem* = (min|max *linear-expression*) | (= *objective-variable* (min|max *linear-expression)) | (min|max (= *objective-variable* *linear-expression))  
*constraint* = (<=|<|>=|>|= *linear-expression*\*)  
*linear-expression* = *var* | *number* | (\* *var* *number*) | (\* *number* *var*) | (\* *number* *number*) | (+ *linear-expression*\*)

### simple constraints
After a linear problem is parsed, the constraints are stored using a simplified version of the DSL.
The in/equality constraints are represented by a `<=`, `>=`, or `=` expression with two arguments.
The first argument is an alist mapping the variables with their coefficients for the linear expression.
The second argument is a non-negative constant.
