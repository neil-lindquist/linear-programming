### make-linear-problem
`make-linear-problem` uses a basic DSL to describe the linear-problem to assemble.
It takes an *optimization-problem* form, followed by the *constraint* forms, both of which are described by the following grammar.

*optimization-problem* = (min|max *linear-expression*) | (= *objective-variable* (min|max *linear-expression)) | (min|max (= *objective-variable* *linear-expression))  
*constraint* = *inequality-constraint* | *positivity-constraint*  
*positivity-contraint* = (signed *var*\*)  
*inequality-constraint* = (<=|<|>=|> *linear-expression*\*)  
*linear-expression* = *var* | *number* | (\* *var* *number*) | (\* *number* *var*) | (\* *number* *number*) | (+ *linear-expression*\*)

### simple constraints
Simplified inequality constraints are stored with the linear problem.
These consist of either a `<=` or `>=` expression with two arguments.
The first argument is an alist mapping the variables with their coefficients for the linear expression.
The second argument is a non-negative constant.
