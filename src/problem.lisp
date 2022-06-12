
(uiop:define-package :linear-programming/problem
  (:use :cl
         :iterate
         :linear-programming/conditions
         :linear-programming/expressions)
  (:import-from :alexandria
                #:if-let)
  (:import-from :linear-programming/utils
                #:validate-bounds
                #:lb-max
                #:ub-min)
  (:export #:make-linear-problem
           #:parse-linear-problem

           #:parsing-error

           #:min
           #:max
           #:integer
           #:binary
           #:bounds
           #:<=
           #:>=
           #:<
           #:>
           #:=
           #:+
           #:*

           #:problem
           #:problem-type
           #:problem-vars
           #:problem-objective-var
           #:problem-objective-func
           #:problem-integer-vars
           #:problem-var-bounds
           #:problem-constraints)
  (:documentation "Handles the representation of linear programming problems."))

(in-package :linear-programming/problem)

(defstruct problem
  "The representation of a linear programming problem."
  (type 'max :read-only t :type (member max min))
  (vars #() :read-only t :type (simple-array symbol (*)))
  (objective-var '#:z :read-only t :type symbol)
  (objective-func nil :read-only t :type list)
  (integer-vars nil :read-only t :type list)
  (var-bounds nil :read-only t :type list)
  (constraints nil :read-only t :type list))

(setf (documentation 'problem-type 'function) "Whether the problem is a `min` or `max` problem."
      (documentation 'problem-vars 'function) "An array of the variables specified in the problem."
      (documentation 'problem-objective-var 'function) "The name of the objective function."
      (documentation 'problem-objective-func 'function) "The objective function as a linear expression alist."
      (documentation 'problem-integer-vars 'function) "A list of variables with integer constraints."
      (documentation 'problem-var-bounds 'function) "A list of variable bounds, of the form `(var . (lower-bound . upper-bound))`."
      (documentation 'problem-constraints 'function) "A list of (in)equality constraints.")

(defun parse-linear-constraints (exprs)
  "Parses the list of constraints and returns a list containing a list of simple
inequalities and a list of integer variables."
  (iter expressions-loop
        (for expr in exprs)
    (case (first expr)
      ((<= <)
       (when (eq (first expr) '<)
         (warn "< constraints are deprecated in favor of <= ones due to misleading semantics."))
       (collect (cons '<= (mapcar 'parse-linear-expression (rest expr)))
                into equalities))
      ((>= >)
       (when (eq (first expr) '>)
         (warn "> constraints are deprecated in favor of >= ones due to misleading semantics."))
       (collect (cons '<= (reverse (mapcar 'parse-linear-expression (rest expr))))
                into equalities))
      ((=)
       (collect (cons '= (mapcar 'parse-linear-expression (rest expr)))
                into equalities))
      ((integer)
       (unioning (rest expr)
                 into integer))
      ((bounds)
       (appending (mapcar (lambda (entry)
                            (cond
                              ((symbolp (first entry))
                               (unless (and (<= (length entry) 2)
                                            (or (null (second entry))
                                                (numberp (second entry))))
                                 (error 'parsing-error :description (format nil "Invalid bounds entry ~S" entry)))
                               (cons (first entry) (cons nil (second entry))))
                              (t
                               (unless (and (numberp (first entry))
                                            (symbolp (second entry))
                                            (or (null (third entry))
                                                (numberp (third entry))))
                                 (error 'parsing-error :description (format nil "Invalid bounds entry ~S" entry)))
                               (cons (second entry) (cons (first entry) (third entry))))))
                          (rest expr))
                  into bounds))
      ((binary)
       (unioning (rest expr)
                 into integer)
       (appending (mapcar (lambda (var) `(,var . (0 . 1)))
                          (rest expr))
                  into bounds))
      (t (error 'parsing-error :description (format nil "~A is not a valid constraint" expr))))
    (finally
      (iter equalities-loop
            (for constraint in equalities)
            (for op = (first constraint))
        (iter (for rhs in (nthcdr 2 constraint))
              (for lhs previous rhs initially (second constraint))
          (let* ((lin-exp (sum-linear-expressions
                            lhs (scale-linear-expression rhs -1)))
                 (const (- (cdr (or (assoc '+constant+ lin-exp :test 'eq) '(+constant+ . 0)))))
                 (sum (delete '+constant+ lin-exp :test 'eq :key 'car)))
            (unless const
              (setf const 0))
            (in equalities-loop
              (cond
                ((= 1 (length sum))
                 (let* ((var (first (first sum)))
                        (coef (rest (first sum)))
                        (const (/ const coef))
                        (new-bound (cond
                                     ((eq op '=) (cons const const))
                                     ((<= coef 0) (cons const nil))
                                     (t (cons nil const))))
                        (match (assoc var bounds))
                        (old-bound (cdr match)))
                   (if match
                     (setf (cdr match)
                           (cons (lb-max (car old-bound) (car new-bound))
                                 (ub-min (cdr old-bound) (cdr new-bound))))
                     (collect (cons var
                                    (cons (or (car new-bound) 0) ; if there isn't a previous bound, use the implicit bound
                                          (cdr new-bound)))
                              into extra-bounds))))
                ((eq op '=)
                 (collect (list '= sum const) into simple-constraints))
                ((<= 0 const)
                 (collect (list '<= sum const) into simple-constraints))
                (t
                 (collect (list '>= (scale-linear-expression sum -1) (- const))
                          into simple-constraints))))))
        (finally
          (return-from expressions-loop
            (list simple-constraints
                  integer
                  (reduce (lambda (result next)
                            (if-let (match (assoc (first next) result))
                              (let* ((lb (lb-max (car (cdr next)) (car (cdr match))))
                                     (ub (ub-min (cdr (cdr next)) (cdr (cdr next)))))
                                (validate-bounds lb ub (first next))
                                (setf (cdr match) (cons lb ub))
                                result)
                              (list* next result)))
                          (nconc extra-bounds bounds)
                          :initial-value nil))))))))



(defun parse-linear-problem (objective-exp constraints)
  "Parses the expressions into a linear programming problem"
  (let* ((objective-var-p (eq (first objective-exp) '=))
         (objective (if objective-var-p
                      (third objective-exp)
                      objective-exp))
         (objective-var (if objective-var-p
                          (second objective-exp)
                          (gensym "Z"))))
    (when (and (not objective-var-p)
               (listp (second objective))
               (eq (first (second objective)) '=))
      (setf objective-var (second (second objective)))
      (setf objective (list (first objective) (third (second objective))))
      (setf objective-var-p t))
    (unless (member (first objective) '(min max) :test 'eq)
      (error 'parsing-error
             :description (format nil "~A is neither min nor max in objective function ~A"
                                      (first objective) objective)))
    (let* ((type (first objective))
           (objective-func (parse-linear-expression (second objective)))
           (parsed-constraints (parse-linear-constraints constraints))
           (eq-constraints (first parsed-constraints))
           (integer-constraints (second parsed-constraints))
           (bounds (third parsed-constraints))
           ;collect all of the variables referenced
           (var-list (remove-duplicates (mapcar #'car objective-func)))
           (var-list (union var-list integer-constraints))
           (var-list (union var-list (mapcar #'first bounds)))
           (var-list (union var-list
                            (reduce (lambda (l1 l2) (union l1 (mapcar #'car l2)))
                                    eq-constraints
                                    :key 'second
                                    :initial-value nil)))
           (variables (make-array (length var-list)
                                  :initial-contents var-list
                                  :element-type 'symbol)))
      (make-problem :type type
                    :vars variables
                    :objective-var objective-var
                    :objective-func objective-func
                    :integer-vars integer-constraints
                    :var-bounds bounds
                    :constraints eq-constraints))))


(defmacro make-linear-problem (objective &rest constraints)
  "Creates a linear problem from the expressions in the body"
  `(parse-linear-problem ',objective ',constraints))
