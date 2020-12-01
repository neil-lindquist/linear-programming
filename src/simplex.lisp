
(uiop:define-package :linear-programming/simplex
  (:use :cl
        :iterate
        :linear-programming/conditions
        :linear-programming/problem
        :linear-programming/utils)
  (:import-from :alexandria
                #:curry
                #:if-let
                #:when-let
                #:copy-array
                #:once-only)
  (:export #:tableau
           #:tableau-p
           #:copy-tableau
           #:tableau-problem
           #:tableau-instance-problem
           #:tableau-matrix
           #:tableau-basis-columns
           #:tableau-var-count
           #:tableau-constraint-count
           #:tableau-objective-value
           #:tableau-variable
           #:tableau-reduced-cost
           #:with-tableau-variables

           #:pivot-row
           #:n-pivot-row

           #:build-tableau
           #:solve-tableau
           #:n-solve-tableau
           #:simplex-solver)
  (:documentation "The actual simplex solver implementation for the default solver backend. This
package should be used through the interface provided by the
`linear-programming/solver` package."))

(in-package :linear-programming/simplex)


;;; Tableau representation

(deftype var-mapping-entry ()
  "Represents a maping from a problem's variable to a set of columns in the tableau."
  '(cons (member positive negative signed) (cons fixnum (or null (cons real null)))))

(defstruct (tableau (:copier #:shallow-tableau-copy))
  "Contains the necessary information for a simplex tableau."
  ; note that two tableaus are stored, which may differ when solving integer problems
  (problem nil :read-only t :type problem) ; the overall problem
  (instance-problem nil :read-only t :type problem) ; the problem for this specific tableau
  (matrix #2A() :read-only t :type (simple-array real 2))
  (basis-columns #() :read-only t :type (simple-array fixnum (*)))
  (var-count 0 :read-only t :type (and fixnum unsigned-byte))
  (constraint-count 0 :read-only t :type (and fixnum unsigned-byte))
  (var-mapping (make-hash-table :test #'eq) :read-only t :type hash-table)
  (fp-tolerance-factor 1024 :read-only t :type real))

(declaim (inline copy-tableau))
(defun copy-tableau (tableau)
  "Copies the given tableau and it's matrix."
  (declare (optimize (speed 3)))
  (make-tableau :problem (tableau-problem tableau)
                :instance-problem (tableau-instance-problem tableau)
                :matrix (copy-array (tableau-matrix tableau))
                :basis-columns (copy-array (tableau-basis-columns tableau))
                :var-count (tableau-var-count tableau)
                :constraint-count (tableau-constraint-count tableau)
                :var-mapping (tableau-var-mapping tableau)
                :fp-tolerance-factor (tableau-fp-tolerance-factor tableau)))

(declaim (inline tableau-objective-value))
(defun tableau-objective-value (tableau)
  "Gets the objective function value in the tableau."
  (aref (tableau-matrix tableau)
        (tableau-constraint-count tableau)
        (tableau-var-count tableau)))

(declaim (inline tableau-variable))
(defun tableau-variable (tableau var)
  "Gets the value of the given variable from the tableau"
  (if (eq var (problem-objective-var (tableau-instance-problem tableau)))
    (tableau-objective-value tableau)
    (let* ((mapping (gethash var (tableau-var-mapping tableau))))
      (unless mapping
        (error "~S is not a variable in the tableau" var))
      (locally
        (declare (type var-mapping-entry mapping))
        (ecase (first mapping)
          (positive
           (+ (third mapping)
              (if-let (idx (position (second mapping) (tableau-basis-columns tableau)))
                (aref (tableau-matrix tableau) idx (tableau-var-count tableau))
                0)))
          (negative
           (+ (third mapping)
              (if-let (idx (position (second mapping) (tableau-basis-columns tableau)))
                (- (aref (tableau-matrix tableau) idx (tableau-var-count tableau)))
                0)))
          (signed
           (- (if-let (idx (position (second mapping) (tableau-basis-columns tableau)))
                (aref (tableau-matrix tableau) idx (tableau-var-count tableau))
                0)
              (if-let (idx (position (1+ (second mapping)) (tableau-basis-columns tableau)))
                (aref (tableau-matrix tableau) idx (tableau-var-count tableau))
                0))))))))


(declaim (inline tableau-reduced-cost))
(defun tableau-reduced-cost (tableau var)
  "Gets the reduced cost (i.e. the shadow price for the lower bound) for the given
variable from the tableau."
  (let* ((mapping (gethash var (tableau-var-mapping tableau))))
    (unless mapping
      (error "~S is not a variable in the tableau" var))
    (unless (eq (first mapping) 'positive)
      (error "~S has no lower bound" var))
    (aref (tableau-matrix tableau)
          (tableau-constraint-count tableau) (the fixnum (second mapping)))))

;; TODO implement upper-bound equivalent for reduced-cost
;; Requires keeping track of which row is the upperbound for positive vars

(defmacro with-tableau-variables (var-list tableau &body body)
  "Evaluates the body with the variables in `var-list` bound to their values from
the tableau."
  (once-only (tableau)
    (if (typep var-list 'problem)
      (let ((problem var-list)) ; for readability
        `(let ((,(problem-objective-var problem) (tableau-objective-value ,tableau))
               ,@(iter (for var in-sequence (problem-vars problem))
                   (collect `(,var (tableau-variable ,tableau ',var)))))
           (declare (ignorable ,(problem-objective-var problem)
                               ,@(map 'list #'identity (problem-vars problem))))
           ,@body))
      `(let (,@(iter (for var in-sequence var-list)
                 (collect `(,var (tableau-variable ,tableau ',var)))))
         ,@body))))


(defun build-tableau (problem instance-problem &key (fp-tolerance-factor 1024))
  "Creates the tableau from the given linear problem.  If the trivial basis is not
feasible, instead a list is returned containing the two tableaus for a two-phase
simplex method."
  (let* ((constraints (problem-constraints instance-problem))
         (num-problem-vars (length (problem-vars problem)))
         (mappings (make-hash-table :size (ceiling num-problem-vars 7/10)
                                    :rehash-threshold 7/10
                                    :rehash-size 5)) ; only bump up size if our numbers end up slightly off
         (num-problem-var-cols num-problem-vars)) ; columns for problem vars
    (declare (type fixnum num-problem-var-cols))
    (unless constraints
      ;; There aren't any constraints, so simply max/min out each var in a trivial tableau
      (return-from build-tableau
                   (let ((vars (problem-vars problem))
                         (matrix (make-array (list (1+ num-problem-vars) (1+ num-problem-vars))
                                             :element-type 'real
                                             :initial-element 0))
                         (objective-value 0)
                         (basis-cols (make-array (list num-problem-vars) :element-type 'fixnum)))
                     (iter (with max-problem-p = (eq (problem-type problem) 'max))
                           (for i from 0)
                           (for var in-vector vars)
                           (for obj-coef = (cdr (assoc var (problem-objective-func problem))))
                           (for bound = (assoc var (problem-var-bounds problem)))
                       (setf (aref basis-cols i) i
                             (aref matrix i i) 1)
                       (if (eq (<= 0 obj-coef) max-problem-p) ; XNOR of t/nil values
                        (if (cddr bound)
                          (setf (gethash (aref vars i) mappings) (list 'positive i (cddr bound))
                                objective-value (+ objective-value (* obj-coef (cddr bound))))
                          (error 'unbounded-problem-error))
                        (if (cadr bound)
                          (setf (gethash (aref vars i) mappings) (list 'positive i (cadr bound))
                                objective-value (+ objective-value (* obj-coef (cadr bound))))
                          (error 'unbounded-problem-error))))
                     (setf (aref matrix num-problem-vars num-problem-vars) objective-value)
                     (make-tableau :problem problem
                                   :instance-problem problem
                                   :matrix matrix
                                   :basis-columns basis-cols
                                   :var-count num-problem-vars
                                   :constraint-count num-problem-vars
                                   :var-mapping mappings
                                   :fp-tolerance-factor fp-tolerance-factor))))


    (iter (for var in-vector (problem-vars problem))
          (for bound = (find var (problem-var-bounds problem) :key #'first))
          (for var-id upfrom 0)
          (generate column upfrom 0)
          (next column)
      (setf (gethash var mappings)
            (cond
              ((null bound)
               (list 'positive column 0))
              ((and (cadr bound) (cddr bound))
               (push (if (<= 0 (cddr bound))
                       `(<= ((,var . 1)) ,(cddr bound))
                       `(>= ((,var . 1)) ,(- (cddr bound))))
                     constraints)
               (list 'positive column (cadr bound)))
              ((cadr bound)
               (list 'positive column (cadr bound)))
              ((cddr bound)
               (list 'negative column (cddr bound)))
              (t
               (prog1
                 (list 'signed column)
                 (next column)
                 (incf num-problem-var-cols))))))

    (let* ((num-constraints (length constraints))
           (num-slack (count-if-not (curry #'eq '=) constraints :key #'first))
           (num-cols (+ num-problem-var-cols num-slack 1))
           (matrix (make-array (list (1+ num-constraints) num-cols)
                              :element-type 'real
                              :initial-element 0))
           (basis-columns (make-array (list num-constraints) :element-type 'fixnum))
           (artificial-var-rows nil))
      ;; constraint rows
      (iter (with col-offset = 0)
            (for row from 0 below num-constraints)
            (for constraint in (problem-constraints instance-problem))
        ;; rhs
        (setf (aref matrix row (1- num-cols)) (third constraint))
        ;; variables
        (iter (for (var . coef) in (second constraint))
              (for mapping = (gethash var mappings))
          (declare (type (or null var-mapping-entry) mapping))
          (ecase (first mapping)
            (positive
             (setf (aref matrix row (second mapping)) coef)
             (decf (aref matrix row (1- num-cols)) (* coef (third mapping))))
            (negative
             (setf (aref matrix row (second mapping)) (- coef))
             (decf (aref matrix row (1- num-cols)) (* coef (third mapping))))
            (signed
             (setf (aref matrix row (second mapping)) coef
                   (aref matrix row (1+ (second mapping))) (- coef)))))
        ;; slack
        (case (first constraint)
          (<= (setf (aref matrix row (+ num-problem-var-cols col-offset)) 1
                    (aref basis-columns row) (+ num-problem-var-cols col-offset))
              (incf col-offset))
          (>= (push row artificial-var-rows)
              (setf (aref matrix row (+ num-problem-var-cols col-offset)) -1
                    (aref basis-columns row) num-cols)
              (incf col-offset))
          (= (push row artificial-var-rows)
             (setf (aref basis-columns row) num-cols))
          (t (error 'parsing-error
                    :description (format nil "~S is not a valid constraint equation" constraint)))))
      ;; objective row
      (iter (for (var . coef) in (problem-objective-func problem))
            (for mapping = (gethash var mappings))
        (declare (type (or null var-mapping-entry) mapping))
        (ecase (first mapping)
          (positive
           (setf (aref matrix num-constraints (second mapping)) (- coef))
           (incf (aref matrix num-constraints (1- num-cols)) (* coef (third mapping))))
          (negative
           (setf (aref matrix num-constraints (second mapping)) coef)
           (incf (aref matrix num-constraints (1- num-cols)) (* coef (third mapping))))
          (signed
           (setf (aref matrix num-constraints (second mapping)) (- coef)
                 (aref matrix num-constraints (1+ (second mapping))) coef))))
      (let ((main-tableau (make-tableau :problem problem
                                        :instance-problem instance-problem
                                        :matrix matrix
                                        :basis-columns basis-columns
                                        :var-count (1- num-cols)
                                        :constraint-count num-constraints
                                        :var-mapping mappings
                                        :fp-tolerance-factor fp-tolerance-factor))
            (art-tableau (when artificial-var-rows
                           (let* ((num-art (length artificial-var-rows))
                                  (num-art-cols (+ num-cols num-art))
                                  (art-matrix (make-array (list (1+ num-constraints) num-art-cols)
                                                          :element-type 'real
                                                          :initial-element 0))
                                  (art-basis-columns (copy-array basis-columns)))
                             (iter (for row in artificial-var-rows)
                                   (for i from 0)
                               (setf (aref art-basis-columns row)
                                     (+ num-cols -1 i))
                               (setf (aref art-matrix row (+ num-cols -1 i)) 1))

                             ;copy coefficients
                             (iter (for c from 0 below (1- num-cols))
                               (setf (aref art-matrix num-constraints c)
                                     (iter (for r from 0 below num-constraints)
                                        (setf (aref art-matrix r c) (aref matrix r c))
                                        (when (member r artificial-var-rows)
                                          (sum (aref art-matrix r c))))))
                             ;copy rhs
                             (let ((c (1- num-art-cols)))
                               (setf (aref art-matrix num-constraints c)
                                     (iter (for r from 0 below num-constraints)
                                       (setf (aref art-matrix r c)
                                             (aref matrix r (1- num-cols)))
                                       (when (member r artificial-var-rows)
                                         (sum (aref art-matrix r c))))))
                             (make-tableau :problem problem
                                           :instance-problem (linear-programming/problem::make-problem
                                                                           :type 'min ;artificial problem
                                                                           :vars (problem-vars problem))
                                           :matrix art-matrix
                                           :basis-columns art-basis-columns
                                           :var-count (+ num-cols -1 num-art)
                                           :constraint-count num-constraints
                                           :var-mapping mappings
                                           :fp-tolerance-factor fp-tolerance-factor)))))
        (if art-tableau
          (list art-tableau main-tableau)
          main-tableau)))))


;;; Tableau solver

(defun pivot-row (tableau entering-col changing-row)
  "Non-destructively applies a single pivot to the table."
  (n-pivot-row (copy-tableau tableau) entering-col changing-row))

(defun n-pivot-row (tableau entering-col changing-row)
  "Destructively applies a single pivot to the table."
  (declare (optimize (speed 3))
           (type fixnum entering-col changing-row))
  (let* ((matrix (tableau-matrix tableau))
         (row-count (array-dimension matrix 0))
         (col-count (array-dimension matrix 1)))
    (let ((row-scale (aref matrix changing-row entering-col)))
      (iter (declare (iterate:declare-variables)
                     (optimize (speed 3) (safety 0)))
            (for (the fixnum c) from 0 below col-count)
        (setf (aref matrix changing-row  c) (/ (aref matrix changing-row c) row-scale))))
    (iter (declare (iterate:declare-variables)
                   (optimize (speed 3) (safety 0)))
          (for (the fixnum r) from 0 below row-count)
      (unless (= r changing-row)
        (let ((scale (aref matrix r entering-col)))
          (iter (declare (iterate:declare-variables)
                         (optimize (speed 3) (safety 0)))
                (for (the fixnum c) from 0 below col-count)
            (decf (aref matrix r c) (* scale (aref matrix changing-row c))))))))
  (setf (aref (tableau-basis-columns tableau) changing-row) entering-col)
  tableau)

(declaim (inline find-entering-column))
(defun find-entering-column (tableau)
  "Gets the column to add to the basis."
  (let ((num-constraints (tableau-constraint-count tableau)))
    (if (eq 'max (problem-type (tableau-instance-problem tableau)))
      (iter (for i from 0 below (tableau-var-count tableau))
        (finding i minimizing (aref (tableau-matrix tableau) num-constraints i)
                  into col)
        (finally
          (return (when (fp< (aref (tableau-matrix tableau) num-constraints col) 0
                             (/ (tableau-fp-tolerance-factor tableau) 8))
                    col))))
      (iter (for i from 0 below (tableau-var-count tableau))
        (finding i maximizing (aref (tableau-matrix tableau) num-constraints i)
                   into col)
        (finally
          (return (when (fp> (aref (tableau-matrix tableau) num-constraints col) 0
                             (/ (tableau-fp-tolerance-factor tableau) 8))
                    col)))))))

(declaim (inline find-pivoting-row))
(defun find-pivoting-row (tableau entering-col)
  "Gets the column that will leave the basis."
  (let ((matrix (tableau-matrix tableau)))
    (iter (for i from 0 below (tableau-constraint-count tableau))
      (when (fp< 0 (aref matrix i entering-col)
                 (/ (tableau-fp-tolerance-factor tableau) 2))
        (finding i minimizing (/ (aref matrix i (tableau-var-count tableau))
                                 (aref matrix i entering-col)))))))

(defun solve-tableau (tableau)
  "Attempts to solve the tableau using the simplex method. If a list of two
tableaus is given, then a two-phase version is used. The original tableau(s) are
unchanged."
  (if (listp tableau)
    (n-solve-tableau (mapcar #'copy-tableau tableau))
    (n-solve-tableau (copy-tableau tableau))))

(defun n-solve-tableau (tableau)
  "A non-consing version of [`solve-tableau`](#function-linear-programming/simplex:solve-tableau)."
  (cond
    ((listp tableau)
     (let ((solved-art-tab (n-solve-tableau (first tableau)))
           (main-tab (second tableau)))
       (unless (fp= 0 (tableau-objective-value solved-art-tab)
                    (tableau-fp-tolerance-factor solved-art-tab))
         (error 'infeasible-problem-error))

       ;; Have starting basis, use solve-art-tab to set main-tab to that basis
       (let ((main-matrix (tableau-matrix main-tab))
             (art-matrix (tableau-matrix solved-art-tab))
             (art-basis (tableau-basis-columns solved-art-tab))
             (num-vars (tableau-var-count main-tab))
             (num-art-vars (tableau-var-count solved-art-tab))
             (num-constraints (tableau-constraint-count main-tab)))

         ;; Check that all artificial variables are out of the basis
         ;; Degeneracy can allow an artificial variable to be zero, but still in the basis
         (iter (for basis-col in-vector (tableau-basis-columns solved-art-tab))
               (for i from 0)
           (when (>= basis-col num-vars)
             ;; Need to find non-basis variable with a nonzero in this row
             (when (/= 0 (aref art-matrix i num-art-vars))
               (error (format nil "Artificial variable ~S still non-zero" basis-col)))
             (let ((new-col -1))
               (iter (for j from 0 below num-vars)
                 (when (and (/= 0 (aref art-matrix i j))
                            (iter (for new-col in-vector art-basis)
                              (always (/= new-col j))))
                   (setf new-col j)
                   (return)))
               (when (= new-col -1)
                 (error "Artificial variable still in basis and cannot be replaced"))
               (n-pivot-row solved-art-tab new-col i))))

         ;; Copy tableau coefficients/RHS
         (iter (for row from 0 below num-constraints)
           (iter (for col from 0 below num-vars)
             (setf (aref main-matrix row col) (aref art-matrix row col)))
           (setf (aref main-matrix row num-vars)
                 (aref art-matrix row num-art-vars)))

         ;; Update basis columns and objective row to match
         (iter (for basis-col in-vector art-basis)
               (for i from 0)
           (setf (aref (tableau-basis-columns main-tab) i) basis-col)
           (let ((scale (aref main-matrix num-constraints basis-col)))
             (when (/= 0 scale)
               (iter (for col from 0 to num-vars)
                 (decf (aref main-matrix num-constraints col)
                       (* scale (aref main-matrix i col))))))))
       (n-solve-tableau main-tab)))
    ((tableau-p tableau)
     (iter (for entering-column = (find-entering-column tableau))
           (while entering-column)
       (let ((pivoting-row (find-pivoting-row tableau entering-column)))
         (unless pivoting-row
           (error 'unbounded-problem-error))
         (n-pivot-row tableau entering-column pivoting-row)))
     tableau)
    (t (check-type tableau tableau))))


;;; Branch and Bound solver

(defun gen-entries (tableau entry)
  "Generates new entries to correct one of the integer constraints."
  (let* ((split-var (violated-integer-constraint tableau))
         (split-var-val (tableau-variable tableau split-var)))
    (list (list* `(<= ((,split-var . 1)) ,(floor split-var-val))
                 entry)
          (list* `(>= ((,split-var . 1)) ,(ceiling split-var-val))
                 entry))))

(defun violated-integer-constraint (tableau)
  "Gets a variable that is required to be an integer but is currently violating
that constraint."
  (iter (for var in (problem-integer-vars (tableau-problem tableau)))
    (unless (integerp (tableau-variable tableau var))
      (return var))))


(defun build-and-solve (problem extra-constraints &key (fp-tolerance-factor 1024))
  "Builds and solves a tableau with the extra constrains added to the problem."
  ;if problem becomes infeasible, just return :infeasible
  (handler-case
    (solve-tableau
      (build-tableau
        problem
        (if (null extra-constraints)
          problem
          (linear-programming/problem::make-problem
                        :type (problem-type problem)
                        :vars (problem-vars problem)
                        :objective-var (problem-objective-var problem)
                        :objective-func (problem-objective-func problem)
                        :integer-vars (problem-integer-vars problem)
                        :var-bounds (problem-var-bounds problem)
                        :constraints (append extra-constraints
                                             (problem-constraints problem))))
        :fp-tolerance-factor fp-tolerance-factor))
    (infeasible-problem-error () :infeasible)))



(defun simplex-solver (problem &rest args)
  "The solver interface function for the simplex backend.  The `fp-tolerance`
keyword argument can be used to indicate the tolerance for error on floating
point comparisons (defaults to 1024)."

  (let ((fp-tolerance-factor (getf args :fp-tolerance 1024))
        (current-best nil)
        (current-solution nil)
        (stack (list '()))
        (comparator (if (eq (problem-type problem) 'max) '< '>)))
    (iter (while stack)
      (let* ((entry (pop stack))
             (tab (build-and-solve problem entry :fp-tolerance-factor fp-tolerance-factor)))
           (cond
             ; Reached an infeasible leaf.  Do nothing
             ((eq tab :infeasible))

             ; This branch can't contain the optimal solution.  Do nothing
             ((and (violated-integer-constraint tab)
                   current-best
                   (not (funcall comparator current-best (tableau-objective-value tab)))))

             ; Not integral, but not suboptimal.  Add children to stack
             ((violated-integer-constraint tab)
              (setf stack (append (gen-entries tab entry) stack)))

             ; Integral.  If better than best, save this result.
             ((or (not current-best)
                  (funcall comparator current-best (tableau-objective-value tab)))
              (setf current-best (tableau-objective-value tab)
                    current-solution tab)))))

    ; if it failed to find a solution, raise the appropriate error
    ; otherwise, return the found solution
    (if current-solution
      current-solution
      (error 'infeasible-problem-error))))
