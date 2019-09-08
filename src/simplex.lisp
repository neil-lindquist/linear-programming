
(uiop:define-package :linear-programming/simplex
  (:use :cl
        :iterate
        :linear-programming/conditions
        :linear-programming/system-info
        :linear-programming/problem)
  (:import-from :alexandria
                #:curry
                #:compose
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
           #:tableau-shadow-price

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

;;; helper macro to allow specializing code for different real type's
(defmacro specialize-tableau-matrix (m &body body)
  "Takes a tableau ."
    ;; if float arrays are specialized, use the array's type
    ;; otherwise, fallback to testing the type of an arbitrary element
  `(,@(if +float-array-specializable+
       `(ecase (array-element-type ,m))
       `(etypecase (aref ,m 0 0)))
    ;; create specialized versions for the supported float types
    ;; use rationals as a fallback
    ,@(iter (for type in (append +supported-floats+ (list 'rational)))
            (for key in (append +supported-floats+ (list 't)))
        (collect `(,key
                   (macrolet ((the-matrix-type (val)
                                `(the ',',type ,val)))
                     (flet ((set-matrix-entry (i j val)
                              (setf (aref ,m i j) (coerce val ',type)))
                            (coerce-matrix-type (val)
                              (coerce val ',type)))
                       (declare (type (simple-array ,type 2) ,m)
                                (inline set-matrix-entry coerce-matrix-type)
                                (ignore (function set-matrix-entry) (function coerce-matrix-type)))
                       ,@body)))))))

(declaim (inline specialization-type-of))
(defun specialization-type-of (x)
  "Gets the specialization type to use for `x`."
  (or (when (rationalp x)
        'rational)
      (type-of x)))


;;; Tableau representation

(defstruct (tableau (:copier #:shallow-tableau-copy))
  "Contains the necessary information for a simplex tableau."
  ; note that two tableaus are stored, which may differ when solving integer problems
  (problem nil :read-only t :type problem) ; the overall problem
  (instance-problem nil :read-only t :type problem) ; the problem for this specific tableau
  (matrix #2A() :read-only t :type (or (simple-array rational 2)
                                       (simple-array long-float 2)
                                       (simple-array double-float 2)
                                       (simple-array single-float 2)
                                       (simple-array short-float 2)))
  (basis-columns #() :read-only t :type (simple-array fixnum (*)))
  (var-count 0 :read-only t :type (and fixnum unsigned-byte))
  (constraint-count 0 :read-only t :type (and fixnum unsigned-byte)))

(declaim (inline copy-tableau))
(defun copy-tableau (tableau)
  "Copies the given tableau and it's matrix."
  (declare (optimize (speed 3)))
  (make-tableau :problem (tableau-problem tableau)
                :instance-problem (tableau-instance-problem tableau)
                :matrix (copy-array (tableau-matrix tableau))
                :basis-columns (copy-array (tableau-basis-columns tableau))
                :var-count (tableau-var-count tableau)
                :constraint-count (tableau-constraint-count tableau)))

(declaim (inline tableau-objective-value))
(defun tableau-objective-value (tableau)
  "Gets the objective function value in the tableau."
  (aref (tableau-matrix tableau)
        (tableau-constraint-count tableau)
        (tableau-var-count tableau)))

(declaim (inline tableau-variable))
(defun tableau-variable (tableau var)
  "Gets the value of the given variable from the tableau"
  (let ((problem (tableau-instance-problem tableau)))
    (if (eq var (problem-objective-var problem))
      (tableau-objective-value tableau)
      (let* ((var-id (position var (problem-vars problem)))
             (idx (position var-id (tableau-basis-columns tableau))))
        (cond
          ((null var-id) (error "~S is not a variable in the tableau" var))
          (idx (aref (tableau-matrix tableau)
                     idx (tableau-var-count tableau)))
          (t 0))))))


(declaim (inline tableau-shadow-price))
(defun tableau-shadow-price (tableau var)
  "Gets the shadow price for the given variable from the tableau"
  (if-let (idx (position var (problem-vars (tableau-instance-problem tableau))))
    (aref (tableau-matrix tableau)
          (tableau-constraint-count tableau) idx)
    (error "~S is not a variable in the tableau" var)))


(defun build-tableau (problem &optional (instance-problem problem))
  "Creates the tableau from the given linear problem.  If the trivial basis is not
feasible, instead a list is returned containing the two tableaus for a two-phase
simplex method."
  (let* ((num-constraints (length (problem-constraints instance-problem)))
         (num-slack (count-if-not (curry #'eq '=) (problem-constraints instance-problem) :key #'first))
         (vars (problem-vars problem))
         (num-vars (length vars))
         (num-cols (+ num-vars num-slack 1))
         (real-type (float-contagion (reduce 'float-contagion (problem-objective-func problem)
                                             :key (compose 'specialization-type-of #'cdr))
                                     (reduce 'float-contagion (problem-constraints problem)
                                             :key (lambda (constraint)
                                                    (reduce 'float-contagion (second constraint)
                                                            :key (compose 'specialization-type-of #'cdr))))))
         (matrix (make-array (list (1+ num-constraints) num-cols)
                            :element-type real-type
                            :initial-element 0))
         (basis-columns (make-array (list num-constraints)
                                    :element-type 'fixnum))
         (artificial-var-rows nil))
    (specialize-tableau-matrix matrix
      ; constraint rows
      (iter (declare (iterate:declare-variables)
                     (optimize (speed 3) (safety 0)))
            (for (the fixnum row) from 0 below num-constraints)
            (for constraint in (problem-constraints instance-problem))
        ;variables
        (iter (declare (iterate:declare-variables))
              (for (the fixnum col) from 0 below num-vars)
              (for var = (aref vars col))
          (when-let (value (cdr (assoc var (second constraint) :test #'eq)))
            (set-matrix-entry row col value)))
        ;slack
        (case (first constraint)
          (<= (set-matrix-entry row (+ num-vars row) 1)
              (setf (aref basis-columns row) (+ num-vars row)))
          (>= (push row artificial-var-rows)
              (set-matrix-entry row (+ num-vars row) -1)
              (setf (aref basis-columns row) (+ num-vars num-slack)))
          (= (push row artificial-var-rows)
             (setf (aref basis-columns row) (+ num-vars num-slack)))
          (t (error 'parsing-error
                    :description (format nil "~S is not a valid constraint equation" constraint))))
        ;rhs
        (set-matrix-entry row (+ num-vars num-slack) (third constraint)))
      ;objective row
      (iter (declare (iterate:declare-variables)
                     (optimize (speed 3) (safety 0)))
            (for (the fixnum col) from 0 below num-vars)
            (for var = (aref vars col))
        (when-let (value (cdr (assoc var (problem-objective-func problem) :test #'eq)))
          (set-matrix-entry num-constraints col (- (coerce-matrix-type value)))))
      (let ((main-tableau (make-tableau :problem problem
                                        :instance-problem instance-problem
                                        :matrix matrix
                                        :basis-columns basis-columns
                                        :var-count (+ num-vars num-slack)
                                        :constraint-count num-constraints))
            (art-tableau (when artificial-var-rows
                           (let* ((num-art (length artificial-var-rows))
                                  (art-matrix (make-array (list (1+ num-constraints) (+ num-vars num-slack num-art 1))
                                                          :element-type real-type
                                                          :initial-element 0))
                                  (art-basis-columns (copy-array basis-columns)))
                             (progn
                               (iter (for row in artificial-var-rows)
                                     (for i from 0)
                                 (setf (aref art-basis-columns row)
                                       (+ num-vars num-slack i))
                                 (setf (aref art-matrix row (+ num-vars num-slack i)) 1))

                               ;copy coefficients
                               (iter (for c from 0 below (+ num-vars num-slack))
                                 (setf (aref art-matrix num-constraints c)
                                       (iter (for r from 0 below num-constraints)
                                          (setf (aref art-matrix r c) (aref matrix r c))
                                          (when (member r artificial-var-rows)
                                            (sum (aref art-matrix r c))))))
                               ;copy rhs
                               (let ((c (+ num-vars num-slack num-art)))
                                 (setf (aref art-matrix num-constraints c)
                                       (iter (for r from 0 below num-constraints)
                                         (setf (aref art-matrix r c)
                                               (aref matrix r (+ num-vars num-slack)))
                                         (when (member r artificial-var-rows)
                                           (sum (aref art-matrix r c)))))))
                             (make-tableau :problem problem
                                           :instance-problem (linear-programming/problem::make-problem
                                                                           :type 'min ;artificial problem
                                                                           :vars (problem-vars problem))
                                           :matrix art-matrix
                                           :basis-columns art-basis-columns
                                           :var-count (+ num-vars num-slack num-art)
                                           :constraint-count num-constraints)))))
        (if art-tableau
         (list art-tableau main-tableau)
         main-tableau)))))


;;; Tableau solver

(defun pivot-row (tableau entering-col changing-row)
  "Non-destructively applies a single pivot to the table."
  (n-pivot-row (copy-tableau tableau) entering-col changing-row))

(defun n-pivot-row (tableau entering-col changing-row)
  "Destructively applies a single pivot to the table."
  (declare (type fixnum entering-col changing-row)
           (optimize (speed 3)))
  (let* ((matrix (tableau-matrix tableau))
         (row-count (array-dimension matrix 0))
         (col-count (array-dimension matrix 1)))
    (specialize-tableau-matrix matrix
      (let ((row-scale (aref matrix changing-row entering-col)))
        (iter (declare (iterate:declare-variables))
              (for (the fixnum c) from 0 below col-count)
          (setf (aref matrix changing-row  c) (/ (aref matrix changing-row c) row-scale))))
      (iter (declare (iterate:declare-variables)
                     (optimize (speed 3) (safety 0)))
            (for (the fixnum r) from 0 below row-count)
        (unless (= r changing-row)
          (let ((scale (aref matrix r entering-col)))
            (iter (declare (iterate:declare-variables))
                  (for (the fixnum c) from 0 below col-count)
              (decf (aref matrix r c) (* scale (aref matrix changing-row c)))))))))
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
          (return (when (< (aref (tableau-matrix tableau) num-constraints col) 0)
                    col))))
      (iter (for i from 0 below (tableau-var-count tableau))
        (finding i maximizing (aref (tableau-matrix tableau) num-constraints i)
                   into col)
        (finally
          (return (when (> (aref (tableau-matrix tableau) num-constraints col) 0)
                    col)))))))

(declaim (inline find-pivoting-row))
(defun find-pivoting-row (tableau entering-col)
  "Gets the column that will leave the basis."
  (declare (optimize (speed 3) (safety 0)))
  (let ((matrix (tableau-matrix tableau)))
    (specialize-tableau-matrix matrix
      (loop :with ratio = (aref matrix 0 0) ; an arbeitrary value of the specialization type
            :with result = -1
            :for i fixnum :from 0 :below (tableau-constraint-count tableau)
        :do (when (< 0 (aref matrix i entering-col))
              (let ((temp (/ (aref matrix i (tableau-var-count tableau))
                             (aref matrix i entering-col))))
                (when (or (= -1 result)
                          (< temp ratio))
                  (setf ratio temp
                        result i))))
        :finally (return (when (/= -1 result)
                           result))))))


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
       (unless (= 0 (tableau-objective-value solved-art-tab))
         (error 'infeasible-problem-error))

       ; Have starting basis, use solve-art-tab to set main-tab to that basis
       (let ((main-matrix (tableau-matrix main-tab))
             (art-matrix (tableau-matrix solved-art-tab))
             (num-vars (tableau-var-count main-tab))
             (num-constraints (tableau-constraint-count main-tab)))
         ; Copy tableau coefficients/RHS
         (iter (for row from 0 below num-constraints)
           (iter (for col from 0 below num-vars)
             (setf (aref main-matrix row col) (aref art-matrix row col)))
           (setf (aref main-matrix row num-vars)
                 (aref art-matrix row (tableau-var-count solved-art-tab))))

         ; Update basis columns and objective row to match
         (iter (for basis-col in-vector (tableau-basis-columns solved-art-tab))
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


(defun build-and-solve (problem extra-constraints)
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
                        :constraints (append extra-constraints
                                             (problem-constraints problem))))))
    (infeasible-problem-error () :infeasible)))



(defun simplex-solver (problem)
  "The solver interface function for the simplex backend."
  (let ((current-best nil)
        (current-solution nil)
        (stack (list '()))
        (comparator (if (eq (problem-type problem) 'max) '< '>)))
    (iter (while stack)
      (let* ((entry (pop stack))
             (tab (build-and-solve problem entry)))
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
    ; return the tableau with the solution
    current-solution))
