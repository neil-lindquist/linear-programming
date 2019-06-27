
(uiop:define-package :linear-programming/simplex
  (:use :cl
        :alexandria
        :iterate
        :linear-programming/conditions
        :linear-programming/problem)
  (:export #:tableau
           #:tableau-p
           #:copy-tableau
           #:tableau-problem
           #:tableau-matrix
           #:tableau-basis-columns
           #:tableau-var-count
           #:tableau-constraint-count
           #:tableau-objective-value
           #:tableau-variable
           #:tableau-shadow-price

           #:n-pivot-row

           #:build-tableau
           #:solve-tableau
           #:n-solve-tableau
           #:with-tableau-variables))

(in-package :linear-programming/simplex)

(defstruct (tableau (:copier #:shallow-tableau-copy))
  (problem nil :read-only t :type (or linear-problem artificial-linear-problem))
  (matrix #2A() :read-only t :type (simple-array real 2))
  (basis-columns #() :read-only t :type (simple-array * (*)))
  (var-count 0 :read-only t :type (integer 0 *))
  (constraint-count 0 :read-only t :type (integer 0 *)))

(declaim (inline tableau-objective-value))
(defun copy-tableau (tableau)
  "Copies the given tableau and it's matrix"
  (declare (optimize (speed 3)))
  (make-tableau :problem (tableau-problem tableau)
                :matrix (copy-array (tableau-matrix tableau))
                :basis-columns (copy-array (tableau-basis-columns tableau))
                :var-count (tableau-var-count tableau)
                :constraint-count (tableau-constraint-count tableau)))

(declaim (inline tableau-objective-value))
(defun tableau-objective-value (tableau)
  "Gets the objective function value in the tableau"
  (aref (tableau-matrix tableau)
        (tableau-var-count tableau)
        (tableau-constraint-count tableau)))


(defclass artificial-linear-problem ()
  ((problem :reader base-problem
          :initarg :base-problem
          :type linear-problem
          :documentation "The underlying problem"))
  (:documentation "Represents the linear problem when artificial variables are
                  present when using a two-phase simplex."))
(defmethod lp-type ((problem artificial-linear-problem))
  (declare (ignore problem))
  'min)
(defmethod variables ((problem artificial-linear-problem))
  (variables (base-problem problem)))


(defun build-tableau (problem)
  "Creates the tableau from the given linear problem.  If the trivial basis is
   not feasible, instead a list is returned containing the two tableaus for a
   two-phase simplex method."
  (when (/= 0 (length (signed-vars problem)))
    (error "Cannot currently handle possibly negative variables."))
  (let* ((num-constraints (length (constraints problem)))
         (num-slack (count-if-not (curry #'eq '=) (constraints problem) :key #'first))
         (vars (variables problem))
         (num-vars (length vars))
         (matrix (make-array (list (+ num-vars num-slack 1) (1+ num-constraints))
                            :element-type 'real
                            :initial-element 0))
         (basis-columns (make-array (list num-constraints) :element-type `(integer 0 ,(+ num-vars num-slack 1))))
         (artificial-var-rows nil))
    ; constraint rows
    (iter (for row from 0 below num-constraints)
          (for constraint in (constraints problem))
      ;variables
      (iter (for col from 0 below num-vars)
            (for var = (aref vars col))
        (when-let (value (cdr (assoc var (second constraint))))
          (setf (aref matrix col row) value)))
      ;slack
      (case (first constraint)
        (<= (setf (aref matrix (+ num-vars row) row) 1
                  (aref basis-columns row) (+ num-vars row)))
        (>= (push row artificial-var-rows)
            (setf (aref matrix (+ num-vars row) row) -1
                  (aref basis-columns row) (+ num-vars num-slack)))
        (= (push row artificial-var-rows)
           (setf (aref basis-columns row) (+ num-vars num-slack)))
        (t (error 'parsing-error
                  :description (format nil "~S is not a valid constraint equation" constraint))))
      ;rhs
      (setf (aref matrix (+ num-vars num-slack) row) (third constraint)))
    ;objective row
    (iter (for col from 0 below num-vars)
          (for var = (aref vars col))
      (when-let (value (cdr (assoc var (objective-function problem))))
        (setf (aref matrix col num-constraints) (- value))))
    (let ((main-tableau (make-tableau :problem problem
                                      :matrix matrix
                                      :basis-columns basis-columns
                                      :var-count (+ num-vars num-slack)
                                      :constraint-count num-constraints))
          (art-tableau (when artificial-var-rows
                         (let* ((num-art (length artificial-var-rows))
                                (art-matrix (make-array (list (+ num-vars num-slack num-art 1) (1+ num-constraints))
                                                        :element-type 'real
                                                        :initial-element 0))
                                (art-basis-columns (copy-array basis-columns)))
                           (iter (for row in artificial-var-rows)
                                 (for i from 0)
                             (setf (aref art-basis-columns row)
                                   (+ num-vars num-slack i))
                             (setf (aref art-matrix (+ num-vars num-slack i) row) 1))

                           ;copy coefficients
                           (iter (for c from 0 below (+ num-vars num-slack))
                             (setf (aref art-matrix c num-constraints)
                                   (iter (for r from 0 below num-constraints)
                                      (setf (aref art-matrix c r) (aref matrix c r))
                                      (when (member r artificial-var-rows)
                                        (sum (aref art-matrix c r))))))
                           ;copy rhs
                           (let ((c (+ num-vars num-slack num-art)))
                             (setf (aref art-matrix c num-constraints)
                                   (iter (for r from 0 below num-constraints)
                                     (setf (aref art-matrix c r)
                                           (aref matrix (+ num-vars num-slack) r))
                                     (when (member r artificial-var-rows)
                                       (sum (aref art-matrix c r))))))
                           (make-tableau :problem (make-instance 'artificial-linear-problem
                                                                 :base-problem problem)
                                         :matrix art-matrix
                                         :basis-columns art-basis-columns
                                         :var-count (+ num-vars num-slack num-art)
                                         :constraint-count num-constraints)))))
      (if art-tableau
        (list art-tableau main-tableau)
        main-tableau))))


(defun pivot-row (tableau entering-col changing-row)
  "Non-destructively applies a single pivot to the table."
  (n-pivot-row (copy-tableau tableau) entering-col changing-row))

(defun n-pivot-row (tableau entering-col changing-row)
  "Destructively applies a single pivot to the table."
  (declare (type unsigned-byte entering-col changing-row))
  (let* ((matrix (tableau-matrix tableau))
         (col-count (array-dimension matrix 0))
         (row-count (array-dimension matrix 1)))
    (let ((row-scale (aref matrix entering-col changing-row)))
      (iter (for c from 0 below col-count)
        (setf (aref matrix c changing-row) (/ (aref matrix c changing-row) row-scale))))
    (iter (for r from 0 below row-count)
      (unless (= r changing-row)
        (let ((scale (aref matrix entering-col r)))
          (iter (for c from 0 below col-count)
            (setf (aref matrix c r)
                  (- (aref matrix c r) (* scale (aref matrix c changing-row)))))))))
  (setf (aref (tableau-basis-columns tableau) changing-row) entering-col)
  tableau)

(declaim (inline find-entering-column))
(defun find-entering-column (tableau)
  "Gets the column to add to the basis"
  (let ((num-constraints (tableau-constraint-count tableau)))
    (if (eq 'max (lp-type (tableau-problem tableau)))
      (iter (for i from 0 below (tableau-var-count tableau))
        (finding i minimizing (aref (tableau-matrix tableau) i num-constraints)
                  into col)
        (finally
          (return (when (< (aref (tableau-matrix tableau) col num-constraints) 0)
                    col))))
      (iter (for i from 0 below (tableau-var-count tableau))
        (finding i maximizing (aref (tableau-matrix tableau) i num-constraints)
                   into col)
        (finally
          (return (when (> (aref (tableau-matrix tableau) col num-constraints) 0)
                    col)))))))

(declaim (inline find-pivoting-row))
(defun find-pivoting-row (tableau entering-col)
  "Gets the column that will leave the basis"
  (let ((matrix (tableau-matrix tableau)))
    (iter (for i from 0 below (tableau-constraint-count tableau))
      (when (< 0 (aref matrix entering-col i))
        (finding i minimizing (/ (aref matrix (tableau-var-count tableau) i)
                                 (aref matrix entering-col i)))))))

(defun solve-tableau (tableau)
  "Attempts to solve the tableau using the simplex method.  If a list of two
   tableaus is given, then a two-phase version is used.
   The original tableau is unchanged"
  (if (listp tableau)
    (n-solve-tableau (mapcar #'copy-tableau tableau))
    (n-solve-tableau (copy-tableau tableau))))

(defun n-solve-tableau (tableau)
  "A non-consing version of `solve-tableau`."
  (cond
    ((listp tableau)
     (let ((solved-art-tab (n-solve-tableau (first tableau)))
           (main-tab (second tableau)))
       (unless (= 0 (tableau-objective-value solved-art-tab))
         (error 'infeasible-problem-error))
       (iter (for i from 0 below (length (tableau-basis-columns solved-art-tab)))
         (when (/= (aref (tableau-basis-columns solved-art-tab) i)
                   (aref (tableau-basis-columns main-tab) i))
           (n-pivot-row main-tab (aref (tableau-basis-columns solved-art-tab) i) i)))
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

(declaim (inline tableau-variable))
(defun tableau-variable (var tableau)
  "Gets the value of the given variable from the tableau"
  (let* ((problem (tableau-problem tableau))
         (objective-var (objective-variable problem)))
    (if (eq var objective-var)
      (tableau-objective-value tableau)
      (if-let (idx (position (position var (variables problem))
                             (tableau-basis-columns tableau)))
        (aref (tableau-matrix tableau)
              (tableau-var-count tableau)
              idx)
        0))))


(declaim (inline tableau-shadow-price))
(defun tableau-shadow-price (var tableau)
  "Gets the shadow price for the given variable from the tableau"
  (if-let (idx (position var (variables (tableau-problem tableau))))
    (aref (tableau-matrix tableau)
          idx (tableau-constraint-count tableau))
    (error "~S is not a variable in the tableau" var)))



(defmacro with-tableau-variables (var-list tableau &body body)
  "Evaluates the body with the variables in `var-list` bound to their values in
   the tableau.  If a linear problem is instead passed as `var-list`, all
   of the problem's variables are bound."
  (once-only (tableau)
    (if (typep var-list 'linear-problem)
      (let* ((problem var-list) ;alias for readability
             (vars (variables problem))
             (num-vars (+ (length vars) (length (constraints problem)))))
        `(let ((,(objective-variable problem) (tableau-objective-value ,tableau))
               ,@(iter (for var in-vector vars)
                       (for i from 0)
                   (collect `(,var (if-let (idx (position ,i (tableau-basis-columns ,tableau)))
                                       (aref (tableau-matrix ,tableau) ,num-vars idx)
                                       0)))))
           (declare (ignorable ,(objective-variable problem) ,@(map 'list #'identity vars)))
           ,@body))
      `(let (,@(iter (for var in-sequence var-list)
                 (collect `(,var (tableau-variable ',var ,tableau)))))
         ,@body))))
