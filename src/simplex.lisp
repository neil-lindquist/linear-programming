
(uiop:define-package :linear-programming/simplex
  (:use :cl
        :alexandria
        :iterate
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

           #:pivot-row

           #:build-tableau
           #:solve-tableau
           #:with-tableau-variables))

(in-package :linear-programming/simplex)

(defstruct tableau
  (problem nil :read-only t :type linear-problem)
  (matrix #2A() :read-only t :type (array real 2))
  (basis-columns #() :read-only t :type vector)
  (var-count 0 :read-only t :type (integer 0 *))
  (constraint-count 0 :read-only t :type (integer 0 *)))

(declaim (inline tableau-objective-value))
(defun tableau-objective-value (tableau)
  "Gets the objective function value in the tableau"
  (aref (tableau-matrix tableau)
        (tableau-var-count tableau)
        (tableau-constraint-count tableau)))

(defun build-tableau (problem)
  "Creates the tableau from the given linear problem."
  (when (/= 0 (length (signed-vars problem)))
    (error "Cannot currently handle possibly negative variables."))
  (let* ((num-slack (length (constraints problem)))
         (vars (variables problem))
         (num-vars (length vars))
         (matrix (make-array (list (+ num-vars num-slack 1) (1+ num-slack))
                            :element-type 'real
                            :initial-element 0))
         (basis-columns (make-array (list num-slack) :element-type `(integer 0 ,(+ num-vars num-slack 1)))))
    ; constraint rows
    (iter (for row from 0 below num-slack)
          (for constraint in (constraints problem))
      (when (not (eq '<= (first constraint)))
        (error "~S is not an <= constraint" constraint))
      ;variables
      (iter (for col from 0 below num-vars)
            (for var = (aref vars col))
        (when-let (value (cdr (assoc var (second constraint))))
          (setf (aref matrix col row) value)))
      ;slack
      (setf (aref matrix (+ num-vars row) row) 1
            (aref basis-columns row) (+ num-vars row))
      ;rhs
      (setf (aref matrix (+ num-vars num-slack) row) (third constraint)))
    ;objective row
    (iter (for col from 0 below num-vars)
          (for var = (aref vars col))
      (when-let (value (cdr (assoc var (objective-function problem))))
        (setf (aref matrix col num-slack) (- value))))
    (make-tableau :problem problem
                  :matrix matrix
                  :basis-columns basis-columns
                  :var-count (+ num-vars num-slack)
                  :constraint-count num-slack)))

(defun pivot-row (tableau entering-col changing-row)
  "Applies a single pivot to the table."
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
  "Attempts to solve the tableau using the simplex method."
  (iter (for entering-column = (find-entering-column tableau))
        (while entering-column)
    (let ((pivoting-row (find-pivoting-row tableau entering-column)))
      (unless pivoting-row
        (error "Cannot find valid row to pivot.  Problem is likely unbound."))
      (pivot-row tableau entering-column pivoting-row)))
  tableau)

(declaim (inline get-tableau-variable))
(defun get-tableau-variable (var tableau)
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
                 (collect `(,var (get-tableau-variable ',var ,tableau)))))
         ,@body))))