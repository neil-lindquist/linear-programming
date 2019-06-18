
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


(defun build-tableau (problem)
  "Creates the tableau from the given linear problem."
  (when (/= (length (non-neg-vars problem)) (length (variables problem)))
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

(defun find-entering-column (tableau)
  "Gets the column to add to the basis"
  (let ((num-constraints (tableau-constraint-count tableau)))
    (if (eq 'max (lp-type (tableau-problem tableau)))
      (iter (for i from 0 below (tableau-var-count tableau))
        (finding i minimizing (aref (tableau-matrix tableau) i num-constraints)
                  into col)
        (finally
          (return (if (< (aref (tableau-matrix tableau) col num-constraints) 0)
                    col
                    nil))))
      (iter (for i from 0 below (tableau-var-count tableau))
        (finding i maximizing (aref (tableau-matrix tableau) i num-constraints)
                   into col)
        (finally
          (return (if (> (aref (tableau-matrix tableau) col num-constraints) 0)
                    col
                    nil)))))))

(defun find-leaving-column (tableau entering-col)
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
    (pivot-row tableau entering-column (find-leaving-column tableau entering-column)))
  tableau)


(defmacro with-tableau-variables (var-list tableau &body body)
  "Evaluates the body with the variables in `var-list` bound to their values in
   the tableau.  If a linear problem is instead passed as `var-list`, all
   of the problem's variables are bound."
  (once-only (tableau)
    (if (typep var-list 'linear-problem)
      (let* ((vars (variables var-list))
             (num-vars (+ (length vars) (length (constraints var-list)))))
        `(let (,@(iter (for var in-vector vars)
                       (for i from 0)
                   (collect `(,var (if-let (idx (position ,i (tableau-basis-columns ,tableau)))
                                     (aref (tableau-matrix ,tableau) ,num-vars idx)
                                     0)))))
           ,@body))
      `(flet ((get-variable-value (var)
                 (if-let (idx (position (position var (variables (tableau-problem ,tableau)))
                                        (tableau-basis-columns ,tableau)))
                   (aref (tableau-matrix ,tableau)
                         (tableau-var-count ,tableau)
                         idx)
                   0)))
         (let (,@(iter (for var in-sequence var-list)
                   (collect `(,var (get-variable-value ',var)))))
           ,@body)))))
