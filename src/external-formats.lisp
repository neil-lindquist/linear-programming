
(uiop:define-package :linear-programming/external-formats
  (:use :cl
        :iterate
        :linear-programming/problem)
  (:import-from :alexandria
                #:curry
                #:if-let
                #:ensure-gethash)
  (:import-from :linear-programming/expressions
                #:scale-linear-expression
                #:format-linear-expression)
  (:import-from :linear-programming/utils
                #:validate-bounds
                #:lb-max
                #:ub-min)
  (:export #:read-sexp
           #:write-sexp
           #:read-mps)
  (:documentation "Handles reading and writing problems to external formats."))

(in-package :linear-programming/external-formats)

;;; utils
(defun read-until (stream test &optional (preview-test (constantly t)))
  "Reads the given stream until the condition is true.  `test` can either be a
predicate that takes one argument, or a character to be tested using `char=`."
  (let ((test (if (characterp test) (curry 'char= test) test))
        (preview-test (if (characterp test) (curry 'char= preview-test) preview-test)))
    (with-output-to-string (result)
      (iter (for c = (peek-char nil stream nil nil))
            (while (and c (not (and (funcall test c)
                                    (funcall preview-test (peek-char nil stream nil nil))))))
         (write-char (read-char stream) result)))))

(defun newlinep (c)
  "Predicate if the given character is a newline or return"
  (or (char= c #\newline)
      (char= c #\return)))

;;; reader and writer functions

(defun read-sexp (stream &key allow-read-eval package)
  "Loads a problem stored in sexp format.  This is a single sexp with the first
element being the objective function and the rest of the elements being the
constraints.  Note that normally `*read-eval*` is bound to `nil`, but can be
enabled with `allow-read-eval`; however, this should only be done when
parsing trusted data."
  (let* ((problem (with-standard-io-syntax
                    (let ((*read-eval* allow-read-eval)
                          (*package* (or (find-package package) *package*)))
                      (read stream)))))
    (parse-linear-problem (first problem) (rest problem))))

(defun write-sexp (stream problem &key package)
  "Writes the problem as a sexp.  The first element is the objective function and
the rest of the elements are the constraints."
  (let* ((objective-func (let ((objective `(,(problem-type problem)
                                             ,(format-linear-expression (problem-objective-func problem)))))
                           (if (symbol-package (problem-objective-var problem)) ; is non uninterned
                             `(= ,(problem-objective-var problem) objective)
                             objective)))
         (eq-constraints (iter (for constraint in (problem-constraints problem))
                           (collect (list (first constraint)
                                          (format-linear-expression (second constraint))
                                          (third constraint)))))
         (int-vars (problem-integer-vars problem))
         (bounds (problem-var-bounds problem))
         (constraints (append (when int-vars (list (list* 'integer int-vars)))
                              (when bounds (list (list* 'bounds bounds)))
                              eq-constraints))
         (problem-sexp (cons objective-func constraints)))
    (with-standard-io-syntax
      (let ((*package* (or (find-package package) *package*)))
        (format stream "~S~%" problem-sexp)))))

(defun read-mps (stream problem-type &key package (read-case (readtable-case *readtable*)) (trim-names-p t) (number-type 'rational) rhs-id)
  ;; Currently using fixed width format
  ;; http://lpsolve.sourceforge.net/5.1/mps-format.htm
  ;; http://cgm.cs.mcgill.ca/~avis/courses/567/cplex/reffileformatscplex.pdf
  ;; TODO consider adding support for free-format MPS files
  (let ((package (or (find-package package) *package*))
        (current-header nil)
        (problem-name nil)
        (rows (make-hash-table :test 'equal)) ; table mapping row ID's to (list* row-type rhs range-type linexp)
        (objective nil) ; name
        (var-info (make-hash-table :test 'eq)))
    (labels ((substring (str &optional (start 0) (end (length str)))
              "As `(subseq str start (min (length str) end))``"
              (subseq str (min (length str) start) (min (length str) end)))
             (field (n line &optional (field-type :raw))
               ;; Gets the `n`th field of the line. Note that `n` is 1-based, with `n=0`
               ;; representing the entire line, excluding the first character.
               (let ((raw (substring line
                                     (aref #(0 1 4 14 24 39 49) n)
                                     (aref #(61 3 12 22 36 47 61) n))))
                 (ecase field-type
                   ;; trim and case-ify names then turn them into symbols in package
                   (:symbol
                     (let ((raw (if trim-names-p (string-trim " " raw) raw)))
                       (intern
                         (ecase read-case
                           (:upcase (string-upcase raw))
                           (:downcase (string-downcase raw))
                           (:preserve raw)
                           (:invert (cond
                                      ((every #'upper-case-p raw) (string-downcase raw))
                                      ((every #'lower-case-p raw) (string-upcase raw))
                                      (t raw))))
                         package)))
                   ;; processes the string like it's a symbol, but doesn't intern it
                   (:name-string
                     (let ((raw (if trim-names-p (string-trim " " raw) raw)))
                       (ecase read-case
                         (:upcase (string-upcase raw))
                         (:downcase (string-downcase raw))
                         (:preserve raw)
                         (:invert (cond
                                    ((every #'upper-case-p raw) (string-downcase raw))
                                    ((every #'lower-case-p raw) (string-upcase raw))
                                    (t raw))))))
                   ;; Parse numbers of the form
                   (:number
                    (let* ((raw (string-trim " " raw))
                           (sign (if (char= #\- (aref raw 0)) -1 1))
                           (position (if (or (char= #\- (aref raw 0)) (char= #\+ (aref raw 0)))
                                       1 0))
                           (value (coerce 0 number-type)))
                      (iter (while (< position (length raw)))
                            (for c = (digit-char-p (aref raw position)))
                            (while c)
                        (setf value (+ c (* value 10)))
                        (incf position))
                      (when (and (< position (length raw))
                                 (char= #\. (aref raw position)))
                        (incf position)
                        (let ((fraction 0)
                              (digits 0))
                          (iter (while (< position (length raw)))
                                (for c = (digit-char-p (aref raw position)))
                                (while c)
                            (setf fraction (+ (* c (expt 10 digits)) fraction))
                            (incf digits)
                            (incf position))
                          (incf value (/ fraction (expt 10 digits)))))
                      (when (and (< position (length raw))
                                 (or (char-equal #\d (aref raw position))
                                     (char-equal #\e (aref raw position))))
                        (let ((exp-sign (if (char= #\- (aref raw 0)) -1 1))
                              (exp 0))
                          (when (or (char= #\- (aref raw 0)) (char= #\+ (aref raw 0)))
                            (incf position))
                          (iter (while (< position (length raw)))
                                (for c = (digit-char-p (aref raw position)))
                                (while c)
                            (setf value (+ c (* exp 10)))
                            (incf position))
                          (setf value (* value (expt 10 (* exp-sign exp))))))
                      (* sign value)))
                   ;; if it's not a name, just return it
                   (:raw
                     raw)))))

      (iter (for line = (read-line stream))
        (if-let (header-card (unless (char= #\space (aref line 0))
                               (string-downcase (string-right-trim " " (substring line 0 15)))))
          (cond
            ;; do nothing on comments
            ((char= (aref header-card 0) #\*))
            ;; Take the name of the problem. Note the lack of body section
            ((string= header-card "name")
             (setf problem-name (field 3 line)))
            ;; The end of the problem. Exit the loop
            ((string= header-card "endata")
             (return))
            ;; Normal section. Mark how to process the body sections
            (t
             (setf current-header header-card)))
          (cond ; else in a section
            ;; Name a row and set the type
            ((string= current-header "rows")
             (let ((row-type (case (char-downcase (aref (field 1 line) 0))
                               (#\n 'objective)
                               (#\g '>=)
                               (#\l '<=)
                               (#\ '=)))
                   (row-name (field 2 line :name-string)))
               (when (and (eq 'objective row-type)
                          (not objective))
                 ;; by default, the first `N` row is the objective function
                 (setf objective row-name))
               (setf (gethash row-name rows)
                     (list row-type 0 nil))))
            ;; Set the coefficients for a variable in up to 2 rows
            ((string= current-header "columns")
             (let ((var-name (field 2 line :symbol)))
               (ensure-gethash var-name var-info (list 0 nil nil))
               (let ((row-name (field 3 line :name-string))
                     (coefficient (field 4 line :number)))
                 (push (cons var-name coefficient)
                       (cdddr (gethash row-name rows))))
               (when (/= 0 (length (field 5 line)))
                 (let ((row-name (field 5 line :name-string))
                       (coefficient (field 6 line :number)))
                   (push (cons var-name coefficient)
                         (cdddr (gethash row-name rows)))))))
            ;; Set the values on the RHS of the rows
            ((string= current-header "rhs")
             (let ((current-rhs-id (field 2 line :name-string)))
               (unless rhs-id
                 (setf rhs-id current-rhs-id))
               (when (string= rhs-id current-rhs-id)
                 (let ((row-name (field 3 line :name-string))
                       (value (field 4 line :number)))
                   (setf (second (gethash row-name rows))
                         value))
                 (when (/= 0 (length (field 5 line)))
                   (let ((row-name (field 5 line :name-string))
                         (value (field 6 line :number)))
                     (setf (second (gethash row-name rows))
                           value))))))
            ;; Ranges turn single constraints into a range
            ((string= current-header "ranges")
             (let ((current-rhs-id (field 2 line :name-string)))
               (unless rhs-id
                 (setf rhs-id current-rhs-id))
               (setf (third (gethash (field 3 line :symbol) rows))
                     (field 4 line :number))
               (when (/= 0 (length (field 5 line)))
                 (setf (third (gethash (field 5 line :symbol) rows))
                       (field 6 line :number)))))
            ;; Sets bounds for specific variables & including an extension for integer variables
            ((string= current-header "bounds")
             (let* ((var (field 3 line :symbol))
                    ;; LB UB intp
                    (attrs (gethash var var-info (list 0 nil nil)))
                    (bound-type (field 1 line :name-string)))
               (setf (gethash var var-info)
                     (cond
                       ((string= bound-type "LO")
                        (list* (field 4 line :number) (cdr attrs)))
                       ((string= bound-type "UP")
                        (list* (second attrs) (field 4 line :number) (cddr attrs)))
                       ((string= bound-type "FX")
                        (let ((value (field 4 line :number)))
                          (list* value value (cddr attrs))))
                       ((string= bound-type "FR")
                        (list* nil nil (cddr attrs)))
                       ((string= bound-type "MI")
                        (list* nil (cdr attrs)))
                       ((string= bound-type "PL")
                        (list* (first attrs) nil (cddr attrs)))
                       ((string= bound-type "BV")
                        (list 0 1 t))
                       ((string= bound-type "LI")
                        (list (field 4 line :number) (second attrs) t))
                       ((string= bound-type "UI")
                        (list (first attrs) (field 4 line :number) t))
                       (t (error 'parsing-error :description (format nil "~S is not a know bound type" bound-type)))))))
            ;; extension for specifying the problem type
            ((string= current-header "objsense")
             (setf current-header nil) ; only one record for this header
             (labels ((process-problem-type (type)
                        (cond
                          ((or (string-equal type "max")
                               (string-equal type "maximizing")
                               (eq type 'max))
                           (setf problem-type 'max))
                          ((or (string-equal type "min")
                               (string-equal type "minimizing")
                               (eq type 'min))
                           (setf problem-type 'min))
                          (t
                           (restart-case (error 'parsing-error :description (format nil "~S is not a know problem type" type))
                             (continue ()
                               :report "Ignore the entry")
                             (use-value (type)
                               :report "Use specified value"
                               :interactive (lambda () (format t "Type a form to be evaluated: ") (eval (read)))
                               (process-problem-type type)))))))
               (process-problem-type (field 0 line :name-string))))
            ;; extension that supports selecting the objective function
            ((string= current-header "objname")
             (setf current-header nil ; only one record for this header
                   objective (field 0 line :name-string)))))))
    (let ((vars (make-array (list (hash-table-count var-info)) :element-type 'symbol
                                                               :initial-element nil))
          (int-vars nil)
          (bounds nil)
          (constraints nil)
          (objective-func (cdddr (gethash objective rows))))
      (iter (for (row content) in-hashtable rows)
            (for op = (first content))
        (unless (eq op 'objective)
          (push (list op (cdddr content) (second content))
                constraints)
          (when (third content)
            (push (cond
                    ((eq op '<=)
                     (list '>= (cdddr content) (- (second content) (abs (third content)))))
                    ((eq op '>=)
                     (list '<= (cdddr content) (+ (second content) (abs (third content)))))
                    ;; invariant: (eq op '=)
                    ((< 0 (third content))
                     (list '<= (cdddr content) (+ (second content) (third content))))
                    ((< (third content) 0)
                     (list '>= (cdddr content) (+ (second content) (third content)))))
                    ;; invariant: (and (eq op '=) (= (third content) 0))
                  constraints))))
      (iter (for remaining on constraints)
            (for c = (first remaining))
        (cond
          ;; simple bound
          ((= 1 (length (second c)))
           (let* ((var (caar (second c)))
                  (info (gethash var var-info))
                  (bound (/ (third c) (cdar (second c)))))
             (ecase (first c)
               (<= (setf (second info) (lb-max (second info) bound)))
               (>= (setf (third info)  (ub-min (third info)  bound)))
               (=  (setf (second info) (lb-max (second info) bound)
                         (third info)  (ub-min (third info)  bound))))
             (setf (car remaining) (cadr remaining) ; remove that constraint
                   (cdr remaining) (cddr remaining))))
          ;; negative rhs
          ((< (third c) 0)
           (setf (second c) (scale-linear-expression (second c) -1)
                 (third c) (- (third c))
                 (first c) (ecase (first c)
                             (<= '>=)
                             (>= '<=)
                             (=  '=))))))
      (iter (for (var info) in-hashtable var-info)
            (for i from 0)
        (setf (aref vars i) var)
        (when (third info)
          (push var int-vars))
        (when (or (not (equalp (first info) 0))
                  (not (null (second info))))
          (validate-bounds (first info) (second info) var)
          (push (cons var (cons (first info) (second info))) bounds)))
      (linear-programming/problem::make-problem :type problem-type
                                                :vars vars
                                                :objective-var (make-symbol objective)
                                                :objective-func objective-func
                                                :integer-vars int-vars
                                                :var-bounds bounds
                                                :constraints constraints))))
