
(uiop:define-package :linear-programming/external-formats
  (:use :cl
        :iterate
        :linear-programming/problem)
  (:import-from :linear-programming/expressions
                #:format-linear-expression)
  (:export #:read-sexp
           #:write-sexp)
  (:documentation "Handles reading and writing problems to external formats."))

(in-package :linear-programming/external-formats)

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
         (free-vars (problem-free-vars problem))
         (constraints (append (when int-vars (list (list* 'integer int-vars)))
                              (when free-vars (list (list* 'free free-vars)))
                              eq-constraints))
         (problem-sexp (cons objective-func constraints)))
    (with-standard-io-syntax
      (let ((*package* (or (find-package package) *package*)))
        (format stream "~S~%" problem-sexp)))))
