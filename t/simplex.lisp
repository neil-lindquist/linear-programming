
(uiop:define-package :linear-programming-test/simplex
  (:use :cl
        :iterate
        :fiveam
        :linear-programming-test/base
        :linear-programming/conditions
        :linear-programming/problem
        :linear-programming/simplex)
  (:export #:simplex))

(in-package :linear-programming-test/simplex)


(defun tableau-matrix-equal (exp-mat exp-vars act-tab)
  (let* ((act-mat (tableau-matrix act-tab))
         (act-vars (problem-vars (tableau-problem act-tab))))
    (if (equalp exp-vars act-vars)
      (equalp exp-mat act-mat)
      (and
        (iter outer
              (for var in-vector act-vars)
              (for j from 0)
          (iter (for i from 0 to (tableau-constraint-count act-tab))
            (in outer (always (= (aref exp-mat i (position var exp-vars))
                                 (aref act-mat i j))))))
        (iter outer
              (for j from (length act-vars) to (tableau-var-count act-tab))
          (iter (for i from 0 to (tableau-constraint-count act-tab))
            (in outer (always (= (aref exp-mat i j)
                                 (aref act-mat i j))))))))))

(defun vars-to-cols (vars tab)
  (map 'vector (lambda (var)
                 (if (symbolp var)
                   (position var (problem-vars (tableau-problem tab)))
                   var))
               vars))


(def-suite simplex
  :in linear-programming
  :description "The suite to test linear-programming/simplex")
(in-suite simplex)

(test build-tableau
  (declare (notinline tableau-objective-value)) ; for coverage purposes

  (let* ((problem (linear-programming/problem::make-problem
                     :type 'max
                     :vars #(x y)
                     :objective-var '#:z
                     :objective-func '((x . 1) (y . 2))
                     :integer-vars '()
                     :constraints '((<= ((x . 5) (y . 1)) 10)
                                    (<= ((x . 1) (y . 7)) 18)
                                    (/= ((x . 1) (y . 1)) 5)))))
    (signals parsing-error (build-tableau problem problem)))

  (let* ((problem (make-linear-problem (max (+ x (* 4 y) (* 3 z)))
                                       (<= (+ (* 2 x) y) 8)
                                       (<= (+ y z) 7)))
         (tableau (build-tableau problem problem)))
    (is-true (tableau-p tableau))
    (is (eq problem (tableau-problem tableau)))
    (is (= 5 (tableau-var-count tableau)))
    (is (= 2 (tableau-constraint-count tableau)))
    (is (tableau-matrix-equal #2A((2 1 0 1 0 8) (0 1 1 0 1 7) (-1 -4 -3 0 0 0))
                              #(x y z)
                              tableau))
    (is (equalp (vars-to-cols #(3 4) tableau) (tableau-basis-columns tableau)))
    (is (= 0 (tableau-objective-value tableau))))

  (let* ((problem (make-linear-problem (max (+ x (* 4 y) (* 3 z)))
                                       (<= (+ (* 2 x) y) 8)
                                       (<= (+ y z) 7)
                                       (= (+ (* 2 x) y z) 8)))
         (tableaus (build-tableau problem problem))
         (art-tableau (first tableaus))
         (main-tableau (second tableaus)))
    (is (= 2 (length tableaus)))
    ; art-tableau
    (is-true (tableau-p art-tableau))
    (is (eq problem (tableau-problem art-tableau)))
    (is (eq 'min (problem-type (tableau-instance-problem art-tableau))))
    (is (= 6 (tableau-var-count art-tableau)))
    (is (= 3 (tableau-constraint-count art-tableau)))
    (is (tableau-matrix-equal #2A((2 1 0 1 0 0 8) (0 1 1 0 1 0 7) (2 1 1 0 0 1 8) (2 1 1 0 0 0 8))
                              #(x y z)
                              art-tableau))
    (is (equalp (vars-to-cols #(3 4 5) art-tableau) (tableau-basis-columns art-tableau)))
    (is (= 8 (tableau-objective-value art-tableau)))
    ; main-tableau
    (is-true (tableau-p main-tableau))
    (is (eq problem (tableau-problem main-tableau)))
    (is (eq problem (tableau-instance-problem main-tableau)))
    (is (= 5 (tableau-var-count main-tableau)))
    (is (= 3 (tableau-constraint-count main-tableau)))
    (is (tableau-matrix-equal #2A((2 1 0 1 0 8) (0 1 1 0 1 7) (2 1 1 0 0 8) (-1 -4 -3 0 0 0))
                              #(x y z)
                              main-tableau))
    (is (equalp #(3 4 6) (tableau-basis-columns main-tableau)))
    (is (= 0 (tableau-objective-value main-tableau))))

  (let* ((problem (make-linear-problem (max (+ x (* 4 y) (* 3 z)))
                                       (<= (+ (* 2 x) y) 8)
                                       (<= (+ y z) 7)
                                       (>= (+ x z) 1)))
         (tableaus (build-tableau problem problem))
         (art-tableau (first tableaus))
         (main-tableau (second tableaus)))
    (is (= 2 (length tableaus)))
    ; art-tableau
    (is-true (tableau-p art-tableau))
    (is (eq problem (tableau-problem art-tableau)))
    (is (eq 'min (problem-type (tableau-instance-problem art-tableau))))
    (is (= 7 (tableau-var-count art-tableau)))
    (is (= 3 (tableau-constraint-count art-tableau)))
    (is (tableau-matrix-equal #2A((2 1 0 1 0 0 0 8) (0 1 1 0 1 0 0 7) (1 0 1 0 0 -1 1 1) (1 0 1 0 0 -1 0 1))
                              #(x y z)
                              art-tableau))
    (is (equalp (vars-to-cols #(3 4 6) art-tableau) (tableau-basis-columns art-tableau)))
    (is (= 1 (tableau-objective-value art-tableau)))
    ; main-tableau
    (is-true (tableau-p main-tableau))
    (is (eq problem (tableau-problem main-tableau)))
    (is (= 6 (tableau-var-count main-tableau)))
    (is (= 3 (tableau-constraint-count main-tableau)))
    (is (tableau-matrix-equal #2A((2 1 0 1 0 0 8) (0 1 1 0 1 0 7) (1 0 1 0 0 -1 1) (-1 -4 -3 0 0 0 0))
                              #(x y z)
                              main-tableau))
    (is (equalp #(3 4 7) (tableau-basis-columns main-tableau)))
    (is (= 0 (tableau-objective-value main-tableau)))))

(test pivot-row
  (let* ((problem (make-linear-problem (max (+ x (* 4 y) (* 3 z)))
                                       (<= (+ (* 2 x) y) 8)
                                       (<= (+ y z) 7)))
         (tableau (build-tableau problem problem))
         (tableau2 (pivot-row tableau (position 'x (problem-vars problem)) 0)))
    (is (not (eq tableau tableau2)))
    (is (= 0 (tableau-objective-value tableau))) ;ensure original not mutated
    (is (eq tableau (n-pivot-row tableau (position 'x (problem-vars problem)) 0)))

    (is-true (tableau-p tableau2))
    (is (eq problem (tableau-problem tableau2)))
    (is (equalp (tableau-matrix tableau) (tableau-matrix tableau2)))
    (is (equalp (tableau-basis-columns tableau) (tableau-basis-columns tableau2)))
    (is (= 5 (tableau-var-count tableau2)))
    (is (= 2 (tableau-constraint-count tableau2)))

    (is (eq problem (tableau-problem tableau)))
    (is (equal 5 (tableau-var-count tableau)))
    (is (equal 2 (tableau-constraint-count tableau)))
    (is (tableau-matrix-equal #2A((1 1/2 0 1/2 0 4) (0 1 1 0 1 7) (0 -7/2 -3 1/2 0 4))
                              #(x y z)
                              tableau))
    (is (equalp (vars-to-cols #(x 4) tableau) (tableau-basis-columns tableau)))
    (is (= 4 (tableau-objective-value tableau)))))


(def-suite solve-tableau
  :in simplex
  :description "The suite to test solve-tableau and n-solve-tableau")
(in-suite solve-tableau)

(test errors
  (signals type-error (n-solve-tableau "max x + y st 2x+y <= 5")))

(test basic-problem
  (let* ((problem (make-linear-problem (max (+ x (* 4 y) (* 3 z)))
                                       (<= (+ (* 2 x) y) 8)
                                       (<= (+ y z) 7)))
         (tableau (build-tableau problem problem))
         (tableau2 (solve-tableau tableau)))
    (is (not (eq tableau tableau2))) ;ensure a new copy was allocated
    (is (= 0 (tableau-objective-value tableau))) ;ensure original not solved
    (is (eq tableau (n-solve-tableau tableau)))

    (is-true (tableau-p tableau2))
    (is (eq problem (tableau-problem tableau2)))
    (is (equalp (tableau-matrix tableau) (tableau-matrix tableau2)))
    (is (equalp (tableau-basis-columns tableau) (tableau-basis-columns tableau2)))
    (is (= 5 (tableau-var-count tableau2)))
    (is (= 2 (tableau-constraint-count tableau2)))

    (is (eq problem (tableau-problem tableau)))
    (is (equal 5 (tableau-var-count tableau)))
    (is (equal 2 (tableau-constraint-count tableau)))
    (is (tableau-matrix-equal #2A((1 0 -1/2 1/2 -1/2 1/2) (0 1 1 0 1 7) (0 0 1/2 1/2 7/2 57/2))
                              #(x y z)
                              tableau))
    (is (equalp (vars-to-cols #(x y) tableau) (tableau-basis-columns tableau)))
    (is (= 57/2 (tableau-objective-value tableau)))))

(test equality-constraint
  (let* ((problem (make-linear-problem (max (+ x (* 4 y) (* 3 z)))
                                       (<= (+ (* 2 x) y) 8)
                                       (<= (+ y z) 7)
                                       (= (+ (* 2 x) y z) 8)))
         (tableaus (build-tableau problem problem))
         (art-tab (first tableaus))
         (main-tab (second tableaus))
         (tab2 (solve-tableau tableaus)))

    (is (not (eq art-tab tab2)))
    (is (not (eq main-tab tab2)))

    (is (eq main-tab (n-solve-tableau tableaus)))

    (is-true (tableau-p tab2))
    (is (eq problem (tableau-problem tab2)))
    (is (equalp (tableau-matrix main-tab) (tableau-matrix tab2)))
    (is (equalp (tableau-basis-columns main-tab) (tableau-basis-columns tab2)))
    (is (equal 5 (tableau-var-count tab2)))
    (is (equal 3 (tableau-constraint-count tab2)))

    ; art-tableau
    (is (eq problem (tableau-problem art-tab)))
    (is (eq 'min (problem-type (tableau-instance-problem art-tab))))
    (is (equal 6 (tableau-var-count art-tab)))
    (is (equal 3 (tableau-constraint-count art-tab)))
    (is (tableau-matrix-equal #2A((1 1/2 0 1/2 0 0 4) (0 1 0 1 1 -1 7) (0 0 1 -1 0 1 0) (0 0 0 0 0 -1 0))
                              #(x y z)
                              art-tab))
    (is (equalp (vars-to-cols #(x 4 z) art-tab) (tableau-basis-columns art-tab)))
    (is (= 0 (tableau-objective-value art-tab)))
    ; main-tableau
    (is-true (tableau-p main-tab))
    (is (eq problem (tableau-problem main-tab)))
    (is (= 5 (tableau-var-count main-tab)))
    (is (= 3 (tableau-constraint-count main-tab)))
    (is (tableau-matrix-equal #2A((1 0 0 0 -1/2 1/2) (0 1 0 1 1 7) (0 0 1 -1 0 0) (0 0 0 1 7/2 57/2))
                              #(x y z)
                              main-tab))
    (is (equalp (vars-to-cols #(x y z) main-tab) (tableau-basis-columns main-tab)))
    (is (= 57/2 (tableau-objective-value main-tab)))))

(test leq-constraint
  (let* ((problem (make-linear-problem (max (+ x (* 4 y) (* 3 z)))
                                       (<= (+ (* 2 x) y) 8)
                                       (<= (+ y z) 7)
                                       (>= (+ x z) 1)))
         (tableaus (build-tableau problem problem))
         (art-tab (first tableaus))
         (main-tab (second tableaus)))
    (is (eq main-tab (n-solve-tableau tableaus)))

    ; art-tableau
    (is (eq problem (tableau-problem art-tab)))
    (is (eq 'min (problem-type (tableau-instance-problem art-tab))))
    (is (equal 7 (tableau-var-count art-tab)))
    (is (equal 3 (tableau-constraint-count art-tab)))
    (is (or (and (tableau-matrix-equal #2A((0 1 -2 1 0 2 -2 6) (0 1 1 0 1 0 0 7) (1 0 1 0 0 -1 1 1) (0 0 0 0 0 0 -1 0))
                                       #(x y z)
                                       art-tab)
                 (equalp (vars-to-cols #(3 4 x) art-tab) (tableau-basis-columns art-tab)))
            (and (tableau-matrix-equal #2A((2 1 0 1 0 0 0 8) (-1 1 0 0 1 1 -1 6) (1 0 1 0 0 -1 1 1) (0 0 0 0 0 0 -1 0))
                                       #(x y z)
                                       art-tab)
                 (equalp (vars-to-cols #(3 4 z) art-tab) (tableau-basis-columns art-tab)))))
    (is (= 0 (tableau-objective-value art-tab)))
    ; main-tableau
    (is (eq problem (tableau-problem main-tab)))
    (is (equal 6 (tableau-var-count main-tab)))
    (is (equal 3 (tableau-constraint-count art-tab)))
    (is (or (and (tableau-matrix-equal #2A((0 1 0 1/3 2/3 2/3 20/3) (0 0 1 -1/3 1/3 -2/3 1/3) (1 0 0 1/3 -1/3 -1/3 2/3) (0 0 0 2/3 10/3 1/3 85/3))
                                       #(x y z)
                                       main-tab)
                 (equalp (vars-to-cols #(y z x) main-tab) (tableau-basis-columns main-tab)))
            (and (tableau-matrix-equal #2A((1 0 0 1/3 -1/3 -1/3 2/3) (0 1 0 1/3 2/3 2/3 20/3) (0 0 1 -1/3 1/3 -2/3 1/3) (0 0 0 2/3 10/3 1/3 85/3))
                                       #(x y z)
                                       main-tab)
                 (equalp (vars-to-cols #(x y z) main-tab) (tableau-basis-columns main-tab)))))
    (is (= 85/3 (tableau-objective-value main-tab)))))

(test unsolvable-problems
  (let* ((problem (make-linear-problem (max (+ x (* 4 y) (* 3 z)))
                                       (<= (+ (* 2 x) y) 4)
                                       (<= (+ y z) 2)
                                       (>= (+ x z) 5)))
         (tableau (build-tableau problem problem)))
    (signals infeasible-problem-error (solve-tableau tableau)))

  (let* ((problem (make-linear-problem (max (+ x (* 4 y) (* 3 z)))
                                       (<= (+ (* 2 x) y) 4)
                                       (<= (+ y (* -1 z)) 4)))
         (tableau (build-tableau problem problem)))
    (signals unbounded-problem-error (solve-tableau tableau))))

(in-suite simplex)

(test copy-tableau
  (declare (notinline copy-tableau))
  (let* ((problem (make-linear-problem (max (+ x (* 4 y) (* 3 z)))
                                       (<= (+ (* 2 x) y) 8)
                                       (<= (+ y z) 7)))
         (tableau1 (build-tableau problem problem))
         (tableau2 (copy-tableau tableau1)))
    (is (not (eq tableau1 tableau2)))
    (is (eq (tableau-problem tableau1) (tableau-problem tableau2)))
    (is (not (eq (tableau-matrix tableau1) (tableau-matrix tableau2))))
    (is (equalp  (tableau-matrix tableau1) (tableau-matrix tableau2)))
    (is (not (eq (tableau-basis-columns tableau1) (tableau-basis-columns tableau2))))
    (is (equalp  (tableau-basis-columns tableau1) (tableau-basis-columns tableau2)))
    (is (= (tableau-var-count tableau1) (tableau-var-count tableau2)))
    (is (= (tableau-constraint-count tableau1) (tableau-constraint-count tableau2)))))

(test tableau-variable
  (declare (notinline tableau-variable))
  (let* ((problem (make-linear-problem (max (= w (+ x (* 4 y) (* 3 z))))
                                       (<= (+ (* 2 x) y) 8)
                                       (<= (+ y z) 7)))
         (tableau (n-solve-tableau (build-tableau problem problem))))
    (is (= 57/2 (tableau-variable tableau 'w)))
    (is (= 1/2 (tableau-variable tableau 'x)))
    (is (= 7 (tableau-variable tableau 'y)))
    (is (= 0 (tableau-variable tableau 'z)))
    (signals error (tableau-variable tableau 'foo)))

  (let* ((problem (make-linear-problem (max (= w (+ x (* 4 y) (* 3 z))))
                                       (<= (+ (* 2 x) y) 8)
                                       (<= (+ y z) 7)
                                       (bounds (x))))
         (tableau (n-solve-tableau (build-tableau problem problem))))
    (is (= 57/2 (tableau-variable tableau 'w)))
    (is (= 1/2 (tableau-variable tableau 'x)))
    (is (= 7 (tableau-variable tableau 'y)))
    (is (= 0 (tableau-variable tableau 'z))))

  (let* ((problem (make-linear-problem (max (= w (+ (- x) (* 4 y) (* 3 z))))
                                       (<= (+ (* -2 x) y) 8)
                                       (<= (+ y z) 7)
                                       (bounds (x))))
         (tableau (n-solve-tableau (build-tableau problem problem))))
    (is (= 57/2 (tableau-variable tableau 'w)))
    (is (= -1/2 (tableau-variable tableau 'x)))
    (is (= 7 (tableau-variable tableau 'y)))
    (is (= 0 (tableau-variable tableau 'z))))

  (let* ((problem (make-linear-problem (max (= w (+ x (* 4 y) (* 3 z))))
                                       (<= (+ (* 2 x) y) 8)
                                       (<= (+ y z) 7)
                                       (bounds (x 5))))
         (tableau (n-solve-tableau (build-tableau problem problem))))
    (is (= 57/2 (tableau-variable tableau 'w)))
    (is (= 1/2 (tableau-variable tableau 'x)))
    (is (= 7 (tableau-variable tableau 'y)))
    (is (= 0 (tableau-variable tableau 'z))))

  (let* ((problem (make-linear-problem (max (= w (+ x (* 4 y) (* 3 z))))
                                       (<= (+ (* 2 x) y) 8)
                                       (<= (+ y z) 7)
                                       (bounds (1 x))))
         (tableau (n-solve-tableau (build-tableau problem problem))))
    (is (= 28 (tableau-variable tableau 'w)))
    (is (= 1 (tableau-variable tableau 'x)))
    (is (= 6 (tableau-variable tableau 'y)))
    (is (= 1 (tableau-variable tableau 'z))))

  (let* ((problem (make-linear-problem (max (= w (+ x (* 4 y) (* 3 z))))
                                       (<= (+ (* 2 x) y) 8)
                                       (<= (+ y z) 7)
                                       (bounds (0 y 5))))
         (tableau (n-solve-tableau (build-tableau problem problem))))
    (is (= 55/2 (tableau-variable tableau 'w)))
    (is (= 3/2 (tableau-variable tableau 'x)))
    (is (= 5 (tableau-variable tableau 'y)))
    (is (= 2 (tableau-variable tableau 'z)))))


(test tableau-reduced-cost
  (declare (notinline tableau-reduced-cost))
  (let* ((problem (make-linear-problem (max (+ x (* 4 y) (* 3 z)))
                                       (<= (+ (* 2 x) y) 8)
                                       (<= (+ y z) 7)))
         (tableau (n-solve-tableau (build-tableau problem problem))))
    (is (= 0 (tableau-reduced-cost tableau 'x)))
    (is (= 0 (tableau-reduced-cost tableau 'y)))
    (is (= 1/2 (tableau-reduced-cost tableau 'z)))
    (signals error (tableau-reduced-cost tableau 'foo)))
  (let* ((problem (make-linear-problem (max (+ x (* 4 y) (* 3 z)))
                                       (<= (+ (* 2 x) y) 8)
                                       (<= (+ y z) 7)
                                       (bounds (z))))
         (tableau (n-solve-tableau (build-tableau problem problem))))
    (is (= 1 (tableau-reduced-cost tableau 'x)))
    (is (= 0 (tableau-reduced-cost tableau 'y)))
    (signals error (tableau-reduced-cost tableau 'z))))

(test with-tableau-variables
  (let* ((problem (make-linear-problem (= w (max (+ x (* 4 y) (* 3 z))))
                                       (<= (+ (* 2 x) y) 8)
                                       (<= (+ y z) 7)))
         (tableau (n-solve-tableau (build-tableau problem problem))))
    (with-tableau-variables (x y z w) tableau
      (is (= 57/2 w))
      (is (= 1/2 x))
      (is (= 7 y))
      (is (= 0 z)))
    (eval `(with-tableau-variables ,problem ,tableau
             (is (= 57/2 w))
             (is (= 1/2 x))
             (is (= 7 y))
             (is (= 0 z))))))
