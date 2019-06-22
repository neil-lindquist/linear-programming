
(uiop:define-package :linear-programming-test/simplex
  (:use :cl
        :fiveam
        :linear-programming-test/base
        :linear-programming/problem
        :linear-programming/simplex)
  (:export #:simplex))

(in-package :linear-programming-test/simplex)

(def-suite simplex
  :in linear-programming
  :description "The suite to test linear-programming/simplex")
(in-suite simplex)

(test build-tableau
  (let* ((problem (make-linear-problem (max (+ x (* 4 y) (* 3 z)))
                                       (<= (+ (* 2 x) y) 8)
                                       (<= (+ y z) 7)))
         (tableau (build-tableau problem)))
    (is-true (tableau-p tableau))
    (is (eq problem (tableau-problem tableau)))
    (is (= 5 (tableau-var-count tableau)))
    (is (= 2 (tableau-constraint-count tableau)))
    (is (equalp #2A((2 0 -1) (1 1 -4) (0 1 -3) (1 0 0) (0 1 0) (8 7 0)) (tableau-matrix tableau)))
    (is (equalp #(3 4) (tableau-basis-columns tableau)))
    (is (= 0 (tableau-objective-value tableau))))

  (let* ((problem (make-linear-problem (max (+ x (* 4 y) (* 3 z)))
                                       (<= (+ (* 2 x) y) 8)
                                       (<= (+ y z) 7)
                                       (= (+ (* 2 x) y z) 8)))
         (tableaus (build-tableau problem))
         (art-tableau (first tableaus))
         (main-tableau (second tableaus)))
    (is (= 2 (length tableaus)))
    ; art-tableau
    (is-true (tableau-p art-tableau))
    (is (eq 'min (lp-type (tableau-problem art-tableau))))
    (is (= 6 (tableau-var-count art-tableau)))
    (is (= 3 (tableau-constraint-count art-tableau)))
    (is (equalp #2A((2 0 2 2) (1 1 1 1) (0 1 1 1) (1 0 0 0) (0 1 0 0) (0 0 1 0) (8 7 8 8))
                (tableau-matrix art-tableau)))
    (is (equalp #(3 4 5) (tableau-basis-columns art-tableau)))
    (is (= 8 (tableau-objective-value art-tableau)))
    ; main-tableau
    (is-true (tableau-p main-tableau))
    (is (eq problem (tableau-problem main-tableau)))
    (is (= 5 (tableau-var-count main-tableau)))
    (is (= 3 (tableau-constraint-count main-tableau)))
    (is (equalp #2A((2 0 2 -1) (1 1 1 -4) (0 1 1 -3) (1 0 0 0) (0 1 0 0) (8 7 8 0))
                (tableau-matrix main-tableau)))
    (is (equalp #(3 4 0) (tableau-basis-columns main-tableau)))
    (is (= 0 (tableau-objective-value main-tableau))))

  (let* ((problem (make-linear-problem (max (+ x (* 4 y) (* 3 z)))
                                       (<= (+ (* 2 x) y) 8)
                                       (<= (+ y z) 7)
                                       (>= (+ x z) 1)))
         (tableaus (build-tableau problem))
         (art-tableau (first tableaus))
         (main-tableau (second tableaus)))
    (is (= 2 (length tableaus)))
    ; art-tableau
    (is-true (tableau-p art-tableau))
    (is (eq 'min (lp-type (tableau-problem art-tableau))))
    (is (= 7 (tableau-var-count art-tableau)))
    (is (= 3 (tableau-constraint-count art-tableau)))
    (is (equalp #2A((1 1 0 0) (2 0 1 1) (0 1 1 1) (1 0 0 0) (0 1 0 0) (0 0 -1 -1) (0 0 1 0) (8 7 1 1))
                (tableau-matrix art-tableau)))
    (is (equalp #(3 4 6) (tableau-basis-columns art-tableau)))
    (is (= 1 (tableau-objective-value art-tableau)))
    ; main-tableau
    (is-true (tableau-p main-tableau))
    (is (eq problem (tableau-problem main-tableau)))
    (is (= 6 (tableau-var-count main-tableau)))
    (is (= 3 (tableau-constraint-count main-tableau)))
    (is (equalp #2A((1 1 0 -4) (2 0 1 -1) (0 1 1 -3) (1 0 0 0) (0 1 0 0) (0 0 -1 0) (8 7 1 0))
                (tableau-matrix main-tableau)))
    (is (equalp #(3 4 5) (tableau-basis-columns main-tableau)))
    (is (= 0 (tableau-objective-value main-tableau)))))


(test solve-tableau
  (let* ((problem (make-linear-problem (max (+ x (* 4 y) (* 3 z)))
                                       (<= (+ (* 2 x) y) 8)
                                       (<= (+ y z) 7)))
         (tableau (build-tableau problem)))
    (is (eq tableau (solve-tableau tableau)))
    (is (eq problem (tableau-problem tableau)))
    (is (equal 5 (tableau-var-count tableau)))
    (is (equal 2 (tableau-constraint-count tableau)))
    (is (equalp #2A((1 0 0) (0 1 0) (-1/2 1 1/2) (1/2 0 1/2) (-1/2 1 7/2) (1/2 7 57/2)) (tableau-matrix tableau)))
    (is (equalp #(0 1) (tableau-basis-columns tableau)))
    (is (= 57/2 (tableau-objective-value tableau))))

  (let* ((problem (make-linear-problem (max (+ x (* 4 y) (* 3 z)))
                                       (<= (+ (* 2 x) y) 8)
                                       (<= (+ y z) 7)
                                       (= (+ (* 2 x) y z) 8)))
         (tableaus (build-tableau problem))
         (art-tab (first tableaus))
         (main-tab (second tableaus)))
    (is (eq main-tab (solve-tableau tableaus)))

    ; art-tableau
    (is (eq 'min (lp-type (tableau-problem art-tab))))
    (is (equal 6 (tableau-var-count art-tab)))
    (is (equal 3 (tableau-constraint-count art-tab)))
    (is (equalp #2A((1 0 0 0) (1/2 1 0 0) (0 0 1 0) (1/2 1 -1 0) (0 1 0 0) (0 -1 1 -1) (4 7 0 0))
                (tableau-matrix art-tab)))
    (is (equalp #(0 4 2) (tableau-basis-columns art-tab)))
    (is (= 0 (tableau-objective-value art-tab)))
    ; main-tableau
    (is-true (tableau-p main-tab))
    (is (eq problem (tableau-problem main-tab)))
    (is (= 5 (tableau-var-count main-tab)))
    (is (= 3 (tableau-constraint-count main-tab)))
    (is (equalp #2A((1 0 0 0) (0 1 0 0) (0 0 1 0) (0 1 -1 1) (-1/2 1 0 7/2) (1/2 7 0 57/2))
                (tableau-matrix main-tab)))
    (is (equalp #(0 1 2) (tableau-basis-columns main-tab)))
    (is (= 57/2 (tableau-objective-value main-tab))))

  (let* ((problem (make-linear-problem (max (+ x (* 4 y) (* 3 z)))
                                       (<= (+ (* 2 x) y) 8)
                                       (<= (+ y z) 7)
                                       (>= (+ x z) 1)))
         (tableaus (build-tableau problem))
         (art-tab (first tableaus))
         (main-tab (second tableaus)))
    (is (eq main-tab (solve-tableau tableaus)))

    ; art-tableau
    (is (eq 'min (lp-type (tableau-problem art-tab))))
    (is (equal 7 (tableau-var-count art-tab)))
    (is (equal 3 (tableau-constraint-count art-tab)))
    (is (equalp #2A((1 1 0 0) (0 0 1 0) (-2 1 1 0) (1 0 0 0) (0 1 0 0) (2 0 -1 0) (-2 0 1 -1) (6 7 1 0))
                (tableau-matrix art-tab)))
    (is (equalp #(3 4 1) (tableau-basis-columns art-tab)))
    (is (= 0 (tableau-objective-value art-tab)))
    ; main-tableau
    (is (eq problem (tableau-problem main-tab)))
    (is (equal 6 (tableau-var-count main-tab)))
    (is (equal 3 (tableau-constraint-count main-tab)))
    (is (equalp #2A((1 0 0 0) (0 0 1 0) (0 1 0 0) (1/3 -1/3 1/3 2/3) (2/3 1/3 -1/3 10/3) (2/3 -2/3 -1/3 1/3) (20/3 1/3 2/3 85/3))
                (tableau-matrix main-tab)))
    (is (equalp #(0 2 1) (tableau-basis-columns main-tab)))
    (is (= 85/3 (tableau-objective-value main-tab)))))


(test get-tableau-variable
  (let* ((problem (make-linear-problem (max (+ x (* 4 y) (* 3 z)))
                                       (<= (+ (* 2 x) y) 8)
                                       (<= (+ y z) 7)))
         (tableau (solve-tableau (build-tableau problem))))
    (is (= 1/2 (get-tableau-variable 'x tableau)))
    (is (= 7 (get-tableau-variable 'y tableau)))
    (is (= 0 (get-tableau-variable 'z tableau)))))

(test with-tableau-variables
  (let* ((problem (make-linear-problem (= w (max (+ x (* 4 y) (* 3 z))))
                                       (<= (+ (* 2 x) y) 8)
                                       (<= (+ y z) 7)))
         (tableau (solve-tableau (build-tableau problem))))
    (with-tableau-variables (x y z w) tableau
      (is (= 57/2 w))
      (is (= 1/2 x))
      (is (= 7 y))
      (is (= 0 z)))
    (eval `(with-tableau-variables ,problem ,tableau
             (is (= 57/2 w))
             (is (= x 1/2))
             (is (= y 7))
             (is (= z 0))))))
