(ns section-2-5-3.polynomial-test
  (:require [clojure.test :refer :all]
            [section-2-5-3.log :refer :all]
            [section-2-5-3.module :refer :all]
            [section-2-5-3.math :refer :all]
            [section-2-5-3.polynomial :refer :all]
            [section-2-5-3.polar :refer :all]
            [section-2-5-3.rational :refer :all]
            [section-2-5-3.rectangular :refer :all]
            [section-2-5-3.complex :refer :all]
            [section-2-5-3.clj-number :refer :all]))

(install-polynomial-package)
(install-polar-package)
(install-rational-package)
(install-rectangular-package)
(install-complex-package)
(install-clj-number-package)

;; common data for tests

;; 5*x^3 + 2x
(def poly1-in-x (make-polynomial :x '((3 5) (1 2))))
;; 5*x^2 + 2x
(def poly2-in-x (make-polynomial :x '((2 5) (1 2))))
(def poly1-in-y (make-polynomial :y '((3 5) (1 2))))

;; Polynomials having just one variable is called a 'univariate
;; polynomial', which is what this will be dealing with.

;; Polynomial is a sum of terms consisting of coefficient multiplying
;; a power of the "indeterminate".

;; 5x^3 + 3x * 7 ;; simple polynomial in x.

;; (y^3 + 1)*x^3 + (2y)x + 1 ;; polnomial in x whose coefficients are
;; polynomials in y.

;; Doing arithmetic in polynomials.


(deftest test-make-new-polynomial
  (testing "FIXME, I fail."
    (is (= '(:polynomial (:x (3 5) (1 2)))
           poly1-in-x))))

(deftest test-different-variable-assertions
  (testing "FIXME, I fail."
    (is (thrown? java.lang.AssertionError
                 (add poly1-in-x poly1-in-y)))))

(deftest test-add-polynomials
  (testing 
    (is (= '(:polynomial (:x (3 5) (2 5) (1 4)))
           (add poly1-in-x poly2-in-x)))))


(deftest test-multiply-polynomials
  (testing 
    ;; (5*x^3 + 2x) * (5*x^2 + 2x) = 
    ;;
    ;; 25*x^5 + 10*x^4 + 10*x^3 + 4*x^2
    (is (= '(:polynomial (:x (5 25) (4 10) (3 10) (2 4)))
           (mul poly1-in-x poly2-in-x)))))
