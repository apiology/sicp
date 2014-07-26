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
(install-sparse-termlist-package)

;; common data for tests

;; 5*x^3 + 2x
(def poly1-in-x (make-polynomial :x '((3 5) (1 2))))
;; 5*x^2 + 2x
(def poly2-in-x (make-polynomial :x '((2 5) (1 2))))
(def zero-in-x (make-polynomial :x '()))


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
    (is (= (make-polynomial :x '((3 5) (1 2)))
           poly1-in-x))))

;; 5*y^3 + 2y
(def poly1-in-y (make-polynomial :y '((3 5) (1 2))))

(deftest test-different-variable-assertions
  (testing "FIXME, I fail."
    (is (thrown? java.lang.AssertionError
                 (add poly1-in-x poly1-in-y)))))

(deftest test-add-polynomials
  (testing 
    (is (= (make-polynomial :x '((3 5) (2 5) (1 4)))
           (add poly1-in-x poly2-in-x)))))


(deftest test-multiply-polynomials
  (testing 
    ;; (5*x^3 + 2x) * (5*x^2 + 2x) = 
    ;;
    ;; 25*x^5 + 10*x^4 + 10*x^3 + 4*x^2
    (is (= (make-polynomial :x '((5 25) (4 10) (3 10) (2 4)))
           (mul poly1-in-x poly2-in-x)))))

;; Exercise 2.87

(deftest test-=zero?
  (testing "zero is zero"
    ;; (5*x^3 + 2x) * (5*x^2 + 2x) = 
    ;;
    ;; 25*x^5 + 10*x^4 + 10*x^3 + 4*x^2
    (is (= true
           (=zero? zero-in-x))))
  (testing "others are not zero"
    (is (= false
           (=zero? poly1-in-x)))))

(def rational-half (make-rational 1 2))

(deftest add-rationals-to-rationals
  (testing "adding rationals to rationals"
    (is (= 1
           (add rational-half rational-half)))))

(deftest add-polynomials-to-rationals
  (testing "adding polynomials with polynomial terms"
    ;; (5*x^3 + 2x) + 1/2 =
    ;; 5*x^3 + 2x + 1/2
    (is (= (make-polynomial :x '((3 5) (1 2) (0 (:rational (1 2)))))
           (add poly1-in-x rational-half)))))

;; (5*y^3 + 2y)*x^3 + 2x
(def poly3-in-x (make-polynomial :x (list (list 3 poly1-in-y) (list 1 2))))

(deftest test-raise-one-step
  (testing "raise-one-step five"
    (is (= '((:polynomial (:any 0 (:complex (:rectangular (3 4)))))
             (:complex (:rectangular (3 4))))
           (raise-one-step (list (make-complex-from-real-imag 3 4)
                                 (make-complex-from-real-imag 3 4)))))))

(deftest test-raise-one-step
  (testing "raise-one-step five"
    (is (= (list (make-polynomial :any '((0 (:complex (:rectangular (3 4))))))
                 '(:complex (:rectangular (3 4))))
           (raise-one-step (list (make-complex-from-real-imag 3 4) 
                                 (make-complex-from-real-imag 3 4)))))))

(deftest polynomials-with-polynomial-terms
  (testing "adding polynomials with polynomial terms"
    ;; (5*y^3 + 2y)*x^3 + 2x + (5*x^3 + 2x) =
    ;; (5*y^3 + 2y + 5)*x^3 + 4x
    (is (= (make-polynomial :x 
                            (list (list 3 (make-polynomial :y '((3 5) (1 2) (0 5))))
                                  '(1 4)))
           (add poly3-in-x poly1-in-x)))))

;; Exercise 2.88

(deftest simple-negate
  (testing "-(5*x^3 + 2x) = -5*x^3 - 2x"
    (is (= (make-polynomial :x '((3 -5) (1 -2)))
           (negate poly1-in-x)))))
(deftest simple-polynomial-subtraction
  (testing "(5*x^2 + 2x) - (5*x^3 + 2x) = (-5*x^3 + 5*x^2)"
    (is (= (make-polynomial :x '((3 -5) (2 5)))
           (sub poly2-in-x poly1-in-x)))))
