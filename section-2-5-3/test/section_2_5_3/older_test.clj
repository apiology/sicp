(ns section-2-5-3.older-test
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

(deftest test-types-to-str
  (testing 
    (is (= ":clojure-number :polynomial :alien-technology"
           (types-to-str '(:clojure-number :polynomial :alien-technology))))))

(deftest test-type-tag
  (testing 
    (is (= ":clojure-number :polynomial :alien-technology"
           (types-to-str '(:clojure-number :polynomial :alien-technology))))))

(deftest test-lt
  (testing "positive"
    (is (= false
           (lt 1 0))))
  (testing "negative"
    (is (= true
           (lt -1 0)))))

(deftest test-abs
  (testing "positive 1"
    (is (= 1
           (abs 1))))
  (testing "positive 2"
    (is (= 2
           (abs 2))))
  (testing "negative"
    (is (= 1
           (abs -1))))
  (testing "negative 2"
    (is (= 2
           (abs -2)))))

(deftest make-rational-test
  (testing "can create negative rationals"
    (is (= '(:rational (-1 1))
           (make-rational -1 1)))))

(deftest test-adding-mixed
  (testing "FIXME, I fail."
    (is (= (make-complex-from-real-imag 6 2) 
           (add (make-complex-from-real-imag 1 2)
                5)))))
(deftest test-lt
  (testing "positive"
    (is (= false
           (lt 1 0))))
  (testing "negative"
    (is (= true
           (lt -1 0)))))

(deftest test-abs
  (testing "positive 1"
    (is (= 1
           (abs 1))))
  (testing "positive 2"
    (is (= 2
           (abs 2))))
  (testing "negative"
    (is (= 1
           (abs -1))))
  (testing "negative 2"
    (is (= 2
           (abs -2)))))

(deftest make-rational-test
  (testing "can create negative rationals"
    (is (= '(:rational (-1 1))
           (make-rational -1 1)))))

(deftest test-adding-mixed
  (testing "FIXME, I fail."
    (is (= (make-complex-from-real-imag 6 2) 
           (add (make-complex-from-real-imag 1 2)
                5)))))

(deftest test-exponentiating-clj-number
  (testing "exponentiation clojure numbers"
    (is (= (exp 2 5) 32))))

(deftest test-exponentiating-complexes
  (testing "exponentiation complex numbers"
    (is (thrown? java.lang.Exception
           (exp (make-complex-from-real-imag 6 2)
                (make-complex-from-real-imag 6 2))))))

(deftest test-raise
  (testing "can raise clj-number"
    (is (= '(:rational (3 1))
           (raise 3))))
  (testing "can raise zero clj-number"
    (is (= '(:rational (0 1))
           (raise 0))))
  (testing "can raise negative clj-number"
    (is (= '(:rational (-1 1))
           (raise -1))))
  (testing "can raise floating clj-number"
    (is (= '(:rational (0.5 1))
           (raise 0.5))))
  (testing "can raise negative floating clj-number"
    (is (= '(:rational (-0.5 1))
           (raise -0.5))))
  (testing "can raise rational"
    (is (= '(:complex (:rectangular ((:rational (3 1)) 0)))
           (raise (raise 3))))))

(deftest test-lower-type
  (testing "lower-type one"
    (is (= nil
           (lower-type :clj-number))))
  (testing "lower-type two"
    (is (= :clj-number
           (lower-type :rational))))
  (testing "lower-type three"
    (is (= :rational
           (lower-type :complex)))))

(deftest test-is-lower
  (testing "is-lower-type? one"
    (is (= false
           (is-lower-type? :clj-number :clj-number))))
  (testing "is-lower-type? two"
    (is (= true
           (is-lower-type? :clj-number :rational))))
  (testing "is-lower-type? three"
    (is (= true
           (is-lower-type? :rational :complex))))
  (testing "is-lower-type? three"
    (is (= false
           (is-lower-type? :rational :clj-number))))
  (testing "is-lower-type? four"
    (is (= true
           (is-lower-type? :clj-number :complex)))))

(deftest test-type-comparator
  (testing "type-comparator one"
    (is (= 0
           (type-comparator :clj-number :clj-number))))
  (testing "type-comparator two"
    (is (= 0
           (type-comparator :rational :rational))))
  (testing "type-comparator three"
    (is (= 0
           (type-comparator :rational :rational))))
  (testing "type-comparator three"
    (is (= -1
           (type-comparator :rational :complex))))
  (testing "type-comparator four"
    (is (= 1
           (type-comparator :rational :clj-number)))))

(deftest test-find-lowest-type
  (testing "find-lowest-type one"
    (is (= :clj-number
           (find-lowest-type #{:clj-number}))))
  (testing "find-lowest-type two"
    (is (= :rational
           (find-lowest-type #{:rational}))))
  (testing "find-lowest-type three"
    (is (= :rational
           (find-lowest-type '(:rational :rational)))))
  (testing "find-lowest-type three"
    (is (= :rational
           (find-lowest-type '(:rational :complex)))))
  (testing "find-lowest-type four"
    (is (= :clj-number
           (find-lowest-type '(:rational :clj-number :complex))))))

(deftest test-find-index-of-type-to-raise
  (testing "find-index-of-type-to-raise one"
    (is (= 0
           (find-index-of-type-to-raise '(:clj-number)))))
  (testing "find-index-of-type-to-raise two"
    (is (= 0
           (find-index-of-type-to-raise '(:rational)))))
  (testing "find-index-of-type-to-raise three"
    (is (= 0
           (find-index-of-type-to-raise '(:rational :rational)))))
  (testing "find-index-of-type-to-raise three"
    (is (= 0
           (find-index-of-type-to-raise '(:rational :complex)))))
  (testing "find-index-of-type-to-raise four"
    (is (= 1
           (find-index-of-type-to-raise '(:rational :clj-number))))))

(deftest test-raise-one-step
  (testing "raise-one-step one"
    (is (= '((:rational (2 1)))
           (raise-one-step (list 2)))))
  (testing "raise-one-step one"
    (is (= '((:rational (2 1)) 2)
           (raise-one-step (list 2 2)))))
  (testing "raise-one-step two"
    (is (= '((:complex (:rectangular ((:rational (1 2)) 0))))
           (raise-one-step (list (make-rational 1 2))))))
  (testing "raise-one-step three"
    (is (= '((:complex (:rectangular ((:rational (1 2)) 0))) (:rational (3 4)))
           (raise-one-step (list (make-rational 1 2) (make-rational 3 4))))))
  (testing "raise-one-step three"
    (is (= '((:complex (:rectangular ((:rational (1 2)) 0))) (:complex (:rectangular (3 4))))
           (raise-one-step (list (make-rational 1 2) (make-complex-from-real-imag 3 4))))))
  (testing "raise-one-step four"
    (is (= '((:rational (1 2)) (:rational (2 1)))
           (raise-one-step (list (make-rational 1 2) 2)))))
  (testing "raise-one-step six"
    (is (= '((:rational 0 1) (:rational (-0.5 1)))
           (raise-one-step '((:rational 0 1) -0.5))))))

(deftest test-project-one-step
  (testing "project a clj-number unsuccessfully"
    (is (= nil
           (project-one-step 5))))
  (testing "project a rational"
    (is (= 5
           (project-one-step (make-rational 5 1)))))
  (testing "project a rational unsuccessfully"
    (is (= 5
           (project-one-step (make-rational 5 2)))))
  (testing "project a complex to a rational"
    (is (= '(:rational (1 2))
           (project-one-step (make-complex-from-real-imag (make-rational 1 2) 0)))))
  (testing "project a complex to a clj-number"
    (is (= 2
           (project-one-step (make-complex-from-real-imag 2 0)))))
  (testing "project a complex unsuccessfully"
    (is (= 2
           (project-one-step (make-complex-from-real-imag 2 1))))))

(deftest test-drop-item-one-step
  (testing "drop a clj-number unsuccessfully"
    (is (= nil
           (drop-item-one-step 5))))
  (testing "drop a rational"
    (is (= 5
           (drop-item-one-step (make-rational 5 1)))))
  (testing "drop a rational unsuccessfully"
    (is (= nil
           (drop-item-one-step (make-rational 5 2)))))
  (testing "drop a complex to a rational"
    (is (= '(:rational (1 2))
           (drop-item-one-step (make-complex-from-real-imag (make-rational 1 2) 0)))))
  (testing "drop a complex to a clj-number"
    (is (= (make-rational 2 1)
           (drop-item-one-step (make-complex-from-real-imag (make-rational 2 1) 0)))))
  (testing "drop a complex unsuccessfully"
    (is (= nil
           (drop-item-one-step (make-complex-from-real-imag (make-rational 2 1) 1))))))


(deftest test-drop
  (testing "drop a clj-number unsuccessfully"
    (is (= 5
           (drop 5))))
  (testing "drop a rational"
    (is (= 5
           (drop (make-rational 5 1)))))
  (testing "drop a rational unsuccessfully"
    (is (= (make-rational 5 2)
           (drop (make-rational 5 2)))))
  (testing "drop a complex to a rational"
    (is (= '(:rational (1 2))
           (drop (make-complex-from-real-imag (make-rational 1 2) 0)))))
  (testing "drop a complex to a clj-number"
    (is (= 2
           (drop (make-complex-from-real-imag (make-rational 2 1) 0)))))
  (testing "drop a complex unsuccessfully"
    (is (= (make-complex-from-real-imag (make-rational 2 1) 1)
           (drop (make-complex-from-real-imag (make-rational 2 1) 1))))))

(deftest test-complex-generic-ops-magnitude
  (testing "magnitude of a regular rectangular complex"
    (is (= (sqrt 2)
           (magnitude (make-complex-from-real-imag 1 1)))))
  (testing "magnitude of a rational rectangular complex"
    (is (= (sqrt (add (square (make-rational 1 2)) (square (make-rational 1 2))))
           (magnitude (make-complex-from-real-imag (make-rational 1 2) 
                                                   (make-rational 1 2))))))
  (testing "magnitude of a regular polar complex"
    (is (= 1
           (magnitude (make-complex-from-mag-ang 1 1)))))
  (testing "magnitude of a rational polar complex"
    (is (= 1
           (magnitude (make-complex-from-mag-ang (make-rational 1 1) (make-rational 1 1)))))))

(deftest test-complex-generic-ops-real
  (testing "real-part of a regular rectangular complex"
    (is (= 1
           (real-part (make-complex-from-real-imag 1 1)))))
  (testing "real-part of a rational rectangular complex"
    (is (= '(:rational (1 2))
           (real-part (make-complex-from-real-imag (make-rational 1 2) 
                                                   (make-rational 1 2))))))
  (testing "real-part of a regular polar complex"
    (is (= 0.5406248433479329
           (real-part (make-complex-from-mag-ang 1 1)))))
  (testing "real-part of a rational polar complex"
    (is (= 0.5406248433479329
           (real-part (make-complex-from-mag-ang (make-rational 1 1) (make-rational 1 1)))))))

(deftest test-complex-generic-ops-imag
  (testing "imag-part of a regular rectangular complex"
    (is (= 1
           (imag-part (make-complex-from-real-imag 1 1)))))
  (testing "imag-part of a rational rectangular complex"
    (is (= '(:rational (1 2))
           (imag-part (make-complex-from-real-imag (make-rational 1 2) 
                                                   (make-rational 1 2))))))
  (testing "imag-part of a regular polar complex"
    (is (= 0.8415945650055845
           (imag-part (make-complex-from-mag-ang 1 1)))))
  (testing "imag-part of a rational polar complex"
    (is (= '(:rational (3.407091745578266E91 4.048376602284329E91)
           (imag-part (make-complex-from-mag-ang (make-rational 1 1) (make-rational 1 1))))))))

(deftest test-complex-generic-ops-angle
  (testing "angle of a regular rectangular complex"
    (is (= 0.7853981633974483
           (angle (make-complex-from-real-imag 1 1)))))
  (testing "angle of a rational rectangular complex"
    (is (= 0.7853981633974483
           (angle (make-complex-from-real-imag (make-rational 1 2) 
                                               (make-rational 1 2))))))
  (testing "angle of a regular polar complex"
    (is (= 1
           (angle (make-complex-from-mag-ang 1 1)))))
  (testing "angle of a rational polar complex"
    (is (= 1
           (angle (make-complex-from-mag-ang (make-rational 1 1) (make-rational 1 1)))))))


