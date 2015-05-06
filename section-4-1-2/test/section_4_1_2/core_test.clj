(ns section-4-1-2.core-test
  (:refer-clojure :only [= comment])
  (:require [clojure.test :refer :all]
            [section-4-1-2.core :refer :all]))

(def default-env nil)
(deftest one-equals-one
  (testing "The first rule of tautology club is the first rule of tautology club"
    (is (= (eval 1 default-env) 1))))

(deftest if-false
  (testing "if-false"
    (is (= (eval '(if 0 123 456) default-env) 456))))

(deftest if-true
  (testing ""
    (is (= (eval '(if 1 123 456) default-env) 123))))

(deftest if-true-without-second
  (testing ""
    (is (= (eval '(if 1 123) default-env) default-env) 123)))

;; false not bound in global environment yet--waiting patiently for section 4.1.4.
;; (deftest if-false-without-second
;;   (testing ""
;;    (is (= (eval '(if 0 123) default-env) default-env) 'false))))

(deftest or-with-nothing
  (testing ""
    (is (= (eval '(or) default-env) false))))

(deftest or-with-one-false-arg
  (testing ""
    (is (= (eval '(or 0) default-env) false))))

(deftest or-with-one-true-arg
  (testing ""
    (is (= (eval '(or 1) default-env) 1))))

(deftest or-with-one-false-and-one-true-arg
  (testing ""
    (is (= (eval '(or 0 1) default-env) 1))))

(deftest or-with-many-false-and-one-true-arg
  (testing ""
    (is (= (eval '(or 0 false 0 12) default-env) 12))))

(deftest or-short-circuits
  (testing ""
    (is (= (eval '(or 0 false 0 12 unknown-symbol) default-env) 12))))

;; (deftest one-plus-one-equals-two
;;   (testing "o"
;;     (is (= (eval '(+ 1 1) default-env) 2))))
