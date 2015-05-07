(ns section-4-1-2.core-test
  (:require [clojure.test :refer :all]
            [section-4-1-2.core :refer :all])
  (:refer-clojure :only [= comment]))

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
    (is (= (eval '(if 1 123) default-env) 123))))

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



(deftest and-with-nothing
  (testing ""
    (is (= (eval '(and) default-env) true))))

(deftest and-with-one-false-arg
  (testing ""
    (is (= (eval '(and 0) default-env) false))))

(deftest and-with-one-true-arg
  (testing ""
    (is (= (eval '(and 1) default-env) 1))))

(deftest and-with-one-false-and-one-true-arg
  (testing ""
    (is (= (eval '(and 0 1) default-env) false))))

(deftest and-short-circuits
  (testing ""
    (is (= (eval '(and 0 1 unkonwn-symbol) default-env) false))))

(deftest and-with-many-false-and-one-true-arg
  (testing ""
    (is (= (eval '(and 0 false 0 12) default-env) false))))

(deftest and-does-not-short-circuit
  (testing ""
    (is (= (eval '(and 0 false 0 12 unknown-symbol) default-env) false))))

(deftest and-evaluates-many-args
  (testing ""
    (is (= (eval '(and 1 2 3 4 5) default-env) 5))))


;; (deftest one-plus-one-equals-two
;;   (testing "o"
;;     (is (= (eval '(+ 1 1) default-env) 2))))




;; XXX add cloverage
(deftest cond-with-one-true-clause
  (testing ""
    (is (= (eval '(cond (true 123)) default-env) 123))))

(deftest cond-with-no-true-clauses
  (testing ""
    (is (= (eval '(cond (false 123)) default-env) false))))

(deftest cond-with-one-late-true-clauses
  (testing ""
    (is (= (eval '(cond (false 123) (true 456)) default-env) 456))))

(deftest cond-with-two-true-clauses
  (testing ""
    (is (= (eval '(cond (false 123) (true 456) (true 789)) default-env) 456))))

(deftest cond-with-lots-of-clauses
  (testing ""
    (is (= (eval '(cond (false 123) (true 456) (true 789) (false 233) (true 111)) default-env) 456))))
