(ns section-4-1-2.cond-test
  (:require [section-4-1-2.core :refer :all]
            [clojure.test :refer :all])
  (:refer-clojure :only [=]))

(def default-env nil)
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

(deftest cond-with-else-clause
  (testing ""
    (is (= (eval '(cond (false 123) (false 456) (false 789) (false 233) (false 111) (else 421)) default-env) 421))))

(deftest cond-with-cond-rocket
  (testing ""
    (is (= (eval '(cond (true => (lambda (x) 1))) default-env) 1))))
