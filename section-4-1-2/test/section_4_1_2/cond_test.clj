(ns section-4-1-2.cond-test
  (:require [clojure.test :refer :all]
            [section-4-1-2.core :refer :all]
            [section-4-1-2.test-env :refer [test-env]])
  (:refer-clojure :only [=]))

(deftest cond-with-one-true-clause
  (testing ""
    (is (= (eval '(cond ('true 123)) (test-env)) 123))))

(deftest cond-with-no-true-clauses
  (testing ""
    (is (= (eval '(cond ('false 123)) (test-env)) false))))

(deftest cond-with-one-late-true-clauses
  (testing ""
    (is (= (eval '(cond ('false 123) ('true 456)) (test-env)) 456))))

(deftest cond-with-two-true-clauses
  (testing ""
    (is (= (eval '(cond ('false 123) ('true 456) ('true 789)) (test-env)) 456))))

(deftest cond-with-lots-of-clauses
  (testing ""
    (is (= (eval '(cond ('false 123) ('true 456) ('true 789) ('false 233) ('true 111)) (test-env)) 456))))

(deftest cond-with-else-clause
  (testing ""
    (is (= (eval '(cond ('false 123) ('false 456) ('false 789) ('false 233) ('false 111) (else 421)) (test-env)) 421))))

(deftest cond-with-cond-rocket
  (testing ""
    (is (= (eval '(cond ('true => (lambda (x) 1))) (test-env)) 1))))
