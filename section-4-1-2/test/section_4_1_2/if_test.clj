(ns section-4-1-2.if-test
  (:require [section-4-1-2.core :refer :all]
            [clojure.test :refer :all])
  (:refer-clojure :only [=]))

(def default-env nil)

(deftest if-false
  (testing "if-false"
    (is (= (eval '(if 0 123 456) default-env) 456))))

(deftest if-true
  (testing ""
    (is (= (eval '(if 1 123 456) default-env) 123))))

(deftest if-true-without-second
  (testing ""
    (is (= (eval '(if 1 123) default-env) 123))))
