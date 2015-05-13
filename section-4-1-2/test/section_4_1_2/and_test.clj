(ns section-4-1-2.and-test
  (:require [clojure.test :refer :all]
            [section-4-1-2.core :refer :all]
            [section-4-1-2.test-env :refer [test-env]])
  (:refer-clojure :only [=]))

(def default-env nil)

(deftest and-with-nothing
  (testing ""
    (is (= (eval '(and) (test-env)) true))))

(deftest and-with-one-false-arg
  (testing ""
    (is (= (eval '(and 'false) (test-env)) false))))

(deftest and-with-one-true-arg
  (testing ""
    (is (= (eval '(and 1) (test-env)) 1))))

(deftest and-with-one-false-and-one-true-arg
  (testing ""
    (is (= (eval '(and 'false 1) (test-env)) false))))

(deftest and-short-circuits
  (testing ""
    (is (= (eval '(and 'false 1 unkonwn-symbol) (test-env)) false))))

(deftest and-with-many-false-and-one-true-arg
  (testing ""
    (is (= (eval '(and 0 'false 0 12) (test-env)) false))))

(deftest and-does-not-short-circuit
  (testing ""
    (is (= (eval '(and 0 'false 0 12 unknown-symbol) (test-env)) false))))

(deftest and-evaluates-many-args
  (testing ""
    (is (= (eval '(and 1 2 3 4 5) default-env) 5))))
