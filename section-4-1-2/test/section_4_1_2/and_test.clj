(ns section-4-1-2.and-test
  (:require [section-4-1-2.core :refer :all]
            [clojure.test :refer :all])
  (:refer-clojure :only [=]))

(def default-env nil)

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
