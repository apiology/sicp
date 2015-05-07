(ns section-4-1-2.or-test
  (:require [section-4-1-2.core :refer :all]
            [clojure.test :refer :all])
  (:refer-clojure :only [=]))

(def default-env nil)

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
