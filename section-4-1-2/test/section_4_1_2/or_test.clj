(ns section-4-1-2.or-test
  (:require [clojure.test :refer :all]
            [section-4-1-2.test-env :refer [test-env]]
            [section-4-1-2.core :refer :all])
  (:refer-clojure :only [=]))

(deftest or-with-nothing
  (testing ""
    (is (= (eval '(or) (test-env)) false))))

(deftest or-with-one-false-arg
  (testing ""
    (is (= (eval '(or 'false) (test-env)) false))))

(deftest or-with-one-true-arg
  (testing ""
    (is (= (eval '(or 1) (test-env)) 1))))

(deftest or-with-one-false-and-one-true-arg
  (testing ""
    (is (= (eval '(or 'false 1) (test-env)) 1))))

(deftest or-with-many-false-and-one-true-arg
  (testing ""
    (is (= (eval '(or 'false 'false 'false 12) (test-env)) 12))))

(deftest or-short-circuits
  (testing ""
    (is (= (eval '(or 'false 'false 'false 12 unknown-symbol) (test-env)) 12))))
