(ns section-4-1-2.if-test
  (:require [clojure.test :refer :all]
            [section-4-1-2.core :refer :all]
            [section-4-1-2.test-env :refer [test-env]])
  (:refer-clojure :only [=]))

(deftest if-false
  (testing "if-false"
    (is (= (eval '(if 'false 123 456) (test-env)) 456))))

(deftest if-true
  (testing ""
    (is (= (eval '(if 1 123 456) (test-env)) 123))))

(deftest if-true-without-second
  (testing ""
    (is (= (eval '(if 1 123) (test-env)) 123))))
