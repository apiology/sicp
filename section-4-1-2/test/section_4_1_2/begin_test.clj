(ns section-4-1-2.begin-test
  (:require [clojure.test :refer :all]
            [section-4-1-2.core :refer [eval]]
            [section-4-1-2.begin :refer :all]
            [section-4-1-2.test-env :refer [test-env]])
  (:refer-clojure :only [=]))

(deftest simple-begin
  (testing ""
    (is (= (eval '(begin 1) (test-env))
           1))))

(deftest two-form-begin
  (testing ""
    (is (= (eval '(begin 1 2) (test-env))
           2))))

(deftest begin-with-define
  (testing ""
    (is (= (eval '(begin
                   (define a 1)
                   a) (test-env))
           1))))

