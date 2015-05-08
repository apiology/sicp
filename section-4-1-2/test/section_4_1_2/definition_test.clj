(ns section-4-1-2.definition-test
  (:require [clojure.test :refer :all]
            [section-4-1-2.core :refer :all]
            [section-4-1-2.test-env :refer [test-env]])
  (:refer-clojure :only [= defn]))

(deftest definition
  (testing ""
    (is (= (eval '(define a 1) (test-env))
           :ok))))

(deftest definition_and_use
  (testing ""
    (is (= (eval '(begin
                   (define a 1)
                   a)
                 (test-env))
           1))))

(deftest redefinition_and_use
  (testing ""
    (is (= (eval '(begin
                   (define a 1)
                   (define a 2)
                   a)
                 (test-env))
           2))))
