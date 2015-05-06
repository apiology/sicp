(ns section-4-1-2.core-test
  (:refer-clojure :only [= comment])
  (:require [clojure.test :refer :all]
            [section-4-1-2.core :refer :all]))

(def default-env nil)
(deftest one-equals-one
  (testing "The first rule of tautology club is the first rule of tautology club"
    (is (= (eval 1 default-env) 1))))

;; (deftest one-plus-one-equals-two
;;   (testing ""
;;     (is (= (eval '(+ 1 1) default-env) 2))))
