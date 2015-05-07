(ns section-4-1-2.core-test
  (:require [clojure.test :refer :all]
            [section-4-1-2.core :refer :all])
  (:refer-clojure :only [= comment list]))

(def default-env nil)

(deftest one-equals-one
  (testing "The first rule of tautology club is the first rule of tautology club"
    (is (= (eval 1 default-env) 1))))

;; false not bound in global environment yet--waiting patiently for section 4.1.4.
;; (deftest if-false-without-second
;;   (testing ""
;;    (is (= (eval '(if 0 123) default-env) default-env) 'false))))

;; (deftest one-plus-one-equals-two
;;   (testing "o"
;;     (is (= (eval '(+ 1 1) default-env) 2))))

