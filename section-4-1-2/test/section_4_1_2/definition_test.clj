(ns section-4-1-2.definition-test
  (:require [clojure.test :refer :all]
            [section-4-1-2.core :refer :all])
  (:refer-clojure :only [= defn]))

(defn default-env []
  (section-4-1-2.environment/extend-environment
   '()
   '()
   section-4-1-2.environment/the-empty-environment))

(deftest definition
  (testing ""
    (is (= (eval '(define a 1) (default-env))
           :ok))))

(deftest definition_and_use
  (testing ""
    (is (= (eval '(begin
                   (define a 1)
                   a)
                 (default-env))
           1))))

(deftest redefinition_and_use
  (testing ""
    (is (= (eval '(begin
                   (define a 1)
                   (define a 2)
                   a)
                 (default-env))
           2))))
