(ns section-4-1-2.lambda-test
  (:require [section-4-1-2.lambda :refer :all]
            [clojure.test :refer :all]))

(def default-env nil)

(deftest simple-lambda
  (testing ""
    (is (= (eval '(lambda (x) 1) default-env)
           (list 'procedure '(x) 1 default-env)))))

(deftest lambda-application
  (testing ""
    (is (= (eval '((lambda (x) 1) 123) default-env)
           1))))