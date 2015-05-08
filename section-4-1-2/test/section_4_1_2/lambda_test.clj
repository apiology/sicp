(ns section-4-1-2.lambda-test
  (:require [clojure.test :refer :all]
            [section-4-1-2.core :refer :all]
            [section-4-1-2.lambda :as lambda])
  (:refer-clojure :only [= list]))

(def default-env section-4-1-2.environment/the-empty-environment)

(deftest simple-lambda
  (testing ""
    (is (= (eval '(lambda (x) 1) default-env)
           (list 'procedure '(x) '(1) default-env)))))

(deftest lambda-application
  (testing ""
    (is (= (eval '((lambda (x) 1) 123) default-env)
           1))))

(deftest lambda-application-on-variable
  (testing ""
    (is (= (eval '((lambda (x) x) 123) default-env)
            123))))

(deftest lambda-make-lambda-simple
  (testing ""
    (is (= (lambda/make-lambda '(a) '(foo a))
           '(lambda (a) (foo a))))))

(deftest lambda-make-lambda
  (testing ""
    (is (= (lambda/make-lambda '(a) 'a)
           '(lambda (a) a)))))

