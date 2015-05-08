(ns section-4-1-2.quote-test
  (:require [clojure.test :refer :all]
            [section-4-1-2.core :refer :all])
  (:refer-clojure :only [=]))

(def default-env section-4-1-2.environment/the-empty-environment)

(deftest quote-with-nothing
  (testing ""
    (is (= (eval '(quote 123) default-env)
           123))))

(deftest quote-with-symbol
  (testing ""
    (is (= (eval '(quote a) default-env) 'a))))
