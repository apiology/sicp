(ns section-4-1-2.letstar-test
  (:require [clojure.test :refer :all]
            [section-4-1-2.core :refer :all]
            [section-4-1-2.letstar :as letstar]
            [section-4-1-2.test-env :refer [test-env]])
  (:refer-clojure :only [=]))

(deftest let*-super-simple
  (testing ""
    (is (= (eval '(let* ((a 1)) a) (test-env)) 1))))

(deftest let*-advanced
  (testing ""
    (is (= (eval '(let* ((a 1) (b 2) (c 3)) c) (test-env))
           3))))
