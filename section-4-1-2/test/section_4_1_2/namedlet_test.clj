(ns section-4-1-2.namedlet-test
  (:require [section-4-1-2.let :refer :all]
            [section-4-1-2.test-env :refer [test-env]]
            [section-4-1-2.core :refer :all]
            [clojure.test :refer :all]))

;; XXX get this working
(deftest namedlet-advanced
   (testing ""
     (is (= (eval '(define (fib n)
                     (let fib-iter ((a 1)
                                    (b 0)
                                    (count n))
                          (if (= count 0)
                            b
                            (fib-iter (+ a b) a (- count 1)))))
                  (test-env))
            0))))
