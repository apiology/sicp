(ns section-4-1-2.namedlet-test
  (:require [section-4-1-2.namedlet :refer :all]
            [clojure.test :refer :all]))


;; XXX get this working
;; (deftest namedlet-advanced
;;   (testing ""
;;     (is (= (eval '(define (fib n)
;;                     (let fib-iter ((a 1)
;;                                    (b 0)
;;                                    (count n))
;;                          (if (= count 0)
;;                            b
;;                            (fib-iter (+ a b) a (- count 1)))))
