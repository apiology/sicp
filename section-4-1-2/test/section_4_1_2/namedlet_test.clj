(ns section-4-1-2.namedlet-test
  (:require [clojure.test :refer :all]
            [section-4-1-2.core :refer [eval]]
            [section-4-1-2.let :as let]
            [section-4-1-2.test-env :refer [test-env]])
  (:refer-clojure :only [= first let second]))


(deftest namedlet-translation
   (testing ""
     (is (= (let/named-let->combination '(let fib-iter ((a 1)
                                                        (b 0)
                                                        (count n))
                                              (if (= count 0)
                                                b
                                                (fib-iter (+ a b) a (- count 1)))))
            '(begin (define fib-iter (lambda (a b count) (if (= count 0) b (fib-iter (+ a b) a (- count 1))))) (fib-iter 1 0 n))))))


(deftest namedlet-easy
  (testing ""
    
    (is (= (eval '(let fib-iter ((a 1)
                                 (b 0)
                                 (count 2))
                       (if (= count 0)
                         b
                         (fib-iter (+ a b) a (- count 1))))
                 (test-env))
           1))))

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
           :ok))))

(deftest namedlet-advanced-with-use
  (testing ""
    (is (= (eval '(begin
                   (define (fib n)
                     (let fib-iter ((a 1)
                                    (b 0)
                                    (count n))
                          (if (= count 0)
                            b
                            (fib-iter (+ a b) a (- count 1)))))
                   (fib 6))
                 (test-env))
           8))))

;; 1 1 2 3 5 8
