(ns section-4-1-2.namedlet-test
  (:require [section-4-1-2.environment :as environment]
            [section-4-1-2.let :as let]
            [section-4-1-2.test-env :refer [test-env]]
            [section-4-1-2.core :refer [eval]]
            [clojure.test :refer :all])
  (:refer-clojure :only [= first let second]))


(deftest namedlet-translation
   (testing ""
     (is (= (let/named-let->combination '(let fib-iter ((a 1)
                                                        (b 0)
                                                        (count n))
                                              (if (= count 0)
                                                b
                                                (fib-iter (+ a b) a (- count 1)))))
            '(let ((fib-iter (lambda (a b count) (if (= count 0) b (fib-iter (+ a b) a (- count 1)))))) (fib-iter 1 0 n))
            ))))

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

;; (environment/first-frame (test-env))

;; XXX get this working
;; (deftest namedlet-advanced-with-use
;;   (testing ""
;;     (is (= (eval '(begin
;;                    (define (fib n)
;;                      (let fib-iter ((a 1)
;;                                     (b 0)
;;                                     (count n))
;;                           (if (= count 0)
;;                             b
;;                             (fib-iter (+ a b) a (- count 1)))))
;;                    (fib 2)
;;                    1)
;;                  (test-env))
;;            :ok))))
