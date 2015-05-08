(ns section-4-1-2.definition-test
  (:require [clojure.test :refer :all]
            [section-4-1-2.core :refer :all])
  (:refer-clojure :only [=]))

(def default-env (section-4-1-2.environment/extend-environment
                  '()
                  '()
                  section-4-1-2.environment/the-empty-environment))

(deftest definition
  (testing ""
    (is (= (eval '(define a 1) default-env)
           :ok))))

;; XXX get this working
;; (deftest definition_and_use
;;   (testing ""
;;     (is (= (eval '(begin
;;                    (define a 1)
;;                    a)
;;                  default-env)
;;            1))))
