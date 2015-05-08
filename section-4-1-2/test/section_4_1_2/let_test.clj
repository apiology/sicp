(ns section-4-1-2.let-test
  (:require [clojure.test :refer :all]
            [section-4-1-2.let :as let])
  (:refer-clojure :only [=]))

(def default-env nil)


(deftest let-clauses-created
  (testing ""
    (is (= (let/clauses->lambda-parameters '((a 1))) '(a)))))

(deftest let-form-created
  (testing ""
    (is (= (let/let->combination '(let ((a 1)) a)) '((lambda (a) a) 1)))))

;; ;; XXX reenable this once I have variable handling
;; (deftest let-super-simple
;;   (testing ""
;;     (is (= (eval '(let ((a 1)) a) default-env) 1))))

