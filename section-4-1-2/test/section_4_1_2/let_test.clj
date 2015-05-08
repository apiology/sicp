(ns section-4-1-2.let-test
  (:require [clojure.test :refer :all]
            [section-4-1-2.core :refer :all]
            [section-4-1-2.let :as let])
  (:refer-clojure :only [=]))

(def default-env section-4-1-2.assignment/the-empty-environment)


(deftest let-clauses-created
  (testing ""
    (is (= (let/clauses->lambda-parameters '((a 1))) '(a)))))

(deftest let-form-created
  (testing ""
    (is (= (let/let->combination '(let ((a 1)) a)) '((lambda (a) a) 1)))))

(deftest let-super-simple
   (testing ""
     (is (= (eval '(let ((a 1)) a) default-env) 1))))

(deftest let-created-from-let*-basic
  (testing ""
    (is (= (let/let*->nested-lets '(let* ((a 1)) a))
           '(let ((a 1)) a)))))

(deftest let-created-from-let*-advanced
  (testing ""
    (is (= (let/let*->nested-lets '(let* ((a 1) (b 2) (c 3)) c))
           '(let ((a 1)) (let ((b 2)) (let ((c 3)) c)))))))

(deftest let*-super-simple
  (testing ""
    (is (= (eval '(let* ((a 1)) a) default-env) 1))))

;; ;; XXX reenable this once I have variable handling
;; (deftest let*-advanced
;;   (testing ""
;;     (is (= (eval '(let* ((a 1) (b 2) (c 3)) c) default-env)
;;            3))))
