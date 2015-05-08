(ns section-4-1-2.lambda
  (:require [section-4-1-2.procedure :as procedure]
            [section-4-1-2.util :as util]))

(defn lambda? [exp]
  (util/tagged-list? exp 'lambda))

(defn lambda-parameters [exp]
  (nth exp 1))

(defn lambda-body [exp]
  (rest (rest exp)))

(defn make-lambda [parameters body]
  (cons 'lambda (cons parameters (list body))))

(defn eval-lambda [exp env eval-fn apply-fn]
  (procedure/make-procedure (lambda-parameters exp)
                            (lambda-body exp)
                            env))
