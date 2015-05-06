(ns section-4-1-2.lambda
  (:require [section-4-1-2.util :as util]))

(defn lambda? [exp]
  (util/tagged-list? exp 'lambda))

(defn lambda-parameters [exp]
  (nth exp 1))

(defn lambda-body [exp]
  (nth exp 2))

(defn make-lambda [parameters body]
  (cons 'lambda (cons parameters body)))
