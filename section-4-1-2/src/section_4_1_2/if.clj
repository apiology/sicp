(ns section-4-1-2.if
  (:require [section-4-1-2.util :as util]
            [section-4-1-2.boolean :as boolean]))

(defn if? [exp]
  (util/tagged-list? exp 'if))

(defn if-predicate [exp]
  (nth exp 1))

(defn if-consequent [exp]
  (nth exp 2))

(defn if-alternative [exp]
  (if (> (count exp) 3)
    (nth exp 3)
    false))
;; (if-alternative '(if 1 true))

(defn make-if [predicate consequent alternative]
  (list 'if predicate consequent alternative))

(defn eval-if [exp env]
  (if (boolean/true? (eval (if-predicate exp) env))
    (eval (if-consequent exp) env)
    (eval (if-alternative exp) env)))

