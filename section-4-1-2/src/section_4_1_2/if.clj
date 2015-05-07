(ns section-4-1-2.if
  (:require [section-4-1-2.boolean :as boolean]
            [section-4-1-2.util :as util])
  (:refer-clojure :only
                  [-> ->> = > atom comment cond conj cons count declare
                      defn empty? filter first fn if-let if-not let
                      list list? nil? not ns nth number? or println
                      reset! rest second seq str string? swap!
                      symbol?]))

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

(defn eval-if [exp env eval-fn]
  (if (boolean/true? (eval-fn (if-predicate exp) env))
    (eval-fn (if-consequent exp) env)
    (eval-fn (if-alternative exp) env)))
