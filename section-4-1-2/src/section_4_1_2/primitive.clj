(ns section-4-1-2.primitive
  (:require [section-4-1-2.boolean :as boolean])
  (:refer-clojure :only [cond defn number? or string?]))

(defn self-evaluating? [exp]
  (cond
    (number? exp) true
    (string? exp) true
    (boolean/boolean? exp) true
    :else false))

(defn eval-primitive [exp env eval-fn apply-fn]
  exp)
