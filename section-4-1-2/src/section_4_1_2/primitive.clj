(ns section-4-1-2.primitive
  (:refer-clojure :only [cond defn number? or string?]))

(defn self-evaluating? [exp]
  (cond
    (number? exp) true
    (string? exp) true
    :else false))

(defn eval-primitive [exp env eval-fn apply-fn]
  exp)
