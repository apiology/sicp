(ns section-4-1-2.primitive
  (:refer-clojure :only [defn or number? cond string?])
  (:require [section-4-1-2.util :as util]
            [section-4-1-2.boolean :as boolean]))

(defn self-evaluating? [exp]
  (cond
    (number? exp) true
    (string? exp) true
    (boolean/boolean? exp) true
    :else false))



