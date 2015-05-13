(ns section-4-1-2.boolean
  (:refer-clojure :only [defn not number? or]))

(defn boolean? [exp]
  (or (clojure.core/true? exp)
      (clojure.core/false? exp)))

(defn true? [cond]
  (not (clojure.core/false? cond)))
