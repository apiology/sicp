(ns section-4-1-2.or
  (:require [section-4-1-2.boolean :as boolean]
            [section-4-1-2.util :as util])
  (:refer-clojure :only [cons defn empty? first let rest]))

(defn or? [exp]
  (util/tagged-list? exp 'or))

(defn or-exps [exp]
  (rest exp))

(defn eval-or [exp env eval-fn]
  (let [exps (or-exps exp)]
    (if (empty? exps)
      'false
      (let [left (first exps)
            left-value (eval-fn left env)]
        (if (boolean/true? left-value)
          left-value
          (recur (cons 'or (rest exps)) env eval-fn))))))

