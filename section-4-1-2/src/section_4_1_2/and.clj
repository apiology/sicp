(ns section-4-1-2.and
  (:require [section-4-1-2.boolean :as boolean]
            [section-4-1-2.util :as util])
  (:refer-clojure :only [cons defn empty? first let rest]))

(defn and? [exp]
  (util/tagged-list? exp 'and))

(defn and-exps [exp]
  (rest exp))

(defn eval-and [exp env eval-fn apply-fn]
  (let [exps (and-exps exp)]
    (if (empty? exps)
      'true
      (let [left (first exps)
            rest-exps (rest exps)
            left-value (eval-fn left env)]
        (if (boolean/true? left-value)
          (if (empty? rest-exps)
            left-value
            (recur (cons 'and rest-exps) env eval-fn apply-fn))
          false)))))
