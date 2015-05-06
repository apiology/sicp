(ns section-4-1-2.and
  (:require [section-4-1-2.util :as util]
            [section-4-1-2.boolean :as boolean]))

(defn and? [exp]
  (util/tagged-list? exp 'and))

(defn and-exps [exp]
  (rest exp))

(defn eval-and [exp env]
  (let [exps (and-exps exp)]
    (if (empty? exps)
      'true
      (let [left (first exps)
            rest-exps (rest exps)
            left-value (eval left env)]
        (if (boolean/true? left-value)
          (if (empty? rest-exps)
            left-value
            (recur (cons 'and rest-exps) env))
          false)))))
    
