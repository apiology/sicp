(ns section-4-1-2.or
  (:require [section-4-1-2.util :as util]
            [section-4-1-2.boolean :as boolean]))

(defn or? [exp]
  (util/tagged-list? exp 'or))

(defn or-exps [exp]
  (rest exp))

(defn eval-or [exp env]
  (let [exps (or-exps exp)]
    (if (empty? exps)
      'false
      (let [left (first exps)
            left-value (eval left env)]
        (if (boolean/true? left-value)
          left-value
          (eval-or (cons 'or (rest exps)) env))))))

