(ns section-4-1-2.letstar
  (:require [section-4-1-2.let :as let]
            [section-4-1-2.util :as util])
  (:refer-clojure :only
                  [cons defn empty? first let list map nth println rest
                        second]))

(defn let*? [exp]
  (util/tagged-list? exp 'let*))

(defn let*->nested-lets [exp]
  (let [clauses (let/let-clauses exp)
        let-bound-expression (let/let-bound-expression exp)]
    (if (empty? clauses)
        let-bound-expression
        (list 'let (list (first clauses)) (let*->nested-lets (list 'let (rest clauses) let-bound-expression))))))

(defn eval-let* [exp env eval-fn apply-fn]
  (eval-fn (let*->nested-lets exp) env))
