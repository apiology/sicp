(ns section-4-1-2.let
  (:require [section-4-1-2.lambda :as lambda]
            [section-4-1-2.let-clause :as let-clause]
            [section-4-1-2.named-let :as named-let]
            [section-4-1-2.util :as util])
  (:refer-clojure :only
                  [< = and cons count defn empty? first let list map
                     nth println rest second]))

(defn let? [exp]
  (util/tagged-list? exp 'let))

(defn let-clauses [exp]
  (nth exp 1))

(defn let-bound-expression [exp]
  (nth exp 2))

(defn let->combination [exp]
  (if (named-let/named-let? exp)
    (named-let/named-let->combination exp)
    (let [clauses (let-clauses exp)
          bound-expression (let-bound-expression exp)]
      (cons (lambda/make-lambda (let-clause/clauses->lambda-parameters clauses)
                                bound-expression)
            (map let-clause/clause->value clauses)))))
  
(defn eval-let [exp env eval-fn apply-fn]
  (eval-fn (let->combination exp) env))
