(ns section-4-1-2.let
  (:require [section-4-1-2.lambda :as lambda]
            [section-4-1-2.util :as util])
  (:refer-clojure :only [defn]))

(defn let? [exp]
  (util/tagged-list? exp 'let))

(defn let-clauses [exp]
  (nth exp 1))

(defn let-bound-expression [exp]
  (nth exp 2))

(defn clause->variable [clause]
  (first clause))

(defn clause->value [clause]
  (second clause))

(defn clauses->lambda-parameters [clauses]
  (map clause->variable clauses))

(defn let->combination [exp]
  (let [clauses (let-clauses exp)
        bound-expression (let-bound-expression exp)]
    (cons (lambda/make-lambda (clauses->lambda-parameters clauses)
                              bound-expression)
          (map clause->value clauses))))

(defn eval-let [exp env eval-fn]
  (eval-fn (let->combination exp) env))
