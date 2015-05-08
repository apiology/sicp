(ns section-4-1-2.let
  (:require [section-4-1-2.lambda :as lambda]
            [section-4-1-2.util :as util])
  (:refer-clojure :only [cons defn empty? first let list map nth rest second]))

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

(defn eval-let [exp env eval-fn apply-fn]
  (eval-fn (let->combination exp) env))

(defn let*->nested-lets [exp]
  (let [clauses (let-clauses exp)
        let-bound-expression (let-bound-expression exp)]
    (if (empty? clauses)
        let-bound-expression
        (list 'let (list (first clauses)) (let*->nested-lets (list 'let (rest clauses) let-bound-expression))))))



