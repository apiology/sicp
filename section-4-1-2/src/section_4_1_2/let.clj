(ns section-4-1-2.let
  (:require [section-4-1-2.lambda :as lambda]
            [section-4-1-2.util :as util])
  (:refer-clojure :only
                  [= < and cons count defn empty? first let list map nth println rest
                        second]))

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

(defn named-let-name [exp]
  (second exp))

(defn named-let-clauses [exp]
  (nth exp 2))

(defn named-let-bound-expression [exp]
  (nth exp 3))

(defn named-let->combination [exp]
  (let [name (named-let-name exp)
        clauses (named-let-clauses exp)
        bound-expression (named-let-bound-expression exp)]

    (list 'let
          (list (list name
                      (lambda/make-lambda (clauses->lambda-parameters clauses)
                                          bound-expression)))
          (cons name (map clause->value clauses)))))

(defn named-let? [exp]
  (and (let? exp) (= (count exp) 4)))

(defn let->combination [exp]
  (if (named-let? exp)
    (named-let->combination exp)
    (let [clauses (let-clauses exp)
          bound-expression (let-bound-expression exp)]
      (cons (lambda/make-lambda (clauses->lambda-parameters clauses)
                                bound-expression)
            (map clause->value clauses)))))
  
(defn eval-let [exp env eval-fn apply-fn]
  (eval-fn (let->combination exp) env))
