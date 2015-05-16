(ns section-4-1-2.named-let
  (:require [section-4-1-2.lambda :as lambda]
            [section-4-1-2.let-clause :as let-clause]
            [section-4-1-2.util :as util]))

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

    (list 'begin
          (list 'define name
                (lambda/make-lambda (let-clause/clauses->lambda-parameters clauses)
                                    bound-expression))
          (cons name (map let-clause/clause->value clauses)))))

(defn named-let? [exp]
  (and (util/tagged-list? exp 'let)
       (= (count exp) 4)))

