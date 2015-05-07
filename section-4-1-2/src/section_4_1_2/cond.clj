(ns section-4-1-2.cond
  (:require [section-4-1-2.begin :as begin]
            [section-4-1-2.if :as if]
            [section-4-1-2.util :as util]))

(defn cond? [exp]
  (util/tagged-list? exp 'cond))

(defn cond-clauses [exp]
  (rest exp))

(defn cond-predicate [clause]
  (first clause))

(defn cond-else-clause? [clause]
  (= (cond-predicate clause) 'else))

(defn cond-actions [clause]
  (rest clause))

(defn sequence->exp [seq]
  (cond 
    (empty? seq) seq
    (util/last-exp? seq) (util/first-exp seq)
    :else (begin/make-begin seq)))

(defn expand-clauses [clauses]
  (if (empty? clauses)
    'false
    (let [first-clause (first clauses)
          rest-clauses (rest clauses)]
      (if (cond-else-clause? first-clause)
        (if (empty? rest-clauses)
          (sequence->exp (cond-actions first-clause))
          (util/error "ELSE clause isn't last -- COND->IF" clauses))
        (if/make-if (cond-predicate first-clause)
                    (sequence->exp (cond-actions first-clause))
                    (expand-clauses rest-clauses))))))

(defn cond->if [exp]
  (expand-clauses (cond-clauses exp)))