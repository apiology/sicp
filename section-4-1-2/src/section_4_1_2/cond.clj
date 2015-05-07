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

(defn cond-hash-clause? [clause]
  (and (= 3 (count clause))
       (= '=> (nth clause 1))))

(defn cond-hash-clause-predicate [clause]
  (first clause))

(defn cond-hash-clause-action [clause]
  (nth clause 2))

(defn make-hash-cond-application [clause]
  (list (cond-hash-clause-action clause) (cond-hash-clause-predicate clause)))

(defn expand-clauses [clauses]
  (if (empty? clauses)
    'false
    (let [first-clause (first clauses)
          rest-clauses (rest clauses)]
      (if (cond-else-clause? first-clause)
        (if (empty? rest-clauses)
          (sequence->exp (cond-actions first-clause))
          (util/error "ELSE clause isn't last -- COND->IF" clauses))
        (if (cond-hash-clause? first-clause)
          ;; XXX sure would be nicer if this used a let and a gemsym
          ;; and didn't evaluate predicate twice
          (if/make-if (cond-hash-clause-predicate first-clause)
                      (make-hash-cond-application first-clause)
                      (expand-clauses rest-clauses))
          (if/make-if (cond-predicate first-clause)
                      (sequence->exp (cond-actions first-clause))
                      (expand-clauses rest-clauses)))))))

(defn cond->if [exp]
  (expand-clauses (cond-clauses exp)))
