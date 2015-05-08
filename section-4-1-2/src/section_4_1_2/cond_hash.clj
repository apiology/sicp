(ns section-4-1-2.cond-hash)

(defn cond-hash-clause? [clause]
  (and (= 3 (count clause))
       (= '=> (nth clause 1))))

(defn cond-hash-clause-predicate [clause]
  (first clause))

(defn cond-hash-clause-action [clause]
  (nth clause 2))

(defn make-hash-cond-application [clause]
  (list (cond-hash-clause-action clause)
        (cond-hash-clause-predicate clause)))

