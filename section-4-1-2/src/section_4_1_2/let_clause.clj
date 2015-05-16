(ns section-4-1-2.let-clause)

(defn clause->variable [clause]
  (first clause))

(defn clause->value [clause]
  (second clause))

(defn clauses->lambda-parameters [clauses]
  (map clause->variable clauses))

