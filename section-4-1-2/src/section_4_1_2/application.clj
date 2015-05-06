(ns section-4-1-2.application)

(defn application? [exp]
  (list? exp))

(defn operator [exp]
  (first exp))

(defn operands [exp]
  (rest exp))

(defn no-operands? [ops]
  (empty? ops))

(defn first-operand [ops]
  (first ops))

(defn rest-operands [ops]
  (rest ops))

