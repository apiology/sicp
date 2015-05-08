(ns section-4-1-2.application
  (:refer-clojure :only [cons defn empty? first let println rest seq?]))

(defn application? [exp]
  (seq? exp))

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

(defn list-of-values
  "Evaluates each item in the list of expressions and returns a list
  of values back"
  [exps env eval-fn]
  (if (no-operands? exps)
    '()
    (cons (eval-fn (first-operand exps) env)
          (list-of-values (rest-operands exps) env eval-fn))))

(defn eval-application [exp env eval-fn apply-fn]
  (apply-fn (eval-fn (operator exp) env)
            (list-of-values
             (operands exp) env eval-fn)))
