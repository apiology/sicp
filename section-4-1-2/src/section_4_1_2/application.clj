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

(defn list-of-values
  "Evaluates each item in the list of expressions and returns a list
  of values back"
  [exps env]
  (if (no-operands? exps)
    '()
    (cons (eval (first-operand exps) env)
          (list-of-values (rest-operands exps) env))))

;; Exercise 4.1
(defn list-of-values-left-eval
  "Evaluates each item in the list of expressions and returns a list
  of values back"
  [exps env]
  (if (no-operands? exps)
    '()
    (let [left-value (eval (first-operand exps) env)]
      (let [rest-values (list-of-values (rest-operands exps) env)]
        (cons left-value rest-values)))))

;; Exercise 4.1
(defn list-of-values-right-eval
  "Evaluates each item in the list of expressions and returns a list
  of values back"
  [exps env]
  (if (no-operands? exps)
    '()
    (let [rest-values (list-of-values (rest-operands exps) env)]
      (let [left-value (eval (first-operand exps) env)]
        (cons left-value rest-values)))))

