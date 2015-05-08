# section-4-1-2

A Clojure library designed to ... well, that part is up to you.

## Usage

FIXME

## License

Copyright © 2015 FIXME

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.

;; Exercise 4.1
(defn list-of-values-left-eval
  "Evaluates each item in the list of expressions and returns a list
  of values back"
  [exps env eval-fn]
  (if (no-operands? exps)
    '()
    (let [left-value (eval-fn (first-operand exps) env)]
      (let [rest-values (list-of-values-left-eval (rest-operands exps) env eval-fn)]
        (cons left-value rest-values)))))

;; Exercise 4.1
(defn list-of-values-right-eval
  "Evaluates each item in the list of expressions and returns a list
  of values back"
  [exps env eval-fn]
  (if (no-operands? exps)
    '()
    (let [rest-values (list-of-values-right-eval (rest-operands exps) env eval-fn)]
      (let [left-value (eval-fn (first-operand exps) env)]
        (cons left-value rest-values)))))

;; Exercise 4.2

;; a

;; Won't work--special forms like (define) need to be special forms
;; and not regular function calls

;; b

;;
;; to do that:
;;
;; (defn application? [exp]
;;   (util/tagged-list? exp 'call))

;; (defn operator [exp]
;;   (second exp))

;; (defn operands [exp]
;;   (rest (rest exp)))

;; Exercise 4.3

