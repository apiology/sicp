(ns section-4-1-2.definition
  (:require [section-4-1-2.lambda :as lambda]
            [section-4-1-2.util :as util]))

(defn definition? [exp]
  (util/tagged-list? exp 'define))

(defn definition-variable [exp]
  (if (symbol? (second exp))
    ;; variable definition
    (second exp)
    ;; procedure definition
    (first (second exp))))

(defn definition-value [exp]
  (if (symbol? (second exp))
    ;; variable definition
    (nth exp 2)
    ;; procedure definition
    (lambda/make-lambda (rest (nth exp 1)) ;; formal parameters
                        (nth exp 2)))) ;; body
