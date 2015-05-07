(ns section-4-1-2.forms
  (:require [section-4-1-2.and :as and]
            [section-4-1-2.application :as application]
            [section-4-1-2.assignment :as assignment]
            [section-4-1-2.begin :as begin]
            [section-4-1-2.cond :as cond]
            [section-4-1-2.definition :as definition]
            [section-4-1-2.if :as if]
            [section-4-1-2.lambda :as lambda]
            [section-4-1-2.or :as or]
            [section-4-1-2.primitive :as primitive]
            [section-4-1-2.procedure :as procedure]
            [section-4-1-2.quote :as quote])
  (:refer-clojure :only
                  [-> ->> = > atom comment cond conj cons count declare
                      defn empty? filter first fn if-let if-not let
                      letfn list list? nil? not ns nth number? or
                      println reset! rest second seq str string? swap!
                      symbol?]))

(defn install-all-forms [forms apply]
  (letfn [(add-form [pred action]
            (swap! forms conj [pred action]))]
    (reset! forms [])
    (add-form primitive/self-evaluating? (fn [exp env eval-fn] exp))
    (add-form assignment/variable? assignment/lookup-variable-value)
    (add-form quote/quoted? (fn [exp env] (quote/text-of-quotation exp)))
    (add-form assignment/assignment? assignment/eval-assignment)
    (add-form definition/definition? definition/eval-definition)
    (add-form if/if? if/eval-if)
    ;; Exercise 4.5
    (add-form and/and? and/eval-and)
    (add-form or/or? or/eval-or)
    (add-form lambda/lambda? (fn [exp env eval-fn]
                               (procedure/make-procedure (lambda/lambda-parameters exp)
                                                         (lambda/lambda-body exp)
                                                         env)))
    (add-form begin/begin? (fn [exp env eval-fn]
                             (begin/eval-sequence (begin/begin-actions exp) env eval-fn)))
    (add-form cond/cond? (fn [exp env eval-fn]
                           (eval-fn (cond/cond->if exp) env)))
    (add-form application/application? (fn [exp env eval-fn]
                                         (apply (eval-fn (application/operator exp) env)
                                                (application/list-of-values
                                                 (application/operands exp) env eval-fn))))))

