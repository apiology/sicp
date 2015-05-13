(ns section-4-1-2.forms
  (:require [section-4-1-2.and :as and]
            [section-4-1-2.application :as application]
            [section-4-1-2.assignment :as assignment]
            [section-4-1-2.begin :as begin]
            [section-4-1-2.cond :as cond]
            [section-4-1-2.definition :as definition]
            [section-4-1-2.if :as if]
            [section-4-1-2.lambda :as lambda]
            [section-4-1-2.let :as let]
            [section-4-1-2.letstar :as letstar]
            [section-4-1-2.or :as or]
            [section-4-1-2.primitive :as primitive]
            [section-4-1-2.quote :as quote])
  (:refer-clojure :only
                  [-> ->> = > atom comment cond conj cons count declare
                      defn empty? filter first fn if-let if-not let
                      letfn list list? nil? not ns nth number? or
                      println reset! rest second seq str string? swap!
                      symbol?]))

(defn install-all-forms [forms]
  (letfn [(add-form [pred action]
            (swap! forms conj [pred action]))]
    (reset! forms [])
    (add-form primitive/self-evaluating? primitive/eval-primitive)
    (add-form assignment/variable? assignment/lookup-variable-value)
    (add-form quote/quoted? quote/eval-quoted)
    (add-form assignment/assignment? assignment/eval-assignment)
    (add-form definition/definition? definition/eval-definition)
    (add-form if/if? if/eval-if)
    ;; Exercise 4.4
    (add-form and/and? and/eval-and)
    (add-form or/or? or/eval-or)
    (add-form lambda/lambda? lambda/eval-lambda)
    (add-form begin/begin? begin/eval-begin)
    (add-form let/let? let/eval-let)
    (add-form letstar/let*? letstar/eval-let*)
    (add-form cond/cond? cond/eval-cond)
    (add-form application/application? application/eval-application)))
