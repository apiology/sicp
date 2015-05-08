(ns section-4-1-2.core
  (:require [section-4-1-2.assignment :as assignment]
            [section-4-1-2.environment :as environment]
            [section-4-1-2.begin :as begin]
            [section-4-1-2.forms :as forms]
            [section-4-1-2.procedure :as procedure]
            [section-4-1-2.util :as util])
  (:refer-clojure :only
                  [-> ->> = < > atom comment cond conj cons count declare
                      defn empty? filter first fn if-let if-not let
                      list list? nil? not ns nth number? or println
                      reset! rest second seq str string? swap!
                      symbol?]))

(declare eval)

(defn extend-environment [vars vals base-env]
  (if (= (count vars) (count vals))
    (cons (environment/make-frame vars vals) base-env)
    (if (< (count vars) (count vals))
      (util/error "Too many arguments supplied" vars vals)
      (util/error "Too few arguments supplied" vars vals))))

(defn apply [procedure arguments]
  (cond
    (procedure/primitive-procedure? procedure)
    (procedure/apply-primitive-procedure procedure arguments)
    
    (procedure/compound-procedure? procedure)
    (begin/eval-sequence
     (procedure/procedure-body procedure)
     (extend-environment
      (procedure/procedure-parameters procedure)
      arguments
      (procedure/procedure-environment procedure))
     eval)
    :else (util/error "Unknown procedure type -- APPLY" procedure)))

;; this vector contains pairs of predicate functions and action
;; functions to identify each form
(def forms (atom []))

(forms/install-all-forms forms)

(defn action-for-exp [exp]
  (if-let [[pred action] (->> @forms
                             (filter #((first %) exp))
                             first)]
    action))

(defn eval [exp env]
  (if-let [action (action-for-exp exp)]
    (action exp env eval apply)
    (util/error "unknown expression type -- EVAL" exp)))
