(ns section-4-1-2.core
  (:require [section-4-1-2.begin :as begin]
            [section-4-1-2.forms :as forms]
            [section-4-1-2.procedure :as procedure]
            [section-4-1-2.util :as util])
  (:refer-clojure :only
                  [-> ->> = > atom comment cond conj cons count declare
                      defn empty? filter first fn if-let if-not let
                      list list? nil? not ns nth number? or println
                      reset! rest second seq str string? swap!
                      symbol?]))

(declare eval)

(defn extend-environment [variables values existing-env]
  existing-env)

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

(def forms (atom []))


(forms/install-all-forms forms apply)

;; 

(defn action-for-exp [exp]
  (if-let [[pred action] (->> @forms
                             (filter #((first %) exp))
                             first)]
    action))


(defn eval [exp env]
  (if-let [action (action-for-exp exp)]
    (action exp env eval)
    (util/error "unknown expression type -- EVAL" exp)))
