(ns section-4-1-2.core
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
            [section-4-1-2.quote :as quote]
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

(defn add-form [pred action]
  (swap! forms conj [pred action]))

;; XXX just move to three-arg form
(defn install-all-forms []
  (reset! forms [])
  (add-form primitive/self-evaluating? (fn [exp env] exp))
  (add-form assignment/variable? assignment/lookup-variable-value)
  (add-form quote/quoted? (fn [exp env] (quote/text-of-quotation exp)))
  (add-form assignment/assignment? (fn [exp env] (assignment/eval-assignment exp env eval)))
  (add-form definition/definition? (fn [exp env] (definition/eval-definition exp env eval)))
  (add-form if/if? (fn [exp env] (if/eval-if exp env eval)))
  ;; Exercise 4.5
  (add-form and/and? (fn [exp env] (and/eval-and exp env eval)))
  (add-form or/or? (fn [exp env] (or/eval-or exp env eval)))
  (add-form lambda/lambda? (fn [exp env]
                             (procedure/make-procedure (lambda/lambda-parameters exp)
                                                       (lambda/lambda-body exp)
                                                       env)))
  (add-form begin/begin? (fn [exp env]
                           (begin/eval-sequence (begin/begin-actions exp) env eval)))
  (add-form cond/cond? (fn [exp env]
                         (eval (cond/cond->if exp) env)))
  (add-form application/application? (fn [exp env]
                                       (apply (eval (application/operator exp) env)
                                              (application/list-of-values
                                               (application/operands exp) env eval)))))


(install-all-forms)

;; 

(defn action-for-exp [exp]
  (if-let [[pred action] (->> @forms
                             (filter #((first %) exp))
                             first)]
    action))


(defn eval [exp env]
  (if-let [action (action-for-exp exp)]
    (action exp env)
    (util/error "unknown expression type -- EVAL" exp)))
