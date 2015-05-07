(ns section-4-1-2.core
  (:require [section-4-1-2.and :as and]
            [section-4-1-2.application :as application]
            [section-4-1-2.assignment :as assignment]
            [section-4-1-2.begin :as begin]
            [section-4-1-2.boolean :as boolean]
            [section-4-1-2.cond :as cond]
            [section-4-1-2.definition :as definition]
            [section-4-1-2.if :as if]
            [section-4-1-2.lambda :as lambda]
            [section-4-1-2.or :as or]
            [section-4-1-2.primitive :as primitive]
            [section-4-1-2.quote :as quote]
            [section-4-1-2.util :as util])
  (:refer-clojure :only
                  [-> ->> = > atom comment cond conj cons count declare
                      defn empty? filter first fn if-let if-not let
                      list list? nil? not ns nth number? or println
                      reset! rest second seq str string? swap!
                      symbol?]))

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))

(declare eval)

(defn eval-definition [exp env]
  (assignment/define-variable! (definition/definition-variable exp)
    (eval (definition/definition-value exp) env)
    env)
  :ok)

(defn primitive-procedure? [exp]
  (util/error "primitive-procedure? not yet implemented"))

(defn apply-primitive-procedure [procedure arguments]
  (util/error "apply-primitive-procedure not yet implemented"))

(defn compound-procedure? [exp]
  (util/error "compound-procedure? not yet implemented"))

(defn procedure-body [procedure]
  (util/error "procedure-body not yet implemented"))

(defn procedure-parameters [procedure]
  (util/error "procedure-body not yet implemented"))

(defn procedure-environment [procedure]
  (util/error "procedure-environment not yet implemented"))

(defn make-procedure [parameters body env]
  (util/error "make-procedure not yet implemented"))

(defn extend-environment [variables values existing-env]
  (util/error "extend-environment not yet implemented"))

(defn apply [procedure arguments]
  (cond
    (primitive-procedure? procedure) (apply-primitive-procedure procedure arguments)
    (compound-procedure? procedure) (begin/eval-sequence
                                     (procedure-body procedure)
                                     (extend-environment
                                      (procedure-parameters procedure)
                                      arguments
                                      (procedure-environment procedure))
                                     eval)
    :else (util/error "Unknown procedure type -- APPLY" procedure)))


(defn eval-and [exp env]
  (let [exps (and/and-exps exp)]
    (if (empty? exps)
      'true
      (let [left (first exps)
            rest-exps (rest exps)
            left-value (eval left env)]
        (if (boolean/true? left-value)
          (if (empty? rest-exps)
            left-value
            (recur (cons 'and rest-exps) env))
          false)))))


(defn eval-if [exp env]
  (if (boolean/true? (eval (if/if-predicate exp) env))
    (eval (if/if-consequent exp) env)
    (eval (if/if-alternative exp) env)))


(defn eval-or [exp env]
  (let [exps (or/or-exps exp)]
    (if (empty? exps)
      'false
      (let [left (first exps)
            left-value (eval left env)]
        (if (boolean/true? left-value)
          left-value
          (eval-or (cons 'or (rest exps)) env))))))

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

(defn install-all-forms []
  (reset! forms [])
  (add-form primitive/self-evaluating? (fn [exp env] exp))
  (add-form assignment/variable? assignment/lookup-variable-value)
  (add-form quote/quoted? (fn [exp env] (quote/text-of-quotation exp)))
  (add-form assignment/assignment? (fn [exp env] (assignment/eval-assignment exp env eval)))
  (add-form definition/definition? eval-definition)
  (add-form if/if? eval-if)
  ;; Exercise 4.5
  (add-form and/and? eval-and)
  (add-form or/or? eval-or)
  (add-form lambda/lambda? (fn [exp env]
                             (make-procedure (lambda/lambda-parameters exp)
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
