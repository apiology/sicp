(ns section-4-1-2.assignment
  (:require [clojure.core.typed :as t :refer [defn]]
            [section-4-1-2.binding :as binding]
            [section-4-1-2.environment :as environment]
            [section-4-1-2.types :as types]
            [section-4-1-2.util :as util])
  (:refer-clojure :exclude [defn eval]))

;; (clojure.core.typed/check-ns)

(defn variable? [exp :- types/Expression] :- Boolean
  (symbol? exp))

(defn assignment? [exp :- types/Expression] :- Boolean
  (util/tagged-list? exp 'set!))

(t/defalias Assignment (t/HSeq [(t/Value 'set) types/Var types/Expression]))

(defn assignment-variable [exp :- Assignment] :- types/Var
  (nth exp 1))

(defn assignment-value [exp :- Assignment] :- types/Expression
  (nth exp 2))

(defn eval-assignment [exp :- Assignment
                       env :- types/Environment
                       eval-fn :- types/EvalFn
                       apply-fn :- types/ApplyFn] :- (t/Val :ok)
  (binding/set-variable-value! (assignment-variable exp)
                               (eval-fn (assignment-value exp) env)
                               env))
