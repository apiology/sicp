(ns section-4-1-2.assignment
  (:require [section-4-1-2.util :as util])
  (:refer-clojure :only [defn nth symbol?]))

(defn variable? [exp]
  (symbol? exp))

(defn assignment? [exp]
  (util/tagged-list? exp 'set!))

(defn assignment-variable [exp]
  (nth exp 1))

(defn assignment-value [exp]
  (nth exp 2))

(defn set-variable-value! [symbol value env]
  (util/error "set-variable-value! not yet implemented"))

(defn define-variable! [symbol value env]
  (util/error "define-variable! not yet implemented"))

(defn lookup-variable-value [symbol env]
  nil)

(defn eval-assignment [exp env eval-fn]
  (set-variable-value! (assignment-variable exp)
                       (eval-fn (assignment-value exp) env)
                       env)
  :ok)
