(ns section-4-1-2.procedure
  (:require [section-4-1-2.util :as util])
  (:refer-clojure :only [defn list nth second]))

(defn primitive-procedure? [exp]
  (util/tagged-list? exp 'primitive))

(defn primitive-implementation [proc]
  (second proc))

(defn apply-primitive-procedure [procedure arguments]
  (clojure.core/apply
   (primitive-implementation procedure) arguments))

(defn compound-procedure? [exp]
  (util/tagged-list? exp 'procedure))

(defn procedure-parameters [procedure]
  (nth procedure 1))

(defn procedure-body [procedure]
  (nth procedure 2))

(defn procedure-environment [procedure]
  (nth procedure 3))

(defn make-procedure [parameters body env]
  (list 'procedure parameters body env))
