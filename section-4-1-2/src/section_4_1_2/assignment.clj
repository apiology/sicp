(ns section-4-1-2.assignment
  (:require [section-4-1-2.util :as util]))

(defn variable? [exp]
  (symbol? exp))

(defn assignment? [exp]
  (util/tagged-list? exp 'set!))

(defn assignment-variable [exp]
  (nth exp 1))

(defn assignment-value [exp]
  (nth exp 2))

