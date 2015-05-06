(ns section-4-1-2.begin
  (:require [section-4-1-2.util :as util])
  (:refer-clojure :only [cond cons defn rest]))

(defn begin? [exp]
  (util/tagged-list? exp 'begin))

(defn begin-actions [exp] (rest exp))

(defn make-begin [seq]
  (cons 'begin seq))

(defn eval-sequence
  "Used for (begin) and for the body of a function--can be more than
  one expression in a row"
  [exps env eval-fn]
  (cond 
    (util/last-exp? exps) (eval-fn (util/first-exp exps) env)
    :else (do
            (eval-fn (util/first-exp exps) env)
            (eval-sequence (util/rest-exps exps) env eval-fn))))

