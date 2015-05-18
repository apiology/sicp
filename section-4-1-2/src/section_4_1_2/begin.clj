(ns section-4-1-2.begin
  (:require [section-4-1-2.util :as util])
  (:refer-clojure :only [cond cons defn empty? rest]))

(defn begin? [exp]
  (util/tagged-list? exp 'begin))

(defn begin-actions [exp] (rest exp))

(defn make-begin [seq]
  (cons 'begin seq))

(defn sequence->exp [seq]
  (cond 
    (empty? seq) seq
    (util/last-exp? seq) (util/first-exp seq)
    :else (make-begin seq)))

(defn eval-sequence
  "Used for (begin) and for the body of a function--can be more than
  one expression in a row"
  [exps env eval-fn]
  (cond 
    (util/last-exp? exps) (eval-fn (util/first-exp exps) env)
    :else (do
            (eval-fn (util/first-exp exps) env)
            (eval-sequence (util/rest-exps exps) env eval-fn))))

(defn eval-begin [exp env eval-fn apply-fn]
  (eval-sequence (begin-actions exp) env eval-fn))
