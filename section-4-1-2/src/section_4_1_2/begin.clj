(ns section-4-1-2.begin
  (:require [section-4-1-2.util :as util]))

(defn begin? [exp]
  (util/tagged-list? exp 'begin))

(defn begin-actions [exp] (rest exp))

(defn make-begin [seq]
  (cons 'begin seq))

