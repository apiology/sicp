(ns section-4-1-2.or
  (:require [section-4-1-2.util :as util]))

(defn or? [exp]
  (util/tagged-list? exp 'or))

(defn or-exps [exp]
  (rest exp))
