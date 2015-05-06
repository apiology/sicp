(ns section-4-1-2.and
  (:require [section-4-1-2.util :as util]))

(defn and? [exp]
  (util/tagged-list? exp 'and))

(defn and-exps [exp]
  (rest exp))
