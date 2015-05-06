(ns section-4-1-2.and
  (:require [section-4-1-2.util :as util]
            [section-4-1-2.boolean :as boolean]))

(defn and? [exp]
  (util/tagged-list? exp 'and))

(defn and-exps [exp]
  (rest exp))
