(ns section-4-1-2.quote
  (:require [section-4-1-2.util :as util]))

(defn quoted? [exp]
  (util/tagged-list? exp 'quote))

(defn text-of-quotation [exp]
  (second exp))

(defn eval-quoted [exp env eval-fn apply-fn]
  (text-of-quotation exp))
