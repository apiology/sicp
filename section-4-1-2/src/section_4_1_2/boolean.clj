(ns section-4-1-2.boolean
  (:refer-clojure :only [defn or number?])
  (:require [section-4-1-2.util :as util]))

;; false not bound in global environment yet--waiting patiently for
;; section 4.1.4, but in the meantime, this helps test the rest
(defn boolean? [exp]
  (or (clojure.core/true? exp)
      (clojure.core/false? exp)))

(defn true? [cond]
  (if (number? cond)
    (clojure.core/pos? cond)
    (if (clojure.core/true? cond)
      true
      (if (clojure.core/false? cond)
        false
        (util/error "true? not implemented on " cond)))))
