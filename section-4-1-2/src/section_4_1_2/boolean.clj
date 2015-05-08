(ns section-4-1-2.boolean
  (:refer-clojure :only [defn not number? or]))

;; false not bound in global environment yet--waiting patiently for
;; section 4.1.4, but in the meantime, this helps test the rest
(defn boolean? [exp]
  (or (clojure.core/true? exp)
      (clojure.core/false? exp)))

(defn true? [cond]
  (not (clojure.core/false? cond)))
