(ns section-2-5-3.clj-number
  (:gen-class)
  (:require [section-2-5-3.module :refer :all]
            [clojure.math.numeric-tower :as math]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; clj-number module
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn install-clj-number-package []
  (let [tag #(attach-tag :clj-number %)]
    (put-op :add '(:clj-number :clj-number) #(tag (+ %1 %2)))
    (put-op :negate '(:clj-number) #(tag (- %1)))
    (put-op :sub '(:clj-number :clj-number) #(tag (- %1 %2)))
    (put-op :mul '(:clj-number :clj-number) #(tag (* %1 %2)))
    (put-op :div '(:clj-number :clj-number) #(tag (/ %1 %2)))
    (put-op :lt '(:clj-number :clj-number) #(< %1 %2))
    (put-op :gt '(:clj-number :clj-number) #(> %1 %2))
    (put-op :equ? '(:clj-number :clj-number) =)
    (put-op :atan2 '(:clj-number :clj-number) #(Math/atan2 %1 %2))
    (put-op :exp '(:clj-number :clj-number) #(tag (math/expt %1 %2)))
    (put-op :=zero? '(:clj-number) #(zero? %1))
    (put-op :make :clj-number #(tag %)))
  :done)
