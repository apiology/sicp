(ns section-4-1-2.test-env
  (:require [section-4-1-2.environment :refer :all]
            [clojure.test :refer :all]))

(defn test-env []
  (section-4-1-2.environment/extend-environment
   '()
   '()
   section-4-1-2.environment/the-empty-environment))
