(ns section-4-1-2.test-env
  (:require [section-4-1-2.environment :as environment]
            [section-4-1-2.global-environment :as global-environment]))

(defn test-env []
  (environment/extend-environment
   '()
   '()
   global-environment/the-global-environment))
