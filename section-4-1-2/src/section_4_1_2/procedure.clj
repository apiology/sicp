(ns section-4-1-2.procedure
  (:require [section-4-1-2.util :as util])
  (:refer-clojure :only [defn list]))

(defn primitive-procedure? [exp]
  (util/error "primitive-procedure? not yet implemented"))

(defn apply-primitive-procedure [procedure arguments]
  (util/error "apply-primitive-procedure not yet implemented"))

(defn compound-procedure? [exp]
  (util/error "compound-procedure? not yet implemented"))

(defn procedure-body [procedure]
  (util/error "procedure-body not yet implemented"))

(defn procedure-parameters [procedure]
  (util/error "procedure-body not yet implemented"))

(defn procedure-environment [procedure]
  (util/error "procedure-environment not yet implemented"))

(defn make-procedure [parameters body env]
  (list 'procedure parameters body env))
