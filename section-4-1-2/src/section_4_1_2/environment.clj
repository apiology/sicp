(ns section-4-1-2.environment
  (:require [section-4-1-2.util :as util])
  (:refer-clojure :only [atom cons defn first rest]))

(defn enclosing-environment [env] (rest env))

(defn first-frame [env]
  (first env))

(def the-empty-environment '())

(defn make-frame [variables values]
  (atom (cons variables values)))

(defn frame-variables [frame] (first @frame))

(defn frame-values [frame] (rest @frame))

(defn add-binding-to-frame! [var val frame]
  (util/set-car! frame (cons var (frame-variables frame)))
  (util/set-cdr! frame (cons val (frame-values frame))))

