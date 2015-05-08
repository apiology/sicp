(ns section-4-1-2.environment
  (:require [section-4-1-2.util :as util])
  (:refer-clojure :only [< = atom cons count defn first list rest]))

(defn enclosing-environment [env] (rest env))

(defn first-frame [env]
  (first env))

(defn make-frame [variables values]
  (atom (cons variables values)))

(def the-first-frame (make-frame '() ()))

(def the-empty-environment (list the-first-frame))

(defn frame-variables [frame] (first @frame))

(defn frame-values [frame] (rest @frame))

(defn add-binding-to-frame! [var val frame]
  (util/set-car! frame (cons var (frame-variables frame)))
  (util/set-cdr! frame (cons val (frame-values frame))))

(defn extend-environment [vars vals base-env]
  (if (= (count vars) (count vals))
    (cons (make-frame vars vals) base-env)
    (if (< (count vars) (count vals))
      (util/error "Too many arguments supplied" vars vals)
      (util/error "Too few arguments supplied" vars vals))))
