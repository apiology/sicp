(ns section-4-1-2.environment
  (:require [clojure.core.typed :as t :refer [defn]]
            [section-4-1-2.types :as types]
            [section-4-1-2.util :as util])
  (:refer-clojure :exclude [defn eval]))

;; (clojure.core.typed/check-ns)

(defn enclosing-environment [env :- types/Environment] :- types/Environment
  (rest env))

(defn first-frame [env :- types/Environment] :- (t/Option types/Frame)
  (first env))

(t/ann ^:no-check make-frame [types/Variables types/RawValues -> types/Frame])
(defn make-frame [variables
                  values]
  (atom (list variables (map atom values))))

(t/ann the-empty-environment types/Environment)
(def the-empty-environment '())

(defn frame-variables [frame :- types/Frame] :- types/Variables
  (first @frame))

(defn frame-values [frame :- types/Frame] :- types/Values
  (second @frame))

(defn add-binding-to-frame! [var :- types/Var
                             val :- types/RawVal
                             frame :- types/Frame] :- (t/Value :ok)
  (t/let [new-vars :- types/Variables
          (cons var (frame-variables frame))
          
          new-vals :- types/Values
          (cons (atom val) (frame-values frame))]
    ;; (println "new-vars=" new-vars ", new-vals=" new-vals)
    (util/set-first! frame new-vars)
    (util/set-second! frame new-vals)
    :ok))

(defn extend-environment [vars :- types/Variables
                          vals :- types/RawValues
                          base-env :- types/Environment] :- types/Environment
  (if (= (count vars) (count vals))
    (cons (make-frame vars vals) base-env)
    (if (< (count vars) (count vals))
      (util/error "Too many arguments supplied" vars vals)
      (util/error "Too few arguments supplied" vars vals))))
