(ns section-4-1-2.binding
  (:require [clojure.core.typed :as t :refer [defn]]
            [section-4-1-2.environment :as environment]
            [section-4-1-2.types :as types]
            [section-4-1-2.util :as util])
  (:refer-clojure :exclude [defn eval]))

(def ^{:preserve [types/Expression]} from-slamhound true)

;; Use t/defn instead
(t/ann scan-for-variable [types/Var types/Environment [types/Value -> types/RawVal]-> types/RawVal])

(defn scan-for-variable [var env f]
  (t/letfn> [scan :- [types/Variables types/Values -> types/RawVal]
             (scan [vars vals]
                   ;; (println "Scanning vars=" vars ", vals=" vals)
                   (cond (empty? vars)
                         (scan-for-variable var (environment/enclosing-environment env) f)
                         
                         (= var (first vars))
                         (let [first-val (first vals)
                               _ (assert (not (nil? first-val)))]
                           (f first-val))
                         
                         :else
                         (recur (rest vars) (rest vals))))]
            (if (or (= env environment/the-empty-environment)
                    (nil? env))
              (util/error "Unbound variable " var)
              (let [frame (environment/first-frame env)
                    _ (assert (not (nil? frame)))]
                ;; (println "frame is " frame)
                (scan (environment/frame-variables frame)
                      (environment/frame-values frame))))))

(defn value->rawval [val :- types/Value] :- types/RawVal
  (deref val))

(defn lookup-variable-value [var :- types/Var
                             env :- types/Environment
                             eval-fn :- types/EvalFn
                             apply-fn :- types/ApplyFn] :- types/RawVal
  ;; (println "Lookup-variable-value ")
  (scan-for-variable var env value->rawval))

(defn set-variable-value! [var :- types/Var
                           val :- types/RawVal
                           env :- types/Environment]
  (t/letfn> [env-loop :- [types/Environment -> (t/Value :ok)]
             (env-loop [env]
            (t/letfn> [scan :- [types/Variables types/Values -> (t/Value :ok)]
                       (scan [vars vals]
                      (cond (empty? vars)
                            (env-loop (environment/enclosing-environment
                                       env))

                            (= var (first vars))
                            (let [first-val (first vals)
                                  _ (assert (not (nil? first-val)))]
                              (reset! first-val val)
                              :ok)
                            
                            :else
                            (recur (rest vars) (rest vals))))]
              (if (= env environment/the-empty-environment)
                (util/error "Unbound variable -- SET!" var)
                (let [frame (environment/first-frame env)
                      _ (assert (not (nil? frame)))]
                  (scan (environment/frame-variables frame)
                        (environment/frame-values frame))))))]
        (env-loop env)))

(defn define-variable! [var :- types/Var
                        val :- types/RawVal
                        env :- types/Environment]
  (let [frame (environment/first-frame env)
        _ (assert (not (nil? frame)))]
    (t/letfn> [scan :- [types/Variables types/Values -> (t/Value :ok)]
               (scan [vars vals]
              ;; (println "scanning vars=" vars ", vals=" vals)
              (cond (empty? vars)
                    (environment/add-binding-to-frame! var val frame)
                    
                    (= var (first vars))
                    (let [first-val (first vals)
                          _ (assert (not (nil? first-val)))]
                      (reset! first-val val)
                      :ok)
                      
                    
                    :else
                    (recur (rest vars) (rest vals))))]
      (scan (environment/frame-variables frame)
            (environment/frame-values frame)))))
