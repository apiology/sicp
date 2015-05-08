(ns section-4-1-2.assignment
  (:require [section-4-1-2.environment :as environment]
            [section-4-1-2.util :as util])
  (:refer-clojure :only
                  [< = atom cond cons count defn deref empty? first let letfn
                     nth println reset! rest symbol?]))

(defn variable? [exp]
  (symbol? exp))

(defn assignment? [exp]
  (util/tagged-list? exp 'set!))

(defn assignment-variable [exp]
  (nth exp 1))

(defn assignment-value [exp]
  (nth exp 2))

(defn lookup-variable-value [var env eval-fn apply-fn]
  (letfn [(env-loop [env]
            (letfn [(scan [vars vals]
                      (cond (empty? vars)
                            (env-loop (environment/enclosing-environment env))
                            
                            (= var (first vars))
                            (deref (first vals))
                            
                            :else
                            (scan (rest vars) (rest vals))))]
              (if (= env environment/the-empty-environment)
                (util/error "Unbound variable " var)
                (let [frame (environment/first-frame env)]
                  (scan (environment/frame-variables frame)
                        (environment/frame-values frame))))))]
    (env-loop env)))

(defn set-variable-value! [var val env]
  (letfn [(env-loop [env]
            (letfn [(scan [vars vals]
                      (cond (empty? vars)
                            (env-loop (environment/enclosing-environment
                                       env))

                            (= var (first vars))
                            (reset! (first vals) val)
                            
                            :else
                            (scan (rest vars) (rest vals))))]
              (if (= env environment/the-empty-environment)
                (util/error "Unbound variable -- SET!" var)
                (let [frame (environment/first-frame env)]
                  (scan (environment/frame-variables frame)
                        (environment/frame-values frame))))))]
        (env-loop env)))

(defn define-variable! [var val env]
  (let [frame (environment/first-frame env)]
    (letfn [(scan [vars vals]
              (cond (empty? vars)
                    (environment/add-binding-to-frame! var val frame)
                    
                    (= var (first vars))
                    (reset! (first vals) val)
                    
                    :else
                    (scan (rest vars) (rest vals))))]
      (scan (environment/frame-variables frame)
            (environment/frame-values frame)))))

(defn eval-assignment [exp env eval-fn apply-fn]
  (set-variable-value! (assignment-variable exp)
                       (eval-fn (assignment-value exp) env)
                       env)
  :ok)
