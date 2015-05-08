(ns section-4-1-2.assignment
  (:require [section-4-1-2.util :as util])
  (:refer-clojure :only [< = atom cond cons count defn empty? first let letfn nth reset! rest symbol?]))

(defn variable? [exp]
  (symbol? exp))

(defn assignment? [exp]
  (util/tagged-list? exp 'set!))

(defn assignment-variable [exp]
  (nth exp 1))

(defn assignment-value [exp]
  (nth exp 2))

(defn define-variable! [symbol value env]
  (util/error "define-variable! not yet implemented"))

(defn lookup-variable-value [symbol env eval-fn]
  nil)

(defn enclosing-environment [env] (rest env))
(defn first-frame [env]
  (first env))
(def the-empty-environment '())

(defn make-frame [variables values]
  (atom (cons variables values)))

(defn frame-variables [frame] (first @frame))

(defn frame-values [frame] (rest @frame))

(defn set-car! [atom-of-list new-car]
  (reset! atom-of-list (cons new-car (rest @atom-of-list))))

(defn set-cdr! [atom-of-list new-cdr]
  (reset! atom-of-list (cons (first @atom-of-list) new-cdr)))

(defn add-binding-to-frame! [var val frame]
  (set-car! frame (cons var (frame-variables frame)))
  (set-cdr! frame (cons val (frame-values frame))))

(defn extend-environment [vars vals base-env]
  (if (= (count vars) (count vals))
    (cons (make-frame vars vals) base-env)
    (if (< (count vars) (count vals))
      (util/error "Too many arguments supplied" vars vals)
      (util/error "Too few arguments supplied" vars vals))))

(defn set-variable-value! [var val env]
  (letfn [(env-loop [env env]
            (letfn [(scan [vars vals]
                     (cond (empty? vars)
                           (env-loop (enclosing-environment env))

                           (= var (first vars))
                           ;; XXX not sure this is right
                           (set-car! vals val)
                           
                           :else
                           (scan (rest vars) (rest vals))))]
              (if (= env the-empty-environment)
                (util/error "Unbound variable -- SET!" var)
                (let [frame (first-frame env)]
                  (scan (frame-variables frame)
                        (frame-values frame))))))]
    (env-loop env)))

(defn define-variable! [var val env]
  (let [frame (first-frame env)]
    (letfn [(scan [vars vals]
              (cond (empty? vars)
                    (add-binding-to-frame! var val frame)
                    
                    (= var (first vars))
                    (set-car! vals val)
                    
                    :else
                    (scan (rest vars) (rest vals))))]
      (scan (frame-variables frame)
            (frame-values frame)))))

(defn eval-assignment [exp env eval-fn apply-fn]
  (set-variable-value! (assignment-variable exp)
                       (eval-fn (assignment-value exp) env)
                       env)
  :ok)

