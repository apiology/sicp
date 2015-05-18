(ns section-4-1-2.do
  (:require [section-4-1-2.begin :as begin]
            [section-4-1-2.named-let :as named-let]
            [section-4-1-2.util :as util])
  (:refer-clojure :only
                  [-> ->> concat cons defn drop empty? first let list
                      map nth rest second take]))

(defn do? [exp]
  (util/tagged-list? exp 'do))

(defn do-bindings-and-increments [exp]
  (second exp))

(defn do-bindings [exp]
  (->> exp
       (do-bindings-and-increments)
       (map #(take 2 %))))

(defn do-increments [exp]
  (->> exp
       (do-bindings-and-increments)
       (map #(nth % 2))))

(defn do-exit-condition-and-exprs [exp]
  (nth exp 2))

(defn do-exit-condition [exp]
  (-> exp
      do-exit-condition-and-exprs
      first))

(defn do-exit-exprs [exp]
  (->> exp
       do-exit-condition-and-exprs
       (drop 1)))

(defn do-body [exp]
  (drop 3 exp))


;; (do ((x 1 (inc x))) ;; note: 1+ doesn't pass clojure reader
;;      ((> x 4))
;;      x)

;; (let gensym ((x 1))
;;     (if (> x 4)
;;       x
;;       (gensym (inc x))))

(defn do->combination [exp]
  (let [fn-name (clojure.core/gensym)
        bindings (do-bindings exp)
        increments (do-increments exp)
        exit-condition (do-exit-condition exp)
        exit-exprs (do-exit-exprs exp)
        body (do-body exp)]
    (named-let/make-named-let fn-name bindings
                              (list 'if exit-condition (begin/sequence->exp exit-exprs)
                                    (begin/sequence->exp
                                     (concat body
                                             (list (cons fn-name increments))))))))
  
(defn eval-do [exp env eval-fn apply-fn]
  (eval-fn (do->combination exp) env))
