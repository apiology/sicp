(ns section-4-1-2.util
  (:require [clojure.core.typed :as t :refer [defn]]
            [section-4-1-2.types :as types])
  (:refer-clojure :exclude [defn eval]))

;; (clojure.core.typed/check-ns)

(defn tagged-list? [exp :- types/Expression
                    tag :- t/Symbol] :- Boolean
  (if (seq? exp)
    (= (first exp) tag)
    false))

(defn error [& msg]
  (throw (IllegalStateException. ^java.lang.String (clojure.core/apply str msg))))

(defn last-exp? [seq] :- Boolean
  (or (not (seq? seq))
      (empty? (rest seq))))

(t/ann ^:no-check first-exp [types/Sequence -> types/Expression])
(defn first-exp [seq]
  (if (seq? seq)
    (first seq)
    seq))

(defn rest-exps [seq :- types/Expressions] :- types/Expressions
  (rest seq))

(t/ann set-first! (t/All [x y] [(t/Atom1 (t/HVec [x y])) x -> Any]))
(defn set-first! [atom-of-list new-car]
  (reset! atom-of-list [new-car (second @atom-of-list)]))

(t/ann set-second! (t/All [x y] [(t/Atom1 (t/HVec [x y])) y -> Any]))
(defn set-second! [atom-of-list new-cdr]
  (reset! atom-of-list [(first @atom-of-list) new-cdr]))
