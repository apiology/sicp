(ns section-4-1-2.util)

(defn tagged-list? [exp tag]
  (if (list? exp)
    (= (first exp) tag)
    false))

(defn error [& msg] (throw (IllegalStateException. ^java.lang.String (clojure.core/apply str msg))))

(defn last-exp? [seq] (empty? (rest seq)))

(defn first-exp [seq] (first seq))

(defn rest-exps [seq] (rest seq))

