(ns section-4-1-2.util)

(defn tagged-list? [exp tag]
  (if (seq? exp)
    (= (first exp) tag)
    false))

(defn error [& msg] (throw (IllegalStateException. ^java.lang.String (clojure.core/apply str msg))))

(defn last-exp? [seq] (or (not (seq? seq)) (empty? (rest seq))))

(defn first-exp [seq] (if (seq? seq) (first seq) seq))

(defn rest-exps [seq] (rest seq))

(defn set-car! [atom-of-list new-car]
  (reset! atom-of-list (cons new-car (rest @atom-of-list))))

(defn set-cdr! [atom-of-list new-cdr]
  (reset! atom-of-list (cons (first @atom-of-list) new-cdr)))
