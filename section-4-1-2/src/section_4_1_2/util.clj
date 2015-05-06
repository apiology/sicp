(ns section-4-1-2.util)

(defn error [& msg] (throw (IllegalStateException. ^java.lang.String (clojure.core/apply str msg))))

