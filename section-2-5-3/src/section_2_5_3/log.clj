(ns section-2-5-3.log
  (:gen-class)
  (require [clojure.string :as str]))

(defn log [& args]
;  (println (pretty-format args))
)

(defn coll-to-str [coll]
;  (println (str "Called coll-to-str on " (str/join " " coll)))
  (let [ret (str/join " " (flatten coll))]
;    (println (str "Returning " ret))
;    (println (str "...which is of type " (class ret)))
    ret))


(defn types-to-str [type-tags]
  (if (seq? type-tags)
    (do
      ; (println (str "Called types-to-str on " (str/join " " type-tags)))
      (coll-to-str type-tags))
    (str type-tags)))

(defn args-to-str [args]
  (coll-to-str args))
