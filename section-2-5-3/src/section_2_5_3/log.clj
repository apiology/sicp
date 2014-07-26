(ns section-2-5-3.log
  (:gen-class)
  (require [clojure.string :as str]))


(defn coll-to-str [coll]
  (if (seq? coll)
    (pr-str coll)
    (str coll)))
;  (println (str "Called coll-to-str on " (str/join " " coll)))
;  (let [ret (str/join " " (flatten coll))]
;    (println (str "Returning " ret))
;    (println (str "...which is of type " (class ret)))
;    ret))

(defn log [& args]
;  (println (str/join " " (map coll-to-str args)))
)

(defn types-to-str [type-tags]
  (if (seq? type-tags)
    (do
      ; (println (str "Called types-to-str on " (str/join " " type-tags)))
      (coll-to-str type-tags))
    (str type-tags)))

(defn args-to-str [args]
  (coll-to-str args))
