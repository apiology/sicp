(ns project-2.core)

(def the-empty-history '())
(def extend-history cons)
(def empty-history? empty?)
(def most-recent-play first)
(def rest-of-plays rest)
(def history-length count)

(defn extract-entry [play game]
  "For a given play against the game, determine which choices were
made and what the scoring will be."
  (first (filter #(= play (first %)) game)))
