(ns lecture13.core
  (:require [lecture13.binary-tree :as bt])
  (:gen-class))

;; if treated as an unordered list

(defn find1 [user-compare query set]
  (cond (empty? set) :not-here
        (user-compare query (first set)) (first set)
        :else (recur user-compare query (rest set))))

(find1 = 5 '(1 2 3 4)) ;= :not-here
(find1 = 3 '(1 2 3 4)) ;= 3

;; if treated as an ordered list

(defn find2 [less? same? query set]
  (cond (empty? set) :not-here
        (less? query (first set)) :not-here
        (same? query (first set)) (first set)
        :else (recur less? same? query (rest set))))

(find2 < = 5 '(1 2 3 4)) ;= :not-here
(find2 < = 3 '(1 2 3 4)) ;= 3

;; now with an ordered binary tree

;; node (internally stored as a list): left, entry, right
;; tree: root node

(defn find3 [less? same? query tree]
  (loop [tree tree]
    (cond (bt/empty-tree? tree) :not-here
          (same? query (bt/entry tree)) (bt/entry tree)
          (less? query (bt/entry tree)) (recur (bt/left tree))
          :else (recur (bt/right tree)))))

(find3 < = 5 bt/my-tree)

;; search: seeker, Node -> symbol

(defn search [next-place start-node]
  (loop [node start-node] 
    (let [next-node (next-place node)]
      (cond (= next-node true) :found
            (= next-node false) :not-found
            :else (recur next-node)))))

;; strategy: graph, (node -> boolean), (graph, node) -> list<node>

(defn swap*!
  "Like swap! but returns a vector of [old-value new-value]"
  [atom f & args]
  (loop [] 
    (let [ov @atom 
          nv (apply f ov args)]
      (if (compare-and-set! atom ov nv)
        [ov nv]
        (recur)))))

(defn remove-first-and-return [atom]
  (let [[old new] (swap*! atom rest)]
    (first old)))

(defn depth-first-strategy [graph goal? children]
  (let [to-be-visited (atom (vector))]
    (fn where-next? [here]
      (swap! to-be-visited concat (children graph here))
      (cond (goal? here) 
            true

            (empty? to-be-visited) 
            false

            :else
            (remove-first-and-return to-be-visited)))))

            

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
