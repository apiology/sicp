(ns lecture13.binary-tree
  (:gen-class))

;; node (internally stored as a list): left, entry, right
;; tree: root node

(def my-tree '(
               (
                () 2 ()
                3
                ())
               5
               ()))
                          

(defn leaf? [t]
  (not (list? t)))

(defn empty-tree? [t]
  (empty? t))

(defn entry [t]
  (second t))

(defn left [t]
  (first t))

(defn third [lst]
  (nth lst 2))

(defn right [t]
  (third t))

