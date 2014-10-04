(ns lecture13.tree
  (:gen-class))

(def my-tree (list 4 (list 5 7) 2))

(defn leaf? [t]
  (not (list? t)))

(defn empty-tree? [t]
  (empty? t))

(defn first-branch [t]
  (first t))

(defn rest-branches [t]
  (rest t))

(def the-empty-tree (list))

(defn empty-tree []
  the-empty-tree)

(defn count-leaves [t]
  (cond (leaf? t) 1
        (empty-tree? t) 0

        :else
        (+ (count-leaves (first-branch t))
           (count-leaves (rest-branches t)))))

(count-leaves my-tree) ;= 4

(defn add-branch-to-tree [branch tree]
  (cons branch tree))

(defn tree-map [proc t]
  (cond (leaf? t) (proc t)
        (empty-tree? t) (empty-tree)

        :else
        (add-branch-to-tree (tree-map proc (first-branch t))
                            (tree-map proc (rest-branches t)))))

(defn tree-manip [leaf-op init tree-merge t]
  (cond (leaf? t) (leaf-op t)
        (empty-tree? t) init

        :else
        (tree-merge (tree-manip leaf-op init tree-merge (first-branch t))
                    (tree-manip leaf-op init tree-merge (rest-branches t)))))
        
(defn count-leaves [t]
  (tree-manip (constantly 1) 0 + t))

(count-leaves my-tree) ;= 4

(defn add-leaves [t]
  (tree-manip identity 0 + t))

(add-leaves my-tree) ;= 18



(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
