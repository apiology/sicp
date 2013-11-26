(ns section-1-3.core
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

;; Section 1.3

(defn cube [x]
  (* x x x))

(defn sum-cubes [a b]
  (if (> a b)
    0
    (+ (cube a) (sum-cubes (+ a 1) b))))

(defn sum-integers [a b]
  (if (> a b)
    0
    (+ a (sum-integers (+ a 1) b))))


(defn pi-sum [a b]
  (if (> a b)
    0
    (+ (/ 1.0 (* a (+ a 2))) (pi-sum (+ a 1) b))))


(defn sum [term a b]
  (if (> a b)
    0
    (+ (term a) (sum term (+ a 1) b))))


(def sum-integers-2 (partial summation +))

(sum-integers 1 5) ;= 15
(sum-integers-2 1 5) ;= 15

(sum-cubes 3)
