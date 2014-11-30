(ns section-3-5-4.math)

(defn average [a b]
  (/ (+ a b) 2))

(defn sqrt-improve [guess x]
  (average guess (/ x guess)))

(defn abs [x]
  (if (> 0 x) (- x) x))

; (display-stream pi-stream)
(defn square [s] (* s s))

(defn remainder [a b]
  (rem a b))

(defn divisible? [x y]
  (= (remainder x y) 0))
