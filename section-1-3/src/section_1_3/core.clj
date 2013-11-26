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


(defn sum [term next a b]
  (if (> a b)
    0
    (+ (term a) (sum term next (next a) b))))


(defn inc [n] (+ n 1))

(defn sum-cubes [a b]
  (sum cube inc a b))

(def sum-cubes-2 (partial sum cube inc))

(sum-cubes 1 2) ;= 9
(sum-cubes-2 1 2) ;= 9

(def sum-integers-2 (partial sum identity inc))

(sum-integers 1 5) ;= 15
(sum-integers-2 1 5) ;= 15

(defn pi-sum [a b]
  (defn pi-term [x]
    (/ 1.0 (* x (+ x 2))))
  (defn pi-next [x]
    (+ x 4))
  (sum pi-term pi-next a b))

(* 8 (pi-sum 1 1000)) ;= 3.139592655589783

(def pi-sum-2
  (partial sum
           #(/ 1.0 (* % (+ % 2)))
           #(+ % 4)))

(* 8 (pi-sum-2 1 1000)) ;= 3.139592655589783

(defn integral [f a b dx]
  (defn add-dx [x] (+ x dx))
  (* (sum f add-dx (+ a (/ dx 2.0)) b)
     dx))

(integral cube 0 1 0.01) ;= 0.24998750000000042
(integral cube 0 1 0.001) ;= 0.249999875000001


;; Exercise 1.29


;; h/3* (y0 + 4y1 + 2y2 + 4 y3 + 2 y4 + yn)

(defn yk [f a k h]
  (f (+ a (* k h))))

(defn ysum [first last f a h]
  (let [coefficient (if (even? first) 2 4)]
    (if (> first last)
      0
      (+ (* coefficient (yk f a first h)) (ysum (inc first) last f a h)))))

(defn simpsons-integrate [f a b n]
  "n must be even"
  (let [h (/ (- b a) n)]
    (float
     (* (/ h 3)
        (+
         (yk f a 0 h)
         (ysum 1 (- n 1) f a h)
         (yk f a n h))))))

(simpsons-integrate cube 0 1 100) ;= 0.25
(simpsons-integrate cube 0 1 1000) ;= 0.25

(integral cube 0 1 0.01) ;= 0.24998750000000042
(integral cube 0 1 0.001) ;= 0.249999875000001
