(ns section-3-1-2.core
  (:gen-class))

(def random-init 0)
(def rand-update inc)

(def rand-in-principle
  (let [x (atom random-init)]
    (fn []
      (swap! x rand-update)
      @x)))

(defn rand-in-practice []
  (rand-int Integer/MAX_VALUE))

(def rand rand-in-practice)
(rand) ;= 1
(rand) ;= 2

(defn remainder [a b]
  (mod a b))

(defn gcd [a b]
  (if (zero? b)
    a
    (gcd b (remainder a b))))

(defn cesaro-test []
  (= (gcd (rand) (rand)) 1))

(defn monte-carlo [trials experiment]
  (println (str "Starting monte-carlo with " trials " trials"))
  (loop [trials-remaining trials 
         trials-passed 0]
    (cond (= trials-remaining 0) (/ trials-passed trials)
          (experiment) (recur (dec trials-remaining) (inc trials-passed))
          :else (recur (dec trials-remaining) trials-passed))))

(defn abs [x]
  (if (> 0 x) (- x) x))

(def ^:private tolerance 0.00001)

(defn fixed-point [f first-guess]
  (letfn [(close-enough? [v1 v2]
            (< (abs (- v1 v2)) tolerance))
          (my-try [guess]
            (let [next (f guess)]
              (if (close-enough? guess next)
                next
                (my-try next))))]
    (my-try first-guess)))

(defn average [a b]
  (/ (+ a b) 2))

(defn sqrt [x]
  (fixed-point #(average % (/ x %))
               1.0))

(defn estimate-pi [trials]
  (sqrt (/ 6 (monte-carlo trials cesaro-test))))

;; with crappy random
(estimate-pi 1) ;= 2.4494897427875517
(estimate-pi 10) ;= 2.4494897427875517
(estimate-pi 100) ;= 2.4494897427875517
(estimate-pi 1000) ;= 2.4494897427875517
(estimate-pi 10000) ;= 2.4494897427875517

;; with real random
(estimate-pi 1) ;= 2.4494897427875517
(estimate-pi 10) ;= 3.162277660168379
(estimate-pi 100) ;= 3.162277660168379
(estimate-pi 1000) ;= 3.1862649393858304
(estimate-pi 10000) ;= 3.138308823527715
(estimate-pi 100000) ;= 3.143007241072719
(estimate-pi 1000000) ;= 3.14101687511923
(estimate-pi 10000000) ;= 3.141860897025468

;; region of space defined by P(x, y) - true within the space, false outside

;; Exercise 3.5

;;;;;;;;;;;;;;;;;
;; Balmer peak ;;
;;;;;;;;;;;;;;;;;

(defn rand-double-in-range [lower upper]
  (+ lower (* (Math/random) (- upper lower))))

(defn random-coordinate [x-upper x-lower y-upper y-lower]
  [(rand-double-in-range x-lower x-upper)
   (rand-double-in-range y-lower y-upper)])

(defn shape-integral-test [shape-pred x-upper x-lower y-upper y-lower]
  (fn []
    (let [[x y] (random-coordinate x-upper x-lower y-upper y-lower)]
      (shape-pred x y))))

(defn estimate-integral [shape-pred x-upper x-lower y-upper y-lower trials]
  (let [test (shape-integral-test shape-pred x-upper x-lower y-upper y-lower)
        mc-result (monte-carlo trials test)
        area-of-trial (* (- x-upper x-lower) (- y-upper y-lower))]
    (println (str "test is " test))
    (println (str "mc-result is " mc-result))
    (println (str "area-of-trial is " area-of-trial))
    (double (* mc-result area-of-trial))))
                               
(defn square [x]
  (* x x))

(defn radius-3-circle [x y]
  (let [ret (<= 
             (+ (square (- x 5))
                (square (- y 7)))
             (square 3))]
    ret))

;; integral (area) should be pi * r * r = 9 * pi = 28.274333882308138

(estimate-integral radius-3-circle 15 -15 15 -15 1) ;= 0
(estimate-integral radius-3-circle 15 -15 15 -15 10) ;= 0
(estimate-integral radius-3-circle 15 -15 15 -15 100) ;= 45.0
(estimate-integral radius-3-circle 15 -15 15 -15 1000) ;= 27.9
(estimate-integral radius-3-circle 15 -15 15 -15 10000) ;= 25.29
(estimate-integral radius-3-circle 15 -15 15 -15 100000) ;= 27.774
(estimate-integral radius-3-circle 15 -15 15 -15 1000000) ;= 28.1259
(estimate-integral radius-3-circle 15 -15 15 -15 10000000) ;= 28.2915

(defn unit-circle [x y]
  (let [ret (<= 
             (+ (square x)
                (square y))
             1)]
    ret))

pi * r^2

(estimate-integral unit-circle 2 -2 2 -2 1) ;= 0.0
(estimate-integral unit-circle 2 -2 2 -2 10) ;= 4.8
(estimate-integral unit-circle 2 -2 2 -2 100) ;= 3.68
(estimate-integral unit-circle 2 -2 2 -2 1000) ;= 2.96
(estimate-integral unit-circle 2 -2 2 -2 10000) ;= 3.1536
(estimate-integral unit-circle 2 -2 2 -2 100000) ;= 3.13568
(estimate-integral unit-circle 2 -2 2 -2 1000000) ;= 3.147568
(estimate-integral unit-circle 2 -2 2 -2 10000000) ;= 3.1445776

;; exercise 3.6

(def rand-in-principle-with-reset
  (let [x (atom random-init)]
    (fn [directive]
      (case directive 
        :reset (fn [new-val] (swap! x (constantly new-val)))
        :generate (do (swap! x rand-update)
                      @x)))))

(rand-in-principle-with-reset :generate) ;= 1
(rand-in-principle-with-reset :generate) ;= 2
((rand-in-principle-with-reset :reset) 42)
(rand-in-principle-with-reset :generate) ;= 43

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
