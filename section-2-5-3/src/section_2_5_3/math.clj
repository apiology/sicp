(ns section-2-5-3.math
  (:gen-class)
  (:require [section-2-5-3.log :refer :all]
            [section-2-5-3.module :refer :all]))

(defn =zero? [num]
  (apply-generic-no-simplify :=zero? num))


(defn lt [x y] (let [ret (apply-generic-no-simplify :lt x y)]
                 (log "(lt " x " " y ") = " ret)
                 ret))
(defn gt [x y] (let [ret (apply-generic-no-simplify :gt x y)]
                 (log "(gt " x " " y ") = " ret)
                 ret))
(defn gte [x y] (let [ret (not (lt x y))]
                  (log "(gte " x " " y ") = " ret)
                  ret))
(defn add [x y] (apply-generic :add x y))
(defn sub [x y] (apply-generic :sub x y))
(defn mul [x y] (apply-generic :mul x y))
(defn div [x y] (apply-generic :div x y))
(defn atan2 [x y] (apply-generic :atan2 x y))
(defn exp [x y] 
  (let [ret (apply-generic :exp x y)]
    (log "exp returning " ret)
    ret))
(defn square [x] (mul x x))

(defn abs [x]
  {:post [(gte % 0)]}
  (log "Calling (abs " x ")")
  (let [is-negative (lt x 0)]
    (log "is-negative on " x ": " is-negative)
    (if is-negative
      (let [ret (sub 0 x)]
        (log "(abs " x ") = " ret)
        ret)
      (let [ret x]
        (log "(abs " x ") = " ret)
        ret))))

(defn cube [x] (mul (mul x x) x))

(defn p [x]
  (sub (mul 3 x) (mul 4 (cube x))))

(defn sine [angle]
  (if (gt (abs angle) 0.1)
    (p (sine (div angle 3.0)))
    angle))
    

(def pi 3.14159265359)

(defn cosine [x]
  (sine (sub (/ pi 2) x)))

(def ^:private tolerance 0.00001)
(defn average [a b]
  (div (add a b) 2))
(defn- average-damp [f]
  #(average % (f %)))

(defn- fixed-point [f first-guess]
  (log "Trying fixed point of " f " given first guess of " first-guess)
  (defn close-enough? [v1 v2]
    (log "Starting close-enough? with " v1 " " v2)
    (let [delta (sub v1 v2)]
      (log "delta is " delta)
      (let [adelta (abs delta)]
        (log "adelta is " adelta)
        (let [ret (lt adelta tolerance)]
          (log "Close enough? " v1 " " v2 ": " ret)
          ret))))
  (defn my-try [guess]
    (let [next (f guess)]
      (if (close-enough? guess next)
        next
        (recur next))))
  (my-try first-guess))
(defn sqrt-damped [x]
  (log "Trying to sqrt-damped of " x)
  (fixed-point (average-damp #(div x %))
               1.0))
(def sqrt sqrt-damped)
