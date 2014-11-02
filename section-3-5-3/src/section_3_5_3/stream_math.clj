(ns section-3-5-3.stream-math
  (:require [section-3-5-3.streams :refer :all]
            [section-3-5-3.math :refer :all]
            [section-3-5-3.letrec :as lr]))

(defn sqrt-stream [x]
  (lr/letrec [s
              (cons-stream 1.0 (stream-map #(sqrt-improve % x) s))]
             s))

(defn sqrt [x tolerance]
  (stream-limit (sqrt-stream x) tolerance))

(defn- pi-summands [n]
  (cons-stream (/ 1.0 n)
               (stream-map - (pi-summands (+ n 2)))))

;;and then scale stream by 4, to go from pi/4 to pi
(def ^:private pi-stream
  (scale-stream
   (partial-sums (pi-summands 1))
   4))

(defn euler-transform [s]
  (let [s0 (stream-ref s 0)
        s1 (stream-ref s 1)
        s2 (stream-ref s 2)]
    (cons-stream (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-cdr s)))))

(defn- ln-2-summands [n]
  (cons-stream (/ 1.0 n)
               (stream-map - (ln-2-summands (inc n)))))

(def ln-2-stream
  (partial-sums (ln-2-summands 1)))

(defn integers-starting-from [n]
  (cons-stream n (integers-starting-from (inc n))))

(def integers (integers-starting-from 1))

(declare primes)

(defn prime? [n]
  (loop [ps primes]
    (cond (> (square (stream-car ps)) n) true
          (divisible? n (stream-car ps)) false
          :else (recur (stream-cdr ps)))))

(def primes
  (cons-stream
   2
   (stream-filter prime? (integers-starting-from 3))))

(def int-pairs (pairs integers integers))
