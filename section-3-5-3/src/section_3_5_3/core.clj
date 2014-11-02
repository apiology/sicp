(ns section-3-5-3.core
  (:require [clojure.tools.macro :as mt]))

(defn- already-run? [state]
  (first state))

(defn- result-of-last-run [state]
  (second state))

(defn memo-proc [proc]
  (let [state-atom (atom [false nil])]
    (fn []
      (let [state @state-atom
            already-run (already-run? state)
            result (result-of-last-run state)]
        (if (not already-run)
          (do (reset! state-atom [true (proc)])
              (result-of-last-run @state-atom))
          result)))))

(defmacro my-delay [a]
  `(memo-proc 
    (fn []
      ~a)))

(defmacro cons-stream [a b]
  `(list ~a (my-delay ~b)))

(def the-empty-stream '())

(defn stream-car [stream]
  (first stream))

(defn my-force [delayed-object]
  (delayed-object))


(defn stream-cdr [stream]
  (if (empty? (rest stream))
    '()
    (my-force (first (rest stream)))))

(defn stream-map [proc & argstreams]
  (if (empty? (first argstreams))
    the-empty-stream
    (cons-stream
     (apply proc (map stream-car argstreams))
     (apply stream-map (cons proc (map stream-cdr argstreams))))))

(defn average [a b]
  (/ (+ a b) 2))

(defn sqrt-improve [guess x]
  (average guess (/ x guess)))

(defn stream-null? [s]
  (empty? s))

(defn stream-for-each [proc s]
  (if (stream-null? s)
    :done
    (do
      (proc (stream-car s))
      (stream-for-each proc (stream-cdr s)))))

(defn display [x]
  (print x))

(defn display-line [s]
  (newline)
  (display s))

(defn display-stream [s]
  (stream-for-each display-line s))

;; http://grokbase.com/t/gg/clojure/11ced7fdr2/letrec
(defmacro letrec [binding & body]
  (let [[var expr] binding
        g-var (gensym)]
    `(let [~g-var (promise)]
       (mt/symbol-macrolet [~var @~g-var]
                        (deliver ~g-var ~expr)
                        ~@body))))

(defn sqrt-stream [x]
  (letrec [s
           (cons-stream 1.0 (stream-map #(sqrt-improve % x) s))]
  s))

(defn stream-ref [s n]
  (if (= n 0)
    (stream-car s)
    (recur (stream-cdr s) (dec n))))

(def sqrt-2 (sqrt-stream 2))

(stream-ref sqrt-2 0)
(stream-ref sqrt-2 1)
(stream-ref sqrt-2 2)
(stream-ref sqrt-2 3)
(stream-ref sqrt-2 4)
(stream-ref sqrt-2 5)

; (display-stream (sqrt-stream 2))

;; pi/4 = 1 - 1/3 + 1/5 - 1/7 + ...

;; summands are the recipricols of the odd integers, with alternating signs

(defn pi-summands [n]
  (cons-stream (/ 1.0 n)
               (stream-map - (pi-summands (+ n 2)))))

;; take the stream of sums of more and more terms
(defn scale-stream [stream factor]
  (stream-map #(* % factor)
              stream))

(defn add-streams [s1 s2]
  (stream-map + s1 s2))

(defn partial-sums [s]
  (cons-stream (stream-car s)
               (add-streams (stream-cdr s)
                            (partial-sums s))))

;;and then scale stream by 4, to go from pi/4 to pi
(def pi-stream
  (scale-stream
   (partial-sums (pi-summands 1))
   4))

;; each value is pi to more and more precision.

; (display-stream pi-stream)
(defn square [s] (* s s))
(defn euler-transform [s]
  (let [s0 (stream-ref s 0)
        s1 (stream-ref s 1)
        s2 (stream-ref s 2)]
    (cons-stream (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-cdr s)))))

; (display-stream (euler-transform pi-stream))

(defn make-tableau [transform s]
  (cons-stream s
               (make-tableau transform
                             (transform s))))

(defn accelerated-sequence [transform s]
  (stream-map stream-car
              (make-tableau transform s)))

; (display-stream (accelerated-sequence euler-transform pi-stream))

;; ex 3.63 - alyssa is right.  yes, this relies on memo-proc working.

;; ex 3.64

(defn abs [x]
  (if (> 0 x) (- x) x))

(defn stream-limit [s tolerance]
  (let [s0 (stream-ref s 0)
        s1 (stream-ref s 1)]
    (if (< (abs (- s1 s0)) tolerance)
      s1
      (recur (stream-cdr s) tolerance))))

(defn sqrt [x tolerance]
  (stream-limit (sqrt-stream x) tolerance))

(sqrt 2 0.00000000000001)

;; ex 3.65

;; ln 2 = 1 - 1/2 + 1/3 - 1/4 + ...

(defn ln-2-summands [n]
  (cons-stream (/ 1.0 n)
               (stream-map - (ln-2-summands (inc n)))))

(def ln-2-stream
  (partial-sums (ln-2-summands 1)))

; (display-stream (ln-2-summands 1))
; (display-stream ln-2-stream)

;; does not converge rapidly...

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Infinite streams of pairs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; int-pairs - all integers i, j where i<= j

;; s and t are the infinite series that i and j are pulled from....
;;
;; composed of three parts:
;; 1) s0, t0
;; 2) rest of the pairs in the first rows
;; 3) pair(stream-cdr s, stream-cdr t)

;; first piece is:

; (stream-car s) (stream-car t) ;; #1

;; second piece is:
; (stream-map #(list (stream-car s) %)
;             (stream-cdr t))

(defn stream-interleave [s1 s2]
  (if (stream-null? s1)
    s2
    (cons-stream (stream-car s1)
                 (interleave s2 (stream-cdr s1)))))
  
(defn pairs [s t]
  (cons-stream
   (list (stream-car s) (stream-car t)) ;; #1
   (stream-interleave
    (stream-map #(list (stream-car s) %)
                (stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))


; (pairs s t)

(defn stream-filter [pred stream]
  (cond (stream-null? stream) the-empty-stream
        (pred (stream-car stream)) (cons-stream (stream-car stream)
                                                (stream-filter pred (stream-cdr stream)))
        :else (stream-filter pred (stream-cdr stream))))

(defn integers-starting-from [n]
  (cons-stream n (integers-starting-from (inc n))))

(def integers (integers-starting-from 1))

(defn remainder [a b]
  (rem a b))

(defn divisible? [x y]
  (= (remainder x y) 0))

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

;; all integers i,j with i<=j such that i+j is prime
(stream-filter #(prime? (+ (first %) (second %)))
               int-pairs)

(stream-ref int-pairs 0)
(stream-ref int-pairs 1)
(stream-ref int-pairs 2)
(stream-ref int-pairs 3)
