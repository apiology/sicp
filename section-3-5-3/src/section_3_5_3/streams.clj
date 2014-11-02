(ns section-3-5-3.streams
  (:require [section-3-5-3.memo-proc :as mp]
            [section-3-5-3.display :refer :all]
            [section-3-5-3.math :refer :all]))

(defmacro my-delay [a]
  `(mp/memo-proc 
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

(defn stream-null? [s]
  (empty? s))

(defn stream-for-each [proc s]
  (if (stream-null? s)
    :done
    (do
      (proc (stream-car s))
      (stream-for-each proc (stream-cdr s)))))

(defn display-stream [s]
  (stream-for-each display-line s))

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

(defn make-tableau [transform s]
  (cons-stream s
               (make-tableau transform
                             (transform s))))

(defn accelerated-sequence [transform s]
  (stream-map stream-car
              (make-tableau transform s)))

(defn stream-ref [s n]
  (if (= n 0)
    (stream-car s)
    (recur (stream-cdr s) (dec n))))

(defn stream-limit [s tolerance]
  (let [s0 (stream-ref s 0)
        s1 (stream-ref s 1)]
    (if (< (abs (- s1 s0)) tolerance)
      s1
      (recur (stream-cdr s) tolerance))))

(defn stream-interleave [s1 s2]
  (if (stream-null? s1)
    s2
    (cons-stream (stream-car s1)
                 (interleave s2 (stream-cdr s1)))))

(defn stream-filter [pred stream]
  (cond (stream-null? stream) the-empty-stream
        (pred (stream-car stream)) (cons-stream (stream-car stream)
                                                (stream-filter pred (stream-cdr stream)))
        :else (stream-filter pred (stream-cdr stream))))

(defn pairs [s t]
  (cons-stream
   (list (stream-car s) (stream-car t)) ;; #1
   (stream-interleave
    (stream-map #(list (stream-car s) %)
                (stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))
