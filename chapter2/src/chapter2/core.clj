(ns chapter2.core)

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))


;; Exercise 2.1

;;(defn make-rat [n d] (list n d))

(defn remainder [a b]
  (mod a b))

(defn gcd [a b]
  (if (zero? b)
    a
    (gcd b (remainder a b))))

(defn make-rat [n d]
  (let [g (gcd n d)]
    (list (/ n g) (/ d g))))

(defn make-rat [n d]
  (if (neg? d)
    (make-rat (- n) (- d))
    (let [g (gcd n d)]
      (list (/ n g) (/ d g)))))

(defn numer [x] (first x))

(defn denom [x] (second x))

(defn add-rat [x y]
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(defn sub-rat [x y]
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(defn mul-rat [x y]
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(defn div-rat [x y]
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(defn div-rat [x y]
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(defn equal-rat? [x y]
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(defn print-rat [x]
  (println)
  (println (str (numer x) "/" (denom x))))

(def one-half (make-rat 1 2))

(print-rat one-half)

(def one-third (make-rat 1 3))

(def denorm-one-third (make-rat -1 -3))

(def minus-one-third (make-rat -1 3))

(def denorm-minus-one-third (make-rat 1 -3))

(make-rat -1 -3)

(print-rat (add-rat one-half one-third))

(print-rat (mul-rat one-half one-third))

(print-rat (add-rat one-third one-third))

(print-rat one-third)

(print-rat denorm-one-third)

(print-rat minus-one-third)

(print-rat denorm-minus-one-third)

;; Exercise 2.2

(defn make-point [x y]
  (list x y))

(defn x-point [p]
  (first p))

(defn y-point [p]
  (second p))

(defn make-segment [start end]
  (list start end))

(defn segment-start [s]
  (first s))

(defn segment-end [s]
  (second s))

(defn average [a b]
  (/ (+ a b) 2))

(defn midpoint-segment [segment]
  (let [start (segment-start segment)
        start-x (x-point start)
        start-y (y-point start)
        end (segment-end segment)
        end-x (x-point end)
        end-y (y-point end)]
    (make-point (average start-x end-x)
                (average start-y end-y))))

(defn print-point [p]
  (println (str "(" (x-point p) "," (y-point p) ")")))

(def origin-point (make-point 0 0))

(def oneone-point (make-point 1 1))

(def unit-diagonal-segment (make-segment origin-point oneone-point))

(print-point (midpoint-segment unit-diagonal-segment))

;; Exercise 2.3

;; implementation specific

(defn make-rect [lower-left-point upper-right-point] 
  (list lower-left-point upper-right-point))

(defn lower-left-point [rect]
  (first rect))

(defn upper-right-point [rect]
  (second rect))


;; less implementation dependent 

(defn top-y-point [rect]
  (y-point (upper-right-point rect)))

(defn bottom-y-point [rect]
  (y-point (lower-left-point rect)))

(defn leftmost-x-point [rect]
  (x-point (lower-left-point rect)))

(defn rightmost-x-point [rect]
  (x-point (upper-right-point rect)))


;; another implementation

;; '(left-x right-x top-y bottom-y)
(defn make-rect [lower-left-point upper-right-point] 
  (list (x-point lower-left-point)
        (x-point upper-right-point)
        (y-point upper-right-point)
        (y-point lower-left-point)))

(defn rect-left-x [rect]
  (nth rect 0))

(defn rect-right-x [rect]
  (nth rect 1))

(defn rect-top-y [rect]
  (nth rect 2))

(defn rect-bottom-y [rect]
  (nth rect 3))

(defn lower-left-point [rect]
  (make-point (rect-left-x rect)
              (rect-bottom-y rect)))

(defn upper-right-point [rect]
  (make-point (rect-right-x rect)
              (rect-top-y rect)))

;; implementation independent

(defn rect-width [rect]
  (- (rightmost-x-point rect) (leftmost-x-point rect)))

(defn rect-height [rect]
  (- (top-y-point rect) (bottom-y-point rect)))

(defn rect-perimeter [rect]
  (* 2 (+ (rect-width rect) (rect-height rect))))

(defn rect-area [rect]
  (* (rect-height rect) (rect-width rect)))

(def unity-rectangle (make-rect (make-point 0 0) (make-point 1 1)))

(rect-width unity-rectangle) ;= 1
(upper-right-point unity-rectangle) ;= (1 1)
(top-y-point unity-rectangle) ;= 1
(rect-height unity-rectangle) ;= 1
(rect-perimeter unity-rectangle) ;= 4
(rect-area unity-rectangle) ;= 1

;; Exercise 2.4

;; (defn cons [x y]
;;   (fn [m] (m x y)))

;; (defn car [z]
;;   (z (fn [p q] p)))

;; (defn cdr [z]
;;   (z (fn [p q] q)))


;; (car (cons 'x 'y)) ;= x

;; (cdr (cons 'x 'y)) ;= y

;; Exercise 2.5

; 2 and 3 are relatively prime, so we can factor any number back to
; those values.  Thus, a and b are preserved.

;; (defn square
;;   [x]
;;   (*' x x))

;; (defn exp [b n]
;;   (loop [b b n n final-multiply 1]
;;      (cond (= n 0)
;;            final-multiply

;;            (even? n)
;;            (recur (*' b b) (/ n 2) final-multiply)

;;            :else
;;            (recur b (dec n) (*' b final-multiply)))))

;; (exp 2 10) ;= 1024

;; (defn remainder [x y] (rem x y))

;; (defn div [x y] (quot x y))

;; (defn num-twos-and-threes [x]
;;   (loop [x x num-twos 0 num-threes 0]
;;     (cond (= x 1) (list num-twos num-threes)
;;           (= (remainder x 2) 0) (recur (div x 2) (inc num-twos) num-threes)
;;           (= (remainder x 3) 0) (recur (div x 3) num-twos (inc num-threes))
;;           :else (throw (str "Could not factor " x)))))

;; (defn cons [x y]
;;    (* (exp 2 x) (exp 3 y)))

;; (defn car [z]
;;   (first (num-twos-and-threes z)))

;; (defn cdr [z]
;;   (second (num-twos-and-threes z)))

;; (cons 7 8) ;= 839808
;; (num-twos-and-threes 839808) ;= 7 8
;; (car (cons 7 8)) ;= 7
;; (cdr (cons 7 8)) ;= 8

; Exercise 2.6

;; Church numerals come in the form of (fn [f] ...).

;; (def zero
;; ;  (fn [_] identity)
;;   (fn [f] (fn [x] x)))

;; (defn add-1 [n]
;;   (fn [f] (fn [x] (f ((n f) x))))
;; )

;; (def one
;; ;; orig
;; ;  (fn [f] (fn [x] (f ((n f) x))))
;; ;; 1
;; ;  (fn [f] (fn [x] (f (((fn [_] identity) f) x))))
;; ;; 2
;; ;  (fn [f] (fn [x] (f (identity x))))
;; ;; 3
;;   (fn [f] (fn [x] (f x)))
;; )

;; (def two
;; ;; orig
;; ;  (fn [f] (fn [x] (f ((n f) x))))
;; ;; 1
;; ;  (fn [f] (fn [x] (f (((fn [f] (fn [x] (f x))) f) x))))
;; ;; 2
;; ;  (fn [f] (fn [x] (f ((fn [x] (f x)) x))))
;; ;; 3
;;   (fn [f] (fn [x] (f (f x)))))

;; (defn + [a b]
;;   (fn [f] (fn [x] ((a f) ((b f) x)))))

;; (defn + [a b]
;;   (fn [f]
;;     (fn [x]
;;       (let [evaluated-b ((b f) x)]
;;         ((a f) evaluated-b)))))

;; (defn + [a b]
;;   (fn [f]
;;     (fn [x]
;;       ((a f) ((b f) x)))))

;; (defn print-church [n]
;;   ((n inc) 0))

;; (print-church zero) ;= 0
;; (print-church one) ;= 1
;; (print-church two) ;= 2
;; (print-church (+ two one)) ;= 3
;; (print-church (+ two two)) ;= 4

;; Section 2.1.4

        
; Exercise 2.7

(defn make-interval [lower-bound upper-bound] (list upper-bound lower-bound))

(defn upper-bound [interval] (first interval))

(defn lower-bound [interval] (second interval))

(def my-interval-one (make-interval 0.9 1.1)) ;= (1.1 0.9)

(defn add-interval [x y]
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(add-interval my-interval-one my-interval-one) ;= (2.2 1.8)

(defn mul-interval [x y]
  (let [p1 (* (lower-bound x) (lower-bound y))
        p2 (* (lower-bound x) (upper-bound y))
        p3 (* (upper-bound x) (lower-bound y))
        p4 (* (upper-bound x) (upper-bound y))]
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(mul-interval my-interval-one my-interval-one) ;= (1.21 0.81)

(defn div-interval [x y]
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

(div-interval my-interval-one my-interval-one) ;= (1.2222 0.8181818)

;; Exercise 2.8

(defn sub-interval [x y]
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

(sub-interval my-interval-one my-interval-one) ;= (0.2 -0.2)

;; Exercise 2.9

;; width is the upper-bound minus the lower-bound

;; show that the width of the sum or difference of two intervals is a
;; function only of the widths of the intervals being added (or
;; subtracted).


;; (defn add-interval [x y]
;;   (make-interval (+ (lower-bound x) (lower-bound y))
;;                  (+ (upper-bound x) (upper-bound y))))

;; width of the combined interval is:
;  (- (+ (upper-bound x) (upper-bound y)) (+ (lower-bound x) (lower-bound y)))
;  (- (+ upper-bound-x upper-bound-y) (+ lower-bound-x lower-bound-y))
;  (- (upper-bound-x + upper-bound-y) (lower-bound-x + lower-bound-y))
;  (upper-bound-x + upper-bound-y) - (lower-bound-x + lower-bound-y)
;  (upper-bound-x - lower-bound-x) + (upper-bound-y - lower-bound y)
;  width-x + width-y

;; width-x is upper-bound-x - lower-bound-x
;; width-y is upper-bound-y - lower-bound-y


;; give two examples to show that this is not the case for
;; multiplication or division.

;; Exercise 2.10


(defn spans-zero [x]
  (and (neg? (lower-bound x)) (pos? (upper-bound x))))

(defn div-interval [x y]
  (if (spans-zero y)
    (throw (IllegalStateException. "Spans zero!"))
    (mul-interval x
                  (make-interval (/ 1 (upper-bound y))
                                 (/ 1 (lower-bound y))))))

(div-interval my-interval-one my-interval-one) ;= (1.2222 0.8181818)

(def my-interval-two (make-interval -0.1 0.2))

(div-interval my-interval-one my-interval-two) ;= ;; Spans zero!

;; Exercise 2.11


(defn- whole-interval [f i]
  (every? f (list (lower-bound i) (upper-bound i))))

(defn- whole-interval-pos? [i]
  (whole-interval pos? i))

(defn- whole-interval-neg? [i]
  (whole-interval neg? i))


(defn mul-interval [x y]
  (let [lo-x (lower-bound x)
        lo-y (lower-bound x)
        hi-x (upper-bound x)
        hi-y (upper-bound y)]
    (cond
     (whole-interval-pos? x)
     (cond
      (whole-interval-pos? y)
      (make-interval (* lo-x lo-y) (* hi-x hi-y))

      (whole-interval-neg? y)
      (make-interval (* hi-x hi-y) (* lo-x lo-y))

      :else ; lo-y is neg, hi-y is pos
      (make-interval (* hi-x lo-y) (* hi-x hi-y)))

     (whole-interval-neg? x)
     (cond
      (whole-interval-pos? y)
      (make-interval (* lo-x hi-y) (* hi-x hi-y))

      (whole-interval-neg? y)
      (make-interval (* hi-x hi-y) (* lo-x lo-y))

      :else ; lo-y is neg, hi-y is pos
      (make-interval (* lo-x hi-y) (* lo-x lo-y)))


     :else ; lo-x is neg, hi-x is pos
     (cond
      (whole-interval-pos? y)
      (make-interval (* lo-x hi-y) (* hi-x hi-y))

      (whole-interval-neg? y)
      (make-interval (* hi-x lo-y) (* lo-x lo-y))

      :else ; lo-y is neg, hi-y is pos
      (make-interval
       (min (* lo-x hi-y) (* hi-x lo-y))
       (max (* lo-x lo-y) (* hi-x hi-y)))))))


;; from text:

(defn make-center-width [c w]
  (make-interval (- c w) (+ c w)))

(defn center [i]
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(defn width [i]
  (/ (- (upper-bound i) (lower-bound i)) 2))

;; Exercise 2.12
(defn abs [x]
  (if (> 0 x) (- x) x))

(defn make-center-percent [c percentage]
  (let [delta (* (abs c) (/ percentage 100))]
    (make-interval (- c delta) (+ c delta))))

(defn percent [i]
  (* 100 (/ (width i) (center i))))


;; Exercise 2.13

(defn percent-of-products [i j]
  (+ (tolerance i) (tolerance j)))

; (0.9 1.1) * (0.8 1.2)

;  10%   20%


; = (0.72 1.32) - center of 1.02, width of 0.6. percdntage of +/- 29.4%



(defn par1 [r1 r2]
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(defn par2 [r1 r2]
  (let [one (make-interval 1 1)]
    (div-interval
         one
         (add-interval (div-interval one r1)
                       (div-interval one r2)))))

(def joe (make-interval 9/10 11/10))
(def pete (make-interval 11/10 13/10))
(def one (make-interval 1 1))
(def mega-joe (make-interval 99999 100001))
(def mega-pete (make-interval 100001 110003))

;; Exercise 2.14

(par1 joe pete) ;= (0.7150000000000001 0.6561000000000001)
(par2 joe pete) ;= (0.5 1)

joe ;= (1.1 0.9)
(div-interval joe joe) ;= (1.2222222222222223 0.81)
(div-interval one joe) ;= (1.1111111111111112 1)

(div-interval mega-joe mega-joe) ;= (1.000020000200002 9999800001)
(div-interval one mega-joe) ;= (1.000010000100001E-5 1)



(defn enqueue
  [sieve n factor]
  ""
  (let [m (+ n factor)]
    (assoc sieve m (conj (sieve m) factor))
))


(defn next-sieve
  "sieve is a map from the next non-prime numbers to their factors"
  [sieve candidate]
  ;; if the candidates is non-prime...
  (if-let [factors (sieve candidate)]
    (reduce #(enqueue %1 candidate %2)
            (dissoc sieve candidate)
            factors)
    (enqueue sieve candidate candidate)))

(defn primes [max]
  (apply concat (vals (reduce next-sieve {} (range 2 max)))))

(primes 27)
(defn prime? [n] (some #(= n %) (primes (inc n))))
(prime? 3)
(= (last (primes (inc 3))) 3)

(apply concat (vals (next-sieve (next-sieve (next-sieve (next-sieve {} 2) 3) 4) 5)))

(def matrix
  [[1 2 3]
   [4 5 6]
   [7 8 9]])

(defn neighbors
  ([size yx] (neighbors [[-1 0] [1 0] [0 -1] [0 1]] size yx))
  ([deltas size yx]
     (filter (fn [new-yx]
               (every? #(< -1 % size) new-yx))
             (map #(vec (map + yx %)) deltas))))

(get-in matrix [1 2])
(assoc-in matrix [1 2] 'x)

(update-in matrix [1 2] * 100)

(map #(get-in matrix %) (neighbors 3 [0 0]))

(defmethod print-method clojure.lang.PersistentQueue
  [q w]
  (print-method '<- w) (print-method (seq q) w) (print-method '-< w))

(def schedule
  (conj clojure.lang.PersistentQueue/EMPTY
        :wake-up :shower :brush-teeth))

(peek (conj schedule :blah))

(defn pos-orig
  "Finds the index of e in the collection coll"
  [e coll]
  (let [cmp (if (map? coll)
              #(= (second %1) %2)
              #(= %1 %2))]
    (loop [s coll idx 0]
      (when (seq s)
        (if (cmp (first s) e)
          (if (map? coll)
            (first (first s))
            idx)
          (recur (next s) (inc idx)))))))

(def my-vec [1 2 3])
(pos-orig 2 my-vec) ;= 1

(defn index [coll]
  (cond
   (map? coll) (seq coll)
   (set? coll) (map vector coll coll)
   :else (map vector (iterate inc 0) coll)))

(index [1 2 3])
(index '(a b c))
(index #{'a 'b 'c})
(index {5 'a, 7 'b, 3 'c})

(defn pos
  "Finds the index of e in the collection coll"
  [e coll]
  (map first (filter #(= e (second %)) (index coll))))

(defn pos
  "Finds the index of e in the collection coll"
  [pred coll]
  (for [[i v] (index coll) :when (pred v)] i))


(pos #(= 1 %) [1 2 3 1 1 1])

(def plays [{:band "Burial", :plays 979, :loved 9}
            {:band "Eno", :plays 2333, :loved 15}
            {:band "Bill Evans", :plays 979, :loved 9}
            {:band "Magma", :plays 2665, :loved 31}])

(defn keys-apply [f ks m]
  "Takes a function, a set of keys, and a map and applies the function
  to the map on the given keys.  A new map of the results of the
  function applied to the keyed entries is returned."
  (let [only (select-keys m ks)]
    (zipmap (keys only) (map f (vals only)))))
(keys-apply #(.toUpperCase %) #{:band} (plays 0))

(defn manip-map [f ks m]
  "Takes a function, a set of keys, and a map and applies the function
   to the map on the given keys.  A modified version of the original
   map is returned with the results of the function applied to each
   keyed entry."
  (conj m (keys-apply f ks m)))

(manip-map #(int (/ % 2)) #{:plays :loved} (plays 0))

(defn halve! [ks]
  (map (partial manip-map #(int (/ % 2)) ks) plays))

(halve! [:plays])

(defn slope
  [& {:keys [p1 p2] :or {p1 [0 0] p2 [1 1]}}]
  (float (/ (- (p2 1) (p1 1))
            (- (p2 0) (p1 0)))))

(slope :p1 [4 15] :p2 [3 21]) ;=> -6.0

(slope :p2 [2 1]) ;=> 0.5

(slope) ;=> 1.0

(defn slope-with-constraints [p1 p2]
  {:pre [(not= p1 p2) (vector? p1) (vector? p2)]
   :post [(float? %)]}
  (float (/ (- (p2 1) (p1 1))
            (- (p2 0) (p1 0)))))

(slope-with-constraints [4 15] [4 15])

(defn put-things [m]
  (into m {:meat "beef" :veggie "broccoli"}))

(put-things {}) ;= {:veggie "broccoli", :meat "beef"}

(defn vegan-constraints [f m]
  {:pre [(:veggie m)]
   :post [(:veggie %) (nil? (:meat %))]}
  (f m))

(vegan-constraints identity {:veggie "carrot"}) ;= {:veggie "carrot"}

(vegan-constraints put-things {:veggie "carrot"}) ;= AssertionError
                                                  ;Assert failed:
                                                  ;(nil? (:meat %))
                                                  ;user/vegan-constraints

(defn balanced-diet [f m]
  {:post [(:meat %) (:veggie %)]}
  (f m))

(balanced-diet put-things {}) ; {:veggie "broccoli", :meat "beef"}

(defn finicky [f m]
  {:post [(= (:meat %) (:meat m))]} ; never change the meat
  (f m))

(finicky put-things {:meat "chicken"})
;= AssertionError Assert failed: (= (:meat %) (:meat m)) user/finicky
;(form-init119182401532430510.clj:1)

(def times-two
  (let [x 2]
    (fn [y] (* y x))))

(def add-and-get
  (let [ai (java.util.concurrent.atomic.AtomicInteger.)]
    (fn [y] (.addAndGet ai y))))

(add-and-get 2) ;= 2
(add-and-get 4) ;= 6
(add-and-get 2) ;= 8

(defn times-n [n]
  (let [x n]
    (fn [y] (* y x))))

((times-n 2) 2)

(def times-four (times-n 4))

(times-four 3) ;= 12

(defn divisible [denom]
  (fn [num]
    (zero? (rem num denom))))

(filter even? (range 10)) ;= (0 2 4 6 8)

(filter (divisible 4) (range 10)) ;= (0 4 8)

(defn filter-divisible [denom s]
  (filter (fn [num] (zero? (rem num denom))) s))

(filter-divisible 4 (range 10)) ;= (0 4 8)

(defn filter-divisible [denom s]
  (filter #(zero? (rem % denom)) s))

(filter-divisible 5 (range 20)) ;= (0 5 10 15)

(def bearings [{:x 0, :y 1} ; north
               {:x 1, :y 0} ; east
               {:x 0, :y -1} ; south
               {:x -1, :y 0}]) ; west

(defn forward [x y bearing-num]
  [(+ x (:x (bearings bearing-num)))
   (+ y (:y (bearings bearing-num)))])

(forward 5 5 0) ;= [5 6]

(forward 5 5 1) ;= [6 5]

(defn bot [x y bearing-num]
  {:coords [x y]
   :bearing ([:north :east :south :west] bearing-num)
   :forward (fn [] (bot (+ x (:x (bearings bearing-num)))
                        (+ y (:y (bearings bearing-num)))
                        bearing-num))})

(def mybot (bot 5 5 0))
(:coords mybot) ;= [5 5]
(:bearing mybot) ;= :north
(:coords ((:forward mybot))) ;= [5 6]


(defn bot [x y bearing-num]
  {:coords [x y]
   :bearing ([:north :east :south :west] bearing-num)
   :forward (fn [] (bot (+ x (:x (bearings bearing-num)))
                        (+ y (:y (bearings bearing-num)))
                        bearing-num))
   :turn-right (fn [] (bot x y (mod (+ 1 bearing-num) 4)))
   :turn-left  (fn [] (bot x y (mod (- 1 bearing-num) 4)))})

(:bearing ((:forward ((:forward ((:turn-right (bot 5 5 0))))))))

(defn mirror-bot [x y bearing-num]
  {:coords [x y]
   :bearing ([:north :east :south :west] bearing-num)
   :forward (fn [] (mirror-bot (- x (:x (bearings bearing-num)))
                               (- y (:y (bearings bearing-num)))
                               bearing-num))
   :turn-right (fn [] (mirror-bot x y (mod (- 1 bearing-num) 4)))
   :turn-left  (fn [] (mirror-bot x y (mod (+ 1 bearing-num) 4)))})

(defn pow [base exp]
  (if (zero? exp)
    1
    (* base (pow base (dec exp)))))

(pow 2 10) ;= 1024
(pow 1.01 925) ;= 9937.353723241924

(pow 2 10000) ;= StackOverflowError   clojure.lang.Numbers$LongOps.dec (Numbers.java:522)

(defn pow [base exp]
  (letfn [(kapow [base exp acc]
            (if (zero? exp)
              acc
              (recur base (dec exp) (* base acc))))]
    (kapow base exp 1)))


(defn gcd [x y]
  (cond
   (> x y) (gcd (- x y) y)
   (< x y) (gcd x (- y x))
   :else x))

(defn elevator [commands]
  (letfn
      [(ff-open [[cmd & r]]
         "When the elevator is open on the 1st floor it can either
         close or be done"
         #(case cmd
            :close (ff-closed r)
            :done true
            false))
       (ff-closed [[cmd & r]]
         "When the elevator is closed on the 1st floor it can either
         open or go up."
         #(case cmd
            :open (ff-open r)
            :up (sf-closed r)
            false))
       (sf-closed [[cmd & r]]
         "When the elevator is closed on the 2nd floor it can either
         go down or open."
         #(case cmd
            :down (ff-closed r)
            :open (sf-open r)
            false))
       (sf-open [[cmd & r]]
         "When the elevator is open on the 2nd floor it can either
         close or be done"
         #(case cmd
            :close (sf-closed r)
            :done true
            false))]
    (trampoline ff-open commands)))

(elevator [:close :open :close :up :open :done])

(defn fac-cps [n k]
  (letfn [(cont [v] (k (* v n)))] ;; next continuation
    (if (zero? n) ;; accept function
      (k 1) ;; return continuation
      (recur (dec n) cont))))

(defn fac [n]
  (fac-cps n identity))

(defn mk-cps [accept? end-value kend kont]
  (fn [n]
    ((fn [n k]
       (let [cont (fn [v] (k (kont v n)))] ;; next continuation
         (if (accept? n) ;; accept continuation
           (k end-value) ;; return condtinuation
           (recur (dec n) cont))))
     n kend)))

(def fac (mk-cps zero? 1 identity #(* %1 %2))) ;; factorial
(fac 10)
(def tri (mk-cps zero? 1 dec #(+ %1 %2)))
(tri 10) ;= 55

;; section 7.4 of Joy of Clojure - A* algorithm to try to get through
;; some valleys and mountains, from the upper left corner to the lower
;; right corner of this:
(def world [[  1   1   1   1    1]
            [999 999 999 999    1]
            [  1   1   1   1    1]
            [  1 999 999 999  999]
            [  1   1   1   1    1]])



(neighbors 5 [0 0]) ;= ((1 0) (0 1))

(defn estimate-cost [step-cost-est size y x]
  "Estimate the costs from y,x to the lower right corner by assuming
  every step costs step-cost-est"
  (* step-cost-est
     (- (+ size size) y x 2)))

(estimate-cost 900 5 0 0) ;= 7200
(estimate-cost 900 5 4 4) ;= 0

(defn path-cost [node-cost cheapest-nbr]
  (+ node-cost
     (:cost cheapest-nbr 0))) ;; add the cheapest neighbor cost, else 0

(path-cost 900 {:cost 1}) ;= 901

(defn total-cost [newcost step-cost-est size y x]
  "Total cost for a path, given the cost to get to y,x was new-cost,
  and you estimate each additional step will cost step-cost-est"
  (+ newcost
     (estimate-cost step-cost-est size y x)))

(total-cost 0 900 5 0 0) ;= 7200

(total-cost 1000 900 5 3 4) ;= 1900

(defn min-by [f coll]
  (when (seq coll)
    (reduce (fn [min this]
              (if (> (f min) (f this)) this min))
            coll)))

(min-by :cost [{:cost 100} {:cost 36} {:cost 9}]) ;= {:cost 9}

(defn astar [start-yx step-est cell-costs]
  (let [size (count cell-costs)] ;; my world measures size x size
    (loop [steps 0
           ;; routes starts out as a size x size array of nil.
           routes (vec (replicate size (vec (replicate size nil))))
           work-todo (sorted-set [0 start-yx])]
      (if (empty? work-todo) ;; check done
        [(peek (peek routes)) :steps steps] ;; grab first route
        (let [[_ yx :as work-item] (first work-todo) ;; get next work item
              rest-work-todo (disj work-todo work-item) ;; clear from todo
              nbr-yxs (neighbors size yx) ;; calc least-cost
              cheapest-nbr (min-by :cost
                                   (keep #(get-in routes %)
                                         nbr-yxs))
              newcost (path-cost (get-in cell-costs yx) ; calc path so far
                                 cheapest-nbr)
              oldcost (:cost (get-in routes yx))]
          (if (and oldcost (>= newcost oldcost)) ; check if new is worse
            (recur (inc steps) routes rest-work-todo)
            (recur (inc steps) ; place new path in routes
                   (assoc-in routes yx
                             {:cost newcost
                              :yxes (conj (:yxes cheapest-nbr [])
                                          yx)})
                   (into rest-work-todo ; add estimated path to todo and recur
                         (map
                          (fn [w]
                            (let [[y x] w]
                              [(total-cost newcost step-est size y x) w]))
                          nbr-yxs)))))))))

(astar [0 0] 900 world) ;=> [{:cost 17, :yxes [[4 4]]} :steps 94]
(count world) ;= 5


;; On Lisp chapter 5

(def myvec [1 2 3 4 5])

(defn remove-if-not [f coll]
  (filter f coll))

(defn complement [f]
  (fn [& args] (not (apply f args))))

(defn remove-if [f coll]
  (remove-if-not (complement f) coll))

((complement even?) 2)

(remove-if even? myvec) ;= (1 3 5)


(remove-if-not even? myvec) ;= (2 4)

(defn joiner [obj]
  (case (seq? obj) conj
        (number? obj) +))

(defn my-join [& args]
  (apply (joiner (first args)) args))

(my-join 1 2 3) ;= 6

(joiner (first [1 2 3]))
(apply (joiner (first [1 2 3])) [1 2 3])

(defn make-adder [n]
  #(+ % n))

(def add3 (make-adder 3))

(add3 2) ;= 5

(remove-if (complement odd?) '(1 2 3 4 5 6)) ;= (1 3 5)

;; Section 5.3

(defn slow [x]
  (do (Thread/sleep 5000) x))

(def slow (memoize slow))

(slow 1) ;= 5 second wait, then 1
(slow 1) ;= 1

;; Section 5.4

(defn fold-right [f s coll]
  ((reduce (fn [acc x] #(acc (f x %))) identity coll) s))

(defn compose [& fns]
  (if (seq fns)
    (let [fn1 (last fns)
          rest-fns (butlast fns)]
      (fn [& args]
        (reduce #(%2 %1)
                (apply fn1 args)
                (reverse rest-fns))))
    identity))

(defn find-if [f lst]
  (if (f (first lst))
    (first lst)
    (find-if f (rest lst))))
((compose) 1) ;= 1
(compose +)

((comp inc find-if) odd? '(2 3 4)) ;= 4
((compose inc find-if) odd? '(2 3 4)) ;= 4

(defn complement [pred]
  (compose not pred))

(remove-if (complement odd?) '(1 2 3 4 5 6)) ;= (1 3 5)

(defn fif
  "Returns a function that calls my-then on the argument if my-if on
  the argument is true, otherwise calls my-else on the argument (or
  returns nil if not provided)"
  ([my-if my-then]
     (fif my-if my-then nil))
  ([my-if my-then my-else]
     #(if (my-if %)
        (my-then %)
        (if my-else (my-else %)))))

(defn double [x] (* x 2))
(defn halve [x] (/ x 2))

(map (fif even? double halve) '(1 2 3)) ;= (1/2 4 3/2)

(defn fint [my-fn & rest-fns]
  "Function intersection--returns a function that true if the argument
  makes all of the funcions return true"
  (if (nil? rest-fns)
    my-fn
    (let [chain (apply fint rest-fns)]
      #(and (my-fn %) (chain %)))))

(filter (fint even? odd?) '(1 2 3)) ;= ()
(filter (fint even? #(< % 4)) '(1 2 3 4)) ;= (2)


(defn fun
  [my-fn & rest-fns]
  "Function union--returns a function that true if the argument
  makes any of the funcions return true"
  (if (nil? rest-fns)
    my-fn
    (let [chain (apply fint rest-fns)]
      #(or (my-fn %) (chain %)))))

(filter (fun even? odd?) '(1 2 3)) ;= (1 2 3)

;; Section 5.5

(defn our-length [coll]
  (if-let [coll (seq coll)]
    (inc (our-length (rest coll)))
    0))

(our-length [1 2 3])
(our-length '(1 2 3))
(our-length "1234") ;= 4

(defn our-every [myfn coll]
  (if-let [coll (seq coll)]
    (and (myfn (first coll))
         (our-every myfn (rest coll)))
    true))

(our-every odd? '(1 2 3)) ;= false

(our-every even? '(1 2 3)) ;= false

(our-every even? '(2 4 6)) ;= true

(defn lrec
  "List recurser.  rec is a function of two arguments--current first
  item in the seq and a function which will continue the recursion."
  ([rec]
     (lrec rec nil))
  ([rec base]
     (fn self [lst]
       (if-let [coll (seq lst)]
         (rec (first lst) #(self (rest lst)))
         (if (fn? base) (base) base)))))

(def our-length (lrec #(inc (%2)) 0))

(our-length '(1 2 3)) ;= 3
(our-length "what the") ;= 8

(defn our-every [f coll] ((lrec #(and (f %1) (%2)) true) coll))


(fn? 0)

(our-every odd? '(1 2 3)) ;= false
(our-every even? '(1 2 3)) ;= false
(our-every even? '(2 4 6)) ;= true

(def copy-list (lrec #(cons %1 (%2))))

(copy-list '(1 2 3)) ;= (1 2 3)

(defn find-if [my-fn coll] ((lrec #(if (my-fn %1) %1 (%2))) coll))

(find-if odd? '(1 2 3)) ;= 1

(defn some [my-fn coll] ((lrec #(or (my-fn %1) (%2)) false) coll))

(some odd? '(1 2 3)) ; = true
(some even? '(1 2 3)) ; = true
(some even? '(1 3)) ; = true

;; Section 5.6

;; Section 2.2 of SICP


(defn list-ref [coll n]
  (if (= 0 n)
    (first coll)
    (list-ref (rest coll) (dec n))))

(def squares (list 1 4 9 16 25))
(list-ref squares 3) ;= 16

(defn length [coll]
  (if (empty? coll)
    0
    (inc (length (rest coll)))))

(def odds (list 1 3 5 7))

(length odds) ;= 4

(defn length [coll]
  (loop [coll coll count 0]
    (if (empty? coll)
      count
      (recur (rest coll) (inc count)))))

(length odds) ;= 4

(concat squares odds) ;= (1 4 9 16 25 1 3 5 7)

(concat odds squares) ;= (1 3 5 7 1 4 9 16 25)

(defn append [coll1 coll2]
  (if (empty? coll1)
    coll2
    (conj (append (rest coll1) coll2) (first coll1))))

(append squares odds) ;= (1 4 9 16 25 1 3 5 7)
(append odds squares) ;= user=> (1 3 5 7 1 4 9 16 25)

;; Exercise 2.17

(defn last-pair [coll]
  "Returns the list that contains only the last element of a given list"
  (cond (empty? coll) nil
        (empty? (rest coll)) (first coll)
        (recur (rest coll))))

;; Exercise 2.18

(defn reverse [coll]
  "Returns a list of the same elements in reverse order"
  (if (empty? coll) coll
      (append (reverse (rest coll))
              (list (first coll)))))

(reverse odds) ;= (7 5 3 1)

;; Exercise 2.19

(defn first-denomination [kinds-of-coins]
  (cond (= kinds-of-coins 1) 1
        (= kinds-of-coins 2) 5
        (= kinds-of-coins 3) 10
        (= kinds-of-coins 4) 25
        (= kinds-of-coins 5) 50))

(defn cc-from-book [amount kinds-of-coins]
  (cond (= amount 0) 1
        (or (< amount 0) (= kinds-of-coins 0)) 0
        :else (+ (cc-from-book amount
                               (- kinds-of-coins 1))
                 (cc-from-book (- amount
                                  (first-denomination kinds-of-coins))
                               kinds-of-coins))))

(defn count-change-from-book [amount]
  (cc-from-book amount 5))

(def no-more? empty?)
(def except-first-denomination rest)
(def first-denomination first)

(defn new-cc [amount coins-to-use]
  (cond (= amount 0) 1
        (or (< amount 0) (no-more? coins-to-use)) 0
        :else (+ (new-cc amount (except-first-denomination coins-to-use))
                 (new-cc (- amount (first-denomination
                                    coins-to-use)) coins-to-use))))

(def us-coins (list 50 25 10 5 1))
(def uk-coins (list 100 50 20 10 5 2 1 0.5))

(new-cc 100 us-coins) ;= 292




(count-change-from-book 1) ;; 1
(count-change-from-book 2) ;; 1
(count-change-from-book 3) ;; 1
(count-change-from-book 4) ;; 1
(count-change-from-book 5) ;; 2
(count-change-from-book 10) ;; 4

;; Exercise 2.20

(defn same-parity [i & rest]
  (if (even? i) (cons i (filter even? rest))
      (cons i (filter odd? rest))))

(same-parity 1 2 3 4 5 6 7) ;= (1 3 5 7)
(same-parity 2 3 4 5 6 7) ;= (2 4 6)

(defn square [x] (* x x))

(defn square-list [items]
  (if (empty? items)
    nil
    (cons (square (first items)) (square-list (rest items)))))

(square-list (list 1 2 3 4)) ;= (1 4 9 16)

(defn square-list [items] (map square items))

;; Exercise 2.23

(defn for-each [f coll]
  (if (empty? coll) true
      (do
        (f (first coll))
        (for-each f (rest coll)))))

(for-each #(println %) (list 57 321 88)) ;
; 57
; 321
; 88
; true

;; Section 2.2.2

(defn count-leaves [x]
  (cond (not (seq? x)) 1
        (empty? x) 0
        :else (+ (count-leaves (first x))
                 (count-leaves (rest x)))))

(def x (cons (list 1 2) (list 3 4)))

x
(list? x)
(seq? x)
(empty? 1)
(count-leaves x) ;= 1

; Exercise 2.24

(list 1 (list 2 (list 3 4)))

; Exercise 2.25

(def foo '(1 3 (5 7) 9))

(first (rest (first (rest (rest foo))))) ;= 7

(def foo '((7)))

(first (first foo)) ;= 7

(def foo '(1 (2 (3 (4 (5 (6 7)))))))

foo

(first (rest (first (rest (first (rest (first (rest (first (rest (first (rest foo)))))))))))) ;= 7

; Exercise 2.26

(def x (list 1 2 3))
(def y (list 4 5 6))
x
y
(append x y) ;= (1 2 3 4 5 6)
(cons x y) ;= ((1 2 3) 4 5 6)
(list x y) ;= ((1 2 3) (4 5 6))

; Exercise 2.27

(def x (list (list 1 2) (list 3 4)))
x ;= ((1 2) (3 4))

(defn deep-reverse [coll]
  "Returns a list of the same elements in reverse order"
  (cond (not (seq? coll)) coll
        (empty? coll) coll
        :else (append (deep-reverse (rest coll))
                      (list (deep-reverse (first coll))))))

(deep-reverse x) ;= ((4 3) (2 1))

; Exercise 2.28

(def x (list (list 1 2) (list 3 4)))
x ;= ((1 2) (3 4))

(defn fringe [tree]
  "Returns a list whose elements are all the leaves of the tree in
  left-to-right order"
  (cond (not (seq? tree)) (list tree)
        (empty? tree) tree
        :else (concat (fringe (first tree)) (fringe (rest tree)))))

(fringe x) ;= (1 2 3 4)

; Exercise 2.29

(defn make-mobile [left right]
  (list left right))

(defn make-branch [length structure]
  (list length structure))

(defn left-branch [mobile]
  (first mobile))

(defn right-branch [mobile]
  (first (rest mobile)))

(defn branch-length [branch]
  (first branch))

(defn branch-structure [branch]
  (first (rest branch)))

(defn branch-weight [branch]
  (let [structure (branch-structure branch)]
    (if (number? structure)
      structure
      (total-weight structure))))

(defn total-weight [mobile]
  (+ (branch-weight (left-branch mobile))
     (branch-weight (right-branch mobile))))

(defn branch-torque [branch]
  (let [length (branch-length branch)
        weight (branch-weight branch)]
    (* length weight)))

(defn branch-balanced? [branch]
  (let [structure (branch-structure branch)]
    (if (number? structure)
      true
      (mobile-balanced? structure))))


(defn mobile-balanced? [mobile]
  (let [left (left-branch mobile)
        right (right-branch mobile)
        left-torque (branch-torque left)
        right-torque (branch-torque right)]
    (and (= left-torque right-torque)
         (branch-balanced? left)
         (branch-balanced? right))))

(defn scale-tree [tree factor]
  (cond (not (seq? tree)) (* tree factor)
        (empty? tree) tree
        :else (cons (scale-tree (first tree) factor)
                    (scale-tree (rest tree) factor))))

(scale-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)) 10)
;= (10 (20 (30 40) 50) (60 70))

(defn scale-tree [tree factor]
  (map #(if (seq? %) (scale-tree % factor) (* % factor)) tree))

(scale-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)) 10)
;= (10 (20 (30 40) 50) (60 70))

(defn square-tree [tree]
  (map #(if (seq? %) (square-tree %) (* % %)) tree))

(square-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)))
;= (1 (4 (9 16) 25) (36 49))

; Exercise 2.31

(defn tree-map [f tree]
  (map #(if (seq? %) (tree-map f %) (f %)) tree))

(defn square-tree [tree]
  (tree-map square tree))

(square-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)))
;= (1 (4 (9 16) 25) (36 49))

; Exercise 2.32

(defn subsets [s]
  (do
    (println (str "Looking for subsets of " (apply str s)))
    (cond (empty? (rest s)) (list s)
          :else
          (let [rest-subsets (subsets (rest s)),
                rest-subsets-with-this-item
                (map #(concat (list (first s)) %) rest-subsets)]
            (do
              (println (str "rest-subsets" (apply str rest-subsets)))
              (println (str "rest-subsets-with-this-item"
                            (apply str rest-subsets-with-this-item)))
              (concat rest-subsets rest-subsets-with-this-item))))))



(subsets '(1 2 3)) ;= ((3) (2 3) (1 3) (1 2 3))
(subsets '(3 1)) ;= ((1) (3 1))

(defn subsets [s]
  (if (empty? s) (list (list))
      (let [rest-subsets (subsets (rest s))]
        (append rest-subsets (map #(cons (first s) %) rest-subsets)))))

(subsets '(1 2 3)) ;= (() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3))
(subsets '(3 1)) ;= user=> (() (1) (3) (3 1))


; Section 2.2.3

(map square (list 1 2 3 4 5))

(defn filter [predicate sequence]
  (cond (empty? sequence) sequence
        (predicate (first sequence)) (cons (first sequence)
                                           (filter predicate (rest sequence)))
        :else (recur predicate (rest sequence))))

(filter odd? (list 1 2 3 4 5)) ;= (1 3 5)

(defn accumulate [op initial sequence]
  (if (empty? sequence) initial
      (op (first sequence)
          (accumulate op initial (rest sequence)))))

(accumulate + 0 (list 1 2 3 4 5)) ;= 15

(accumulate * 1 (list 1 2 3 4 5)) ;= 120

(accumulate cons nil (list 1 2 3 4 5)) ;= (1 2 3 4 5)

(defn enumerate-interval [low high]
  (if (> low high)
    nil
    (cons low (enumerate-interval (+ low 1) high))))

(enumerate-interval 2 7) ;= (2 3 4 5 6 7)

(defn enumerate-tree [tree]
  (cond (not (seq? tree)) (list tree)
         (empty? tree) nil
         :else (append (enumerate-tree (first tree))
                       (enumerate-tree (rest tree)))))

(enumerate-tree (list 1 (list 2 (list 3 4) 5))) ;= (1 2 3 4 5)

(defn sum-odd-squares [tree]
  (accumulate +
              0
              (map square
                   (filter odd?
                           (enumerate-tree tree)))))

(defn fib [n]
  (loop [a 1
         b 0
         count n]
    (if (= count 0)
      b
      (recur (+ a b) a (- count 1)))))

(defn fib-faster [n]
  (fib-iter 1 0 n))


(defn even-fibs [n]
  (accumulate cons
              nil
              (filter even?
                      (map fib
                           (enumerate-interval 0 n)))))

(even-fibs 9)

(defn list-fib-squares [n]
  (accumulate cons
              nil
              (map square
                   (map fib
                        (enumerate-interval 0 n)))))

(list-fib-squares 10) ;= (0 1 1 4 9 25 64 169 441 1156 3025)

(defn product-of-squares-of-odd-elements [sequence]
  (accumulate *
              1
              (map square
                   (filter odd? sequence))))

; Exercise 2.33

(defn map [p sequence]
  (accumulate (fn [x y] (cons (p x) y)) nil sequence))

(map #(+ 2 %) (list 1 2 3)) ;= 3 4 5

(defn append [seq1 seq2]
  (accumulate cons seq2 seq1))

(append '(1 2 3) '(4 5 6)) ;= (1 2 3 4 5 6)

(defn length [sequence]
  (accumulate (fn [x y] (inc y)) 0 sequence))

(length '(1 2 3 4 5)) ;= 5

; Exercise 2.3.4

(defn horner-eval [x coefficient-sequence]
  (accumulate (fn [this-coeff higher-terms] (+ (* higher-terms x)
                                               this-coeff))
              0
              coefficient-sequence))

(horner-eval 2 (list 1 3 0 5 0 1)) ;= 79

;= 1 + 3*2 + 5*2^3 + 2^5 = 79

; Exercise 2.35

(defn count-leaves[t]
  (accumulate
   +
   0
   (map #(if (seq? %) (count-leaves t) 1) t)))

; Exercise 2.36

(defn accumulate-n [op init seqs]
  "Takes an operator, the initial value, and a collection of
   collections.  The operator is used to fold the first element of the
   sequences, then the second, etc until done."
  (if (empty? (first seqs))
    nil
    (cons (accumulate op init (map first seqs))
          (accumulate-n op init (map rest seqs)))))

; Exercise 2.37

(defn dot-product [v w]
  "Dot product of two vectors"
  (accumulate + 0 (clojure.core/map * v w)))

(def V1 '(1 2 3))
(def V2 '(4 5 6))

(dot-product V1 V2)

(defn matrix-*-vector [m v]
  (map #(dot-product % v) m))

(defn transpose [mat]
  (accumulate-n cons (list) mat))

(def testmatrix
  '((1 2 3 4)
    (4 5 6 6)
    (6 7 8 9)))

testmatrix

(transpose testmatrix)
;= ((1 4 6) (2 5 7) (3 6 8) (4 6 9))


;; p_ij = Sum_k m_ik n_kj

(defn matrix-*-matrix [m n]
  (let [cols (transpose n)]
    (map #(matrix-*-vector cols %) m)))

(def A '((1 2) (3 4)))
(def B '((5 6) (7 8)))
(matrix-*-matrix A B)
;= ((19 22) (43 50))

; '((1*5 + 2*7, 1*6 + 2*8)
;   (3*5 + 4*7, 3*6 + 4*8))

; '((19, 22)
;   (43, 50))

;; Exercise 2.38

(defn fold-left [op initial sequence]
;  "Combines the the result of combining all the elements to the left
;   with the last element of the sequence"

  (loop [result initial my-rest sequence]
    (if (empty? my-rest)
      result
      (recur (op result (first my-rest))
             (rest my-rest)))))

(def fold-right accumulate)
;  "Combines the first element of the sequence with the result of
;   combining all the elements to the right"

(fold-right / 1 '(1 2 3)) ;= 3/2

; ((3/1)/2/1) = 1.5

(fold-left / 1 '(1 2 3)) ;= 1/6

; (((1/1)/2)/3) = .16666666666666666666

(fold-right list (list) (list 1 2 3))
; = (1 (2 (3 ())))

(list 1 (list 2 (list 3 (list))))


(fold-left list (list) (list 1 2 3))
; user=> (((() 1) 2) 3)

(list (list (list (list) 1) 2) 3)
; => (((() 1) 2) 3)

; '(1 2 3)
; '(3 2 1)
(defn reverse [coll]
  (fold-right #(concat %2 (list %1)) (list) coll))

(reverse '(1 2 3)) ;= (3 2 1)

(defn reverse [coll]
  (fold-left #(cons %2 %1) (list) coll))

(reverse '(1 2 3))

(cons 3 '(2 1))
; (foo 3 '(2 1))


(defn ordered-pairs [n]
  (accumulate append
              nil
              (map (fn [i]
                     (map (fn [j] (list i j))
                          (enumerate-interval 1 (- i 1))))
                   (enumerate-interval 1 n))))

(ordered-pairs 5)
;= ((2 1) (3 1) (3 2) (4 1) (4 2) (4 3) (5 1) (5 2) (5 3) (5 4))

(defn flatmap [proc coll]
  (accumulate append nil (map proc coll)))

(defn ordered-pairs [n]
  "Returns all pairs of distinct positive integers (i j) where 1 <= j
  < i <= n, such that i+j is prime"
  (flatmap (fn [i]
             (map (fn [j] (list i j))
                  (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

(ordered-pairs 5)
;= ((2 1) (3 1) (3 2) (4 1) (4 2) (4 3) (5 1) (5 2) (5 3) (5 4))

(defn prime-sum? [pair]
  (prime? (+ (first pair) (second pair))))

(prime-sum? '(2 1)) ;= true
(prime-sum? '(4 2)) ;= false

(defn make-pair-sum [pair]
  (list (first pair) (second pair) (+ (first pair) (second pair))))

(make-pair-sum '(4 2)) ;= (4 2 6)

(defn prime-sum-pairs [n]
  (map make-pair-sum
       (filter prime-sum?
               (ordered-pairs n))))


(prime-sum-pairs 5) ;= ((2 1 3) (3 2 5) (4 1 5) (4 3 7) (5 2 7))

(defn remove [item coll]
  (filter #(not (= % item))
          coll))


(defn permutations [set]
  (if (empty? set)
    (list nil)
    (flatmap (fn [each-item]
               (map (fn [smaller-permutation] (cons each-item
                                                    smaller-permutation))
                    (permutations (remove each-item set))))
             set)))

(permutations '(1 2 3 4 5))

;; Exercise 2.40

; already done, see above.

;; Exercise 2.41

(defn sum-to? [s & rest]
  (= s (apply + rest)))

(enumerate-interval 1 5) ;= (1 2 3 4 5)

(defn ordered-triples [n]
  (flatmap (fn [i]
             (map
              #(cons i %)
              (ordered-pairs (dec i))))
           (enumerate-interval 1 n)))

(ordered-triples 5)
;= ((3 2 1) (4 2 1) (4 3 1) (4 3 2) (5 2 1) (5 3 1) (5 3 2) (5 4 1) 
;   (5 4 2) (5 4 3))



(defn ordered-triples-that-sum-to-s [n s]
  "All ordered triples of distinct positive integers i, j and k where
  all are <= n that sum to s."
  (filter (partial sum-to? s)
          (ordered-triples n)))

;; Exercise 2.42

(defn adjoin-position [new-row k board]
  "Adjoins a new row-column position to a set of positions"
  (assoc board k new-row))

(def empty-board
  "Represents an empty set of positions.  This is a map from column
  number to row position"
  {})

(def example-board {1 5, 2 4, 3 3, 4 2, 5 1})

(defn get-row [column board]
  (board column))

(get-row 4 example-board) ;= 2

(defn columns-for-row [row board]
  (map
   #(first %)
   (filter #(= (second %) row) board)))

(columns-for-row 2 example-board) ;= (4)

(defn all-cols [board]
  (keys board))

(defn safe-in-row? [column board]
  ;; get count of rows
  (let [row (get-row column board)]
    (empty?
     (filter #(not= column %) (columns-for-row row board)))))

(defn safe-in-column? [column board]
  true) ; with this representation, nothing can have two entries in
        ; same column

(safe-in-row? 1 example-board)

(defn safe-in-diagonal? [column board]
  (let [row (get-row column board)
        differential (+ row column)
        all-columns (all-cols board)
        rest-columns (filter #(not= column %) all-columns)]
    (not
     (some
      identity
      (map
       #(= (+ (get-row % board) %) differential)
       rest-columns)))))
    ;; for each column, if differential is same as my differential,
    ;; then I fail.
    ;;
    ;;

(def example-board-bad-diagonal {1 5, 2 4, 3 3, 4 2, 5 1})

(def example-board-all-valid {1 3, 2 7, 3 2, 4 8, 5 5, 6 1, 7 4, 8 6})

(safe-in-diagonal? 1 example-board-bad-diagonal) ;= false

(safe-in-diagonal? 1 example-board-all-valid) ;= true

1 1
2 2
3 3

(defn safe? [column board]
  "Determines for a set of positions, whether the queen in the kth
  column is safe with respect to the others"
  ;; note row of kth column queen
  (and
   ;; see if anything else in same row
   (safe-in-row? column board)
   ;; see if anything else in same column
   (safe-in-column? column board)
   ;; see if anything is in same diagonal
   (safe-in-diagonal? column board)
   ))

(defn queens [board-size]
  "Returns sequence of all solutions to the problem of placing n
   queens on an n x n chessboard."
  (letfn [(queen-cols [k]
            ;; returns sequence of all ways to place queens in the
            ;; first k columns of the board-size board
            (if (= k 0)
              (list empty-board)
              (filter
               #(safe? k %)
               (flatmap
                (fn [rest-of-queens]
                  (map (fn [new-row]
                         (adjoin-position new-row k rest-of-queens))
                       (enumerate-interval 1 board-size)))
                (queen-cols (dec k))))))]
    (queen-cols board-size)))

(count (queens 5))
; 3125 for no constraints
; 120 for just row constraints
; 23 for column, row and diagonal constraints
