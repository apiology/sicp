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

;; Exercise 1.30

(defn sum-iter
  ([term next a b]
     (sum term next a b 0))
  ([term next a b sum-so-far]
     (if (> a b)
       sum-so-far
       (recur term next (next a) b (+ (term a) sum-so-far)))))

(defn sum-book [term next a b]
  (let [iter (fn [a result]
               (if (> a b)
                 result
                 (recur (next a) (+ (term a) result))))]
  (iter a 0)))

(def sum-cubes-3 (partial sum-iter cube inc))
(def sum-cubes-4 (partial sum-book cube inc))

(sum-cubes 1 2) ;= 9
(sum-cubes-2 1 2) ;= 9
(sum-cubes-3 1 2) ;= 9
(sum-cubes-4 1 2) ;= 9

;; Exercise 1.31

;; f(1) = 2/3 = (x+1)/(x+2)
  ;; f(2) = 4/3 = (x+2)/(x+1)
;; f(3) = 4/5 = (x+1)/(x+2)
  ;; f(4) = 6/5 = (x+2)/(x+1)
;; f(5) = 6/7 = (x+1)/(x+2)
  ;; f(6) = 8/7 = (x+2)/(x+1)



(defn product [term next a b]
  (if (> a b)
    1
    (* (term a) (product term next (next a) b))))

(def myterm #(double (if (odd? %) (/ (+ % 1) (+ % 2))
                        (/ (+ % 2) (+ % 1)))))

(myterm 1) ;= 2/3
(myterm 5) ;= 6/7
(def pi-approx (partial product myterm inc'))

(* 4 (pi-approx 1 1000)) ;= 3.143160820007324

;; Exercise 1.31(b)

(defn product-book [term next a b]
  (let [iter (fn [a result]
               (if (> a b)
                 result
                 (recur (next a) (* (term a) result))))]
  (iter a 1)))

(def pi-approx-2 (partial product-book myterm inc'))

(* 4 (pi-approx 1 1000)) ;= 3.143160820007324
(* 4 (pi-approx-2 1 1000)) ;= 3.143160820007324
(* 4 (pi-approx-2 1 100000)) ;= 3.1416083612781995

(defn accumulate [combiner null-value term next a b]
  (let [iter (fn [a result]
               (if (> a b)
                 result
                 (recur (next a) (combiner (term a) result))))]
  (iter a null-value)))


(def pi-approx-3 (partial accumulate * 1 myterm inc'))

(* 4 (pi-approx 1 1000)) ;= 3.143160820007324
(* 4 (pi-approx-2 1 1000)) ;= 3.143160820007324
(* 4 (pi-approx-2 1 100000)) ;= 3.1416083612781995

(* 4 (pi-approx-3 1 100000)) ;= 3.1416083612781995


(defn accumulate-recur [combiner null-value term next a b]
  (if (> a b)
    null-value
    (combiner (term a) (product term next (next a) b))))

(def pi-approx-4 (partial accumulate-recur * 1 myterm inc'))

(* 4 (pi-approx 1 1000)) ;= 3.143160820007324
(* 4 (pi-approx-2 1 1000)) ;= 3.143160820007324
(* 4 (pi-approx-2 1 100000)) ;= 3.1416083612781995
(* 4 (pi-approx-3 1 100000)) ;= 3.1416083612781995
(* 4 (pi-approx-4 1 100000)) ;= StackOverflow
(* 4 (pi-approx-4 1 1000)) ;= 3.143160705532265


(defn filtered-accumulate [the-filter combiner null-value term next a b]
  (let [iter (fn [a result]
               (if (> a b)
                 result
                 (if (the-filter a)
                   (recur (next a) (combiner (term a) result))
                   (recur (next a) result))))]
  (iter a null-value)))


;; sum of squares of the prime numbers in the interval a to b
(def sum-of-squares-of-primes
  (partial filtered-accumulate prime? + 0 #(* % %) a b))

(defn product-of-relative-primes [n]
  (filtered-accumulate
   #((= 1 (gcd % n))) ;; filter
   * ;; combineer
   1 ;; null value
   identity ;; just sum the numers
   1
   n))

;; Exercise 1.34

(defn square [x] (* x x))

(defn f [g]
  (g 2))

(f square) ;= 4

(f (fn [z] (* z (+ z 1)))) ;= 6

(f f)

;; Section 1.3.3

;; Half-interval method finds roots of continuous equation f(x).
;; i.e., x such that f(x) = 0

;; If we are given points a and b such that f(a) < 0 < f(b), then
;; there must be at least one root in between.

;; To locate a zero, try the average between a and b, then iterate
;; until the delta between zero is small enough.

;; This is theta(log(L/T)), where L is length of original interval and
;; T is the error tolerance.

(defn average [a b]
  (/ (+ a b) 2))

(def threshold 0.001)

(defn abs [x]
  (if (> 0 x) (- x) x))

(abs -2) ;= 2
(abs 2) ;= 2

(defn close-enough? [x y]
  (< (abs (- x y)) threshold))

(close-enough? 1 2) ;= false
(close-enough? 1 1.001) ;= true

(defn positive? [x] (> x 0))

(defn negative? [x] (< x 0))

(defn search [f neg-point pos-point]
  (let [midpoint (average neg-point pos-point)]
    (if (close-enough? neg-point pos-point)
      midpoint
      (let [test-value (f midpoint)]
        (cond (positive? test-value) (recur f neg-point midpoint)
              (negative? test-value) (recur f midpoint pos-point)
              :else midpoint)))))

(defn half-interval-method [f a b]
  (let [a-value (f a)
        b-value (f b)]
    (cond (and (negative? a-value) (positive? b-value)) (search f a b)
          (and (negative? b-value) (positive? a-value)) (search f b a)
          :else (throw (str "Values are not of opposite sign: " a b)))))

(half-interval-method #(Math/sin %) 2.0 4.0) ;= 3.14111328125

;; Fixed points

;; Fixed point of 'f' satisifies f(x) = x

;; some functions you can find this by apply f repeatedly
;; (f(f(f(f(guess))))) until it stops changing very much.

(def tolerance 0.00001)

(defn fixed-point [f first-guess]
  (defn close-enough? [v1 v2]
    (< (abs (- v1 v2)) tolerance))
  (defn my-try [guess]
    (let [next (f guess)]
      (if (close-enough? guess next)
        next
        (my-try next))))
  (my-try first-guess))

(defn fixed-point-let [f first-guess]
  (let [close-enough? (fn [v1 v2]
                        (< (abs (- v1 v2)) tolerance))
        my-try (fn [guess]
              (let [next (f guess)]
                (if (close-enough? guess next)
                  next
                  (my-try next))))]
    (my-try first-guess)))

(fixed-point #(Math/cos %) 1.0) ;= 0.7390822985224024
(fixed-point-let #(Math/cos %) 1.0) ;= 0.7390822985224024

;; fixed point to y= sin y + cos y
(fixed-point #(+ (Math/sin %) (Math/cos %)) 1.0) ;= 1.2587315962971173

;; finding square root involves finding y such that y^2 = x
;; ... y = x/y means we're looking for the fixed point of #(/ x y)
(defn sqrt-oscilates [x]
  (fixed-point #(/ x %) 1.0))

(sqrt-oscilates 2)

(defn sqrt [x]
  (fixed-point #(average % (/ x %))
               1.0))

(sqrt 2) ;= 1.4142135623746899

;; Exercise 1.35

;; Show that the golden ratio phi is the fixed point of the
;; transformation x |-> 1 + 1/x, and use this fact to compute phi by
;; means of the fixed point procedure.

;; phi is (1 + sqrt(5))/2
;; phi^2 is phi + 1

;; f(x) = x
;; phi^2 - 1 = phi
;; f(x) = phi^2


;; Phi = 1+1/Phi
;; Phi^2 = Phi + 1 ;; definition of Phi

(fixed-point #(+ 1 (/ 1 %)) 1.0) ;= 1.6180327868852458


xo (defn fixed-point-verbose [f first-guess]
  (defn close-enough? [v1 v2]
    (< (abs (- v1 v2)) tolerance))
  (defn my-try [guess]
    (let [next (f guess)]
      (do
        (println (str "Trying with " guess))
        (if (close-enough? guess next)
          next
          (my-try next)))))
  (my-try first-guess))


(fixed-point-verbose #(+ 1 (/ 1 %)) 1.0) ;= 1.6180327868852458

;; Find solution to x^x = 1000

;; Find fixed point of x |=> log(1000)/log(x)

(fixed-point-verbose #(/ (Math/log 1000) (Math/log %)) 3.0)


;; 32 steps with no damping


(fixed-point-verbose #(average % (/ (Math/log 1000) (Math/log %))) 3.0)

;; 8 steps with damping

;; Exercise 1.37

;; Infiite continued fraction expansion with N_i and D_i equal to 1
;; produces 1/Phi

;; One way to terminate is to try it for a certain number of terms

;; Suppose n and d are functions that produce N_i and D_i.


(defn cont-frac [n d num-iterations]
  (defn cont-frac-internal [i]
    (if (> i num-iterations) 0
        (/ (n i)
           (+ (d i)
              (cont-frac-internal (inc i))))))
  (cont-frac-internal 1))


;; should be 1/Phi - 
; 0.61803398874989484820
(cont-frac (fn [_] 1.0) (fn [_] 1.0) 100)
; 0.6180339887498948
(cont-frac (fn [_] 1.0) (fn [_] 1.0) 1) ;= 1.0
(cont-frac (fn [_] 1.0) (fn [_] 1.0) 5) ;= 0.625
(cont-frac (fn [_] 1.0) (fn [_] 1.0) 7) ;= 0.6190476190476191 ;; fail
(cont-frac (fn [_] 1.0) (fn [_] 1.0) 8) ;= 0.6176470588235294 ;; fail
(cont-frac (fn [_] 1.0) (fn [_] 1.0) 9) ;= 0.6181818181818182 ;; fail
(cont-frac (fn [_] 1.0) (fn [_] 1.0) 10) ;= 0.6179775280898876 ;; pass



(defn cont-frac-2 [n d num-iterations]
  (defn my-cont-frac-iter [n d i last-sum]
    (if (= i 0) last-sum
        (recur n d (dec i)
               (/ (n i) (+ (d i) last-sum)))))
  (my-cont-frac-iter n d num-iterations 0))


(cont-frac-2 (fn [_] 1.0) (fn [_] 1.0) 10) ;= 0.6179775280898876 ;; pass


;; Exercise 1.38:


;; e = 2.71828182845904523536028747135266249775724709369995


;; D_i = 1, 2, 1, 1, 4, 1, 1, 6, 1, 1, 8, ...

;; D(1)  = 1
;; D(2)  = 2
;; D(3)  = 1
;; D(4)  = 1
;; D(5)  = 4
;; D(6)  = 1
;; D(7)  = 1
;; D(8)  = 6
;; D(9)  = 1
;; D(10) = 1
;; D(11) = 8




(defn euler-D [n]
  (double (if (= (mod n 3) 2) (+ 2 (* 2 (quot n 3))) 1)))

(euler-D 1) ;= 1.0
(euler-D 11) ;= 8.0

(cont-frac (fn [_] 1) euler-D 100) ;= 0.7182818284590453

;; Exercise 1.39



(defn n-lambert [x i]
  (if (= i 1) x (- (* x x))))

(n-lambert 2 3) ;= 1

(defn d-lambert [i]
  (- (* 2 i) 1))

(defn tan-cf [x k]
  (cont-frac-2 (partial n-lambert x) d-lambert k))

;; should be -2.185039863261519
(double (tan-cf 2 100)) ;; -2.185039863261519

(def square #(* % %))
(defn average-damp [f]
  #(average % (f %)))

((average-damp square) 10)

(defn sqrt-damped [x]
  (fixed-point (average-damp #(/ x %))
               1.0))

(sqrt-damped 2) ;= 1.4142135623746899

(defn cube-root [x]
  (fixed-point (average-damp #(/ x (square %))) 1.0))

(Math/pow (cube-root 27) 3) ;= 27ish

;; Newton's Method

;; If g(x) is a differentiable function, then a solution of the
;; equation g(x) = 0 is a fixed point of the function f(x) where
;;
;;   f(x) = x - g(x) / (Dg(x))
;;
;; For many functions g and for sufficiently good initial guesses for
;; x, Newton's method converges very rapidly to g(x) = 0
;;

;; d(x^2)/dx = 2x ;; for instance

(def dx 0.00001)

(defn deriv [g]
  #(/ (- (g (+ % dx)) (g %))
      dx))

;; ( g(x+dx) - g(x) )/dx

(defn cube [x] (* x x x))

;; Let's derive x^3 at 5 ;; 3x^2 = 3*25 = 75

((deriv cube) 5) ;= 75-ish





(defn newton-transform [g]
  #(- % (/ (g %) ((deriv g) %))))

(defn newtons-method [g guess]
  "Finds x such that g(x) = 0"
  (fixed-point (newton-transform g) guess))

(defn sqrt-newtons [x]
  (newtons-method #(- (square %) x)
                  1.0))

(sqrt-newtons 2.0) ;= 1.4142135623822438


;; Exercise 1.40

;; Want to solve 0 = x^3 + ax^2 + bx + c

(defn cubic [a b c]
  #(+ (cube %) (* a (square %)) (* b %) c))

(newtons-method (cubic a b c) 1)

 ;; Exercise 1.41

(defn my-double [f]
  #(f (f %)))

((my-double inc) 3) ;= 5

(((my-double (my-double my-double)) inc) 0) ;= 16
(((my-double (my-double my-double)) inc) 5) ;= 21

((my-double inc) 1) ;= 3
(((my-double my-double) inc) 1) ;= 5

(inc 3)

;; Exercise 1.42

(defn compose [f g]
  #(f (g %)))

((compose square inc) 6) ;= 49

;; Exercise 1.43

(defn repeated [f n]
  (if (= n 0)
    identity
    (compose f (repeated f (dec n)))))

((repeated square 2) 5) ;= 625

;; Exerise 1.44

;; If f is a function and dx is some small number, then the smoothed
;; function of f is the function whose value at a point x is the
;; average of f(x-dx), f(x) an f(x+dx)

(defn smooth [f]
  #(/ (f (- % dx) 3 (+ % dx))))

(repeated smooth 3)

;; Exercise 1.45

;; Write procedure for cube root using multiple averaging

;; finding square root involves finding y such that y^2 = x
;; ... y = x/y means we're looking for the fixed point of #(/ x y)

(defn sqrt-damped [x]
  (fixed-point (average-damp #(/ x %))
               1.0))


;; y^3 = x

;; y = x/y^2

;; XXX now write same thing for cbrt

(defn cbrt-damped [x]
  (fixed-point (average-damp #(/ x (square %)))
               1.0))

(Math/pow (cbrt-damped 2398) 3)


(defn forth-root-damped [x]
  (fixed-point (average-damp #(/ x (cube %)))
               1.0))


;; doesn't converge...
(Math/pow (forth-root-damped 2323) 4)

(defn forth-root-damped-twice [x]
  (fixed-point ((compose average-damp average-damp) #(/ x (cube %)))
               1.0))

(Math/pow (forth-root-damped-twice 2323) 4)

(defn pow-recursive [x y]
  (if (= y 0) 1
      (* x (pow x (dec y)))))

(defn pow [x y]
  (loop [y y
         product-so-far 1]
    (if (= y 0) product-so-far
        (recur (dec y) (* x product-so-far)))))



(pow 2 10) ;= 1024


(defn nth-root-damped [n damping-factor]
  (fn [x]
    fixed-point ((repeated average-damp damping-factor) #(/ x (pow % (- n 1))))
    1.0))

(defn test-nth-root [n damping-factor]
  
)

(test-nth-root 4 1)

(nth-root-damped 3)
