(ns chapter1.core
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  ;; work around dangerous default behaviour in Clojure
  (println "Book's algorithm doing easy one - 49")
  (alter-var-root #'*read-eval* (constantly false))
  (println "Hello, World!"))

(defn square
  [x]
  (*' x x))

(square 2)
(defn average
  [x y]
  (/ (+ x y) 2))

(defn improve-sqrt
  [guess x]
  (average guess (/ x guess)))

(defn good-enough?
  [oldguess, newguess]
  (def ratio (/ newguess oldguess))
  (println "ratio is " ratio)
  (< (Math/abs (- ratio 1))
     0.0000000001))

(defn root-iter
   [guess x good-enough-fn improve-fn]
   (println guess)
   (def improved-guess (improve-fn guess x))
   (if (good-enough-fn guess improved-guess)
     guess
     (root-iter improved-guess x good-enough-fn improve-fn)))



(defn root-iter-clojure
   [guess x good-enough-fn improve-fn]
   (println guess)
   (def improved-guess (improve-fn guess x))
   (if (good-enough-fn guess improved-guess)
     guess
     (recur improved-guess x good-enough-fn improve-fn)))

(root-iter 1.1 2 good-enough? improve-sqrt)

(root-iter-clojure 1.1 2 good-enough? improve-sqrt)

(def factorial
  (fn [n]
    (loop [cnt n acc 1]
       (if (zero? cnt)
            acc
          (recur (dec cnt) (* acc cnt))))))

(factorial 1) ;; 1
(factorial 3) ;; 6



(defn sqrt
  [x]
  (root-iter 1.0 x good-enough? improve-sqrt))

(defn improve-cube-root
  [guess x]
  (/ (+ (/ x (* guess guess)) 
        (* guess 2))
     3))



(defn cbrt
  [x]
  (root-iter 1.0 x good-enough? improve-cube-root))



(sqrt 0.000000000001)
(sqrt 2000000000000000000000000)



(defn cbrt
  [x]
  (cbrt-iter 1.0 x))


-37

(* 3 4)

(> 10 9.7)

(- (if (> 3 4)
     7
     10)
   (/ 16 10))

(- 10 (/ 16 10))

(- 10 1.6)
8.4


(/ 42 5.0)


(* (- 25 10)
   (+ 6 3))
(* 15 9)

+

(defn double [x] (* 2 x))
(def times-2 double)
(times-2 4)
(def c 4)

c

(double c)

(double (double (+ c 5)))
`#(double %)

(def times-2 #(double %))







      (defn double [x] (* 2 x))
      (def times-2 double) ;; valid
      (times-2 4) ;; 8

      (def times-2 #(double %))
      (times-2 4) ;; 8

(def )

c
(def d c)

d

(cond (>= c 2) d
      (= c (- d 5)) (+ c d)
      :else (Math/abs (- c d)))

=> 4

;; with C-j
(defn abs
  [a]
  (if (> a 0)
    a
    (- a)))

;; with <enter>
(defn abs
  [a]
  (if (> a 0)
    a
    (- a)))

(abs 1)

(abs -1)

(lambda (x) (* x x))

(def mydouble #(* % %))

(mydouble 5)

(#(* % %) 5)

(def sum-of-squares #(+ (* %1 %1) (* %2 %2)))

(sum-of-squares 5 6)

(def square #(* % %))

(def square #(* % %))

(def sum-of-squares 
  #(+ (square %1) (square %2)))


(def sum-of-squares 
  (let [square #(* % %)]
    #(+ (square %1) (square %2))))

(def close-enuf? #(< (abs (- (square %1) %2)) 0.001))

(def close-enuf? (fn [guess x] 
                   (< (abs
                       (- (square guess) x))
                      0.001))

(def average (fn [x y] (* (+ x y) 0.5)))

(average 1 2)


)

(fn [guess x] (< (abs (- (square guess) x)) 0.001))

(def improve 
  (fn [guess_of_sqrt value] 
    (average guess_of_sqrt (/ value guess_of_sqrt))))

(def sqrt-loop 
  (fn [guess_of_sqrt number]
    (if (close-enuf? guess_of_sqrt number)
      guess_of_sqrt
      (sqrt-loop (improve guess_of_sqrt number) number))))

(def sqrt
  (fn [x]
    (sqrt-loop 1.0 x)))

(sqrt 2)


(def sqrt
  (let [
        average 
        #(* (+ %1 %2) 0.5)
        improve 
        (fn [guess_of_sqrt value] (average guess_of_sqrt (/ value guess_of_sqrt)))
        square 
        #(* % %)
        abs
        (fn [a]
          (if (> a 0)
            a
            (- a)))
        close-enuf? 
        (fn [guess x] 
          (< (abs
              (- (square guess) x))
             0.001))
        sqrt-loop 
        (fn sqrt-loop [guess_of_sqrt number]
          (if (close-enuf? guess_of_sqrt number)
            guess_of_sqrt
            (sqrt-loop (improve guess_of_sqrt number) number)))]
    (fn [x]
      (sqrt-loop 1.0 x))))

(sqrt 2)

(defn sq
  [x]
  (* x x))


(defn sos 
  [x y]
  (+ (sq x) (sq y)))

(sos 5 2)

(defn move
  [n from to spare]
  (cond (= n 0) "done"
        :else (move (dec n) from spare to)))

(defn fact 
  [n]
  (let [fact-it
        (fn [n product-so-far]
          (if (= n 1)
            product-so-far
            (recur (dec n) (* product-so-far n))))]
    (fact-it n 1)))

(fact 5)

  (defn fact
        [n]
        (if (= n 1)
            1
            (* n (fact (- n 1)))))




(defn A 
  [x y]
  (cond 
    (= y 0) 0
    (= x 0) (* 2 y)
    (= y 1) 2
    :else (A (- x 1) (A x (- y 1)))))

(A 2 4)

(A 3 3)
(A 0 1) ;; 2
(A 0 2) ;; 4
(A 0 3) ;; 6
(A 0 4) ;; 8

(A 2 0) ;; 0
(A 2 1) ;; 2
(A 2 2) ;; 4
(A 2 3) ;; 16
(A 2 4) ;; 65536
(A 2 5) ;; ...


(A 1 0) ;; 0
(A 1 1) ;; 2
(A 1 2) ;; 4 ;;
(A 1 3) ;; 8
(A 1 4) ;; 16
(A 1 5) ;; 32
(A 1 6) ;; 64
(A 1 7) ;; 128
(A 1 8) ;; 256

(A 2 0) ;; 0 = 0
(A 2 1) ;; 2 = 2
(A 2 2) ;; 4 = 2^2
(A 2 3) ;; 16 = 2^2^2
(A 2 4) ;; 65536
(A 2 999) ;; 



;; towers of hanoi

;; Write a function of the following form:
(hanoi (1 2 3 4 5 6 7 8 9

;; challenge - move an n-high tower from a spike called 'form' to a
;; spike called 'to' with a spike called 'spare' also existing.

(defn print-move
  [from to]
  (println (str "Move a piece from " from " to " to)))

(print-move :one :two)


)
(defn move
  [n from to spare]
  (cond 
   (= n 0) "done"
   :else (do
           (move (dec n) from spare to) ;; move smaller tower to 'spare' spike
           (print-move from to) ;; move the largest guy to the 'to' spike
           (move (dec n) spare to from)))) ;; move the smaller tower from the spare spike to o                                           ;; top of the largest guy which is already moved

;; idea: have move return a list...create list of moves overall...then figure out the rest.

(defn move-iter-1
  [n from to spare]
  (cond 
   (= n 0) '()
   :else (let [before (move-fn (dec n) from spare to)
               after (move-fn (dec n) spare to from)]
           (concat before (list (list from to)) after))))


;; (solve n)
;;;; returns ((:1 :2) (:3 :2) ...)

;; (solve) function calls:
;;;; foo
;;;;;; before: ((n from to spare) ...)
;;;;;; moves: ((from to) (spare from) (from spare))
;;;;;; after: ((n from to spare) ...)

;; end case - before and after are empty

(defn solve
  [n]
  (foo (vector (list n :1 :2 :3)) '() '())


  

)


(defn foo
  [num-to-move from to spare before moves after]
  (cond
   (= n 1) (recur 0 from to spare before (cons (list from to) moves) after)
   
  (cond (and? (empty? before) (empty? after)) (cons (list from to) moves)
        ()
        
              
   )

;; http://wanttowriteprograms.blogspot.com/2007/03/towers-of-hanoi-3.html
;; http://codejournal.blogspot.com/2007/12/iterative-solution-to-towers-of-hanoi.html
;; http://www.skorks.com/2010/03/solving-the-towers-of-hanoi-mathematically-and-programmatically-the-value-of-recursion/


(defn move-fn
  [n from to spare]
  (cond 
   (= n 0) '()
   :else (concat
           (move-fn (dec n) from spare to) ;; move smaller tower to 'spare' spike
           (list (list from to)) ;; move the largest guy to the 'to' spike
           (move-fn (dec n) spare to from)))) ;; move the smaller tower from the spare spike to
                                              ;; top of the largest guy which is already moved


;; -inst are insructions come in form  of (n from to spare)
;; -moves come as an array in form [(from to) ...]

(defn move-fn-iter
  [before-inst before-moves cur-move after-moves after-inst]
  (let [[n from to spare] cur-move]
    (if (= n 0) 
      (cond (seq before-inst) 
            ;; process the next before-inst
            (recur (butlast before-inst)
                   before-moves
                   (last before-inst)
                   after-moves
                   after-inst)
            (seq after-inst)
            ;; process the next after-inst
            (recur before-inst
                   before-moves
                   (first after-inst)
                   after-moves
                   (rest after-inst))
            :else
            (concat before-moves after-moves))
      (let [next-move (list from to)
            next-before-inst (list (dec n) from spare to)
            next-after-inst (list (dec n) spare to from)]
        (cond (seq before-inst) 
              ;; process the next before-inst
              (recur (conj (butlast before-inst) next-before-inst)
                     (conj before-moves next-move)
                     (last before-inst)
                     after-moves
                     (cons next-after-inst after-inst))
              (seq after-inst)
              ;; process the next after-inst
              (recur before-inst
                     before-moves
                     (first after-inst)
                     (cons after-moves next-move)
                     (cons next-after-inst (rest after-inst)))
              ;; done with instructions - return
              :else
              ;;(concat before-moves next-move after-moves))))))
              (recur (array)
                     before-moves
                     next-before-inst
                     after-moves
                     (cons next-after-inst after-inst))

(defn move
  [n]
  (move-fn-iter [] [] (list n :from :to :spare) () ())
)

(move 2)


    before-moves (list from to)
  
  (let [(before-n before-from before-to before-spare) before
        (after-n after-from after-to after-spare) after]
    
      )
)


  

;; pseudo code
;; 

(defn print-moves
  [n from to spare before-moves moves after-moves]
  (case
      (everything-done? before-moves moves after-moves) "done"
      :else (let
                [next-problem (car after-moves)
                 next-n (nth 1 next-problem)
                 next-from (nth 2 next-problem)
                 next-to (nth 3 next-problem)
                 next-spare (nth 3 next-problem)
                 next-after-moves (cdr after-moves)]
              (print-move from to)
              (recur next-n next-from next-to next-spare '[] '() next-after-moves)
              :else
              )
      :else
      ()))

(defn everything-done? [before-moves moves after-moves]
  (and
   (empty? before-moves)
   (empty? moves)
   (empty? after-moves)))


              
                                 (print-move (car moves))
                                 (recur before-moves (cdr moves) after-moves)
]
    (recur (conj before-moves before-move) (cons after-moves after-move))
)










(defn move-iter-2
  [before now after] ;; before and after are lists of (n from to spare).
                     ;; now is a list of moves ((from to) (spare from) (from spare))
  (let [n (car (car before))
        n (car before)]
    
  (
   (= n 0) 
   :else (let [before (list (dec n) from spare to)
               after (list (dec n) spare to from)]
           (recur concat before (list (list from to)) after))))

(move-iter 5 :1 :2 :3)

(defn our-remove-if [fun lst]
  (if (empty? lst) nil
      (if (fun (first lst))
        (our-remove-if fun (rest lst))
        (cons (first lst) (our-remove-if fun (rest lst))))))

(our-remove-if even? '(1 2 3 4 5))

(defn behave [animal]
  ((get animal 'behavior)))


;; 1.2.2.         Tree Recursion

(defn fib-slow [n]
  (cond (= n 0) 0
        (= n 1) 1
        :else (+ (fib-slow (- n 1))
                 (fib-slow (- n 2)))))

(defn fib-iter [a b count]
  (if (= count 0)
    b
    (recur (+ a b) a (- count 1))))

(defn fib-faster [n]
  (fib-iter 1 0 n))

;; i was surprised that the do form hides exceptions of the first statements...



(do (map #(throw (new AssertionError (str "Assert failed in " %))) '("first"))
    true)

(fib 100)
(defn count-change-from-book [amount]
  (cc-from-book amount 5))

(defn cc-from-book [amount kinds-of-coins]
  (cond (= amount 0) 1
        (or (< amount 0) (= kinds-of-coins 0)) 0
        :else (+ (cc-from-book amount
                               (- kinds-of-coins 1))
                 (cc-from-book (- amount
                                  (first-denomination kinds-of-coins))
                               kinds-of-coins))))

(count-change-from-book 1) ;; 1
(count-change-from-book 2) ;; 1
(count-change-from-book 3) ;; 1
(count-change-from-book 4) ;; 1
(count-change-from-book 5) ;; 2
(count-change-from-book 10) ;; 4

;; number of ways to change amount a using n kinds of coins:
;;;; number of ways to change amount a using all but the first kind of coin, +
;;;; number of ways to change amount a-d using all n kinds of coins, where d is 
;;;;;; the denomination of the first kind of coin


(defn first-denomination [kinds-of-coins]
  (cond (= kinds-of-coins 1) 1
        (= kinds-of-coins 2) 5
        (= kinds-of-coins 3) 10
        (= kinds-of-coins 4) 25
        (= kinds-of-coins 5) 50))

(defn my-count-change [amount]
  (my-cc amount 5))

(defn my-cc [amount kinds-of-coins]
  (cond (= amount 0) 1
        (< amount 0) 0
        (= kinds-of-coins 0) 0
        :else
        (let [denom (first-denomination kinds-of-coins)]
          (+ 
           (my-cc amount (dec kinds-of-coins))
           (my-cc (- amount denom) kinds-of-coins)))))

(my-count-change 5) ;; 2
(my-count-change 10) ;; 4
(my-count-change 1000) ;; 292
(my-count-change-memoized 1000) ;; 292
(my-count-change-memoized 950) ;; 292
(def my-count-change-memoized (memoize my-count-change))


(defmacro memoize-fn
  "Produces a memoized anonymous function that can recursively call itself."
  [fn-name & fn-args]
  `(with-local-vars
      [~fn-name (memoize
                (fn ~@fn-args))]
     (.bindRoot ~fn-name @~fn-name)
    @~fn-name))

(def my-cc-memoized
  (memoize-fn my-cc
              [amount kinds-of-coins]
              (cond (= amount 0) 1
                    (< amount 0) 0
                    (= kinds-of-coins 0) 0
                    :else
                    (let [denom (first-denomination kinds-of-coins)]
                      (+ 
                       (my-cc (- amount denom) kinds-of-coins)
                       (my-cc amount (dec kinds-of-coins)))))))

(my-cc-memoized 40000 5)

(defn my-count-change-memoized [amount]
  (let [my-cc-memoized (memoize my-cc)]
        (my-cc-memoized amount 5)))
            

;; Exercise 1.11

(defn f [n]
  (if (< n 3) n
      (+
       (f (dec n))
       (* 2 (f (- n 2)))
       (* 3 (f (- n 3))))))

(f 1) ;; 1
(f 2) ;; 2
(f 3) ;; 4 = 2 + 2*1+3*0
(f 4) ;; 11 = 4 + 2*2 + 3*1
(f 5) ;; 25 = 11 + 2*4 + 3*2
(f 40) ;; never returns

(defn zero-if-nil [n]
  (if (nil? n) 0 n))

(defn f-new [n]
  (case (< n 3) n
        :else (first (f-it (- n 2) '(2 1 0)))))

(defn f-it [num-more-to-create lst]
    (if (= 0 num-more-to-create) lst
        (let [nminus1 (nth lst 0 0)
              nminus2 (nth lst 1 0)
              nminus3 (nth lst 2 0)
              next-item (+ nminus1 (* 2 nminus2) (* 3 nminus3))]
          (recur (dec num-more-to-create) (cons next-item lst)))))

(f-it 3 '(2 1 0))

(def lst '(2 1 0))

(f-new 40) ;; comes back almost immediately

(nth lst 2 0)

(nth (list) 1)
(f-new 1)

;; Exercise 1.12



;; 1-indexed
(defn pascals-triangle [row-num col-num]
  (cond (<= row-num 2) 1
        (= col-num 1) 1
        (= col-num row-num) 1
        :else
        (+
         (pascals-triangle (dec row-num) (dec col-num))
         (pascals-triangle (dec row-num) col-num))))
)

(pascals-triangle 1 1) ;; should be 1
(pascals-triangle 2 2) ;; should be 1
(pascals-triangle 4 3) ;; should be 3
(pascals-triangle 5 3) ;; should be 6

;; Exercise 1.13


;; Given definition of Fib(x):
;;   Fib(1) = 1
;;   Fib(2) = 1
;; else
;;   Fib(n) = Fib(n-1) + Fib(n-2)

;; Prove that Fib(n) is the closest integer to ϕⁿ / √5, where ϕ = (1 + √5)/2
;;
;; Hint: Let Ψ = (1 - √5)/2.  
;;       Use induction and the definition of the Fibonacci numbers
;;       (see section 1.2.2) to prove that Fib(n) = (ϕⁿ - Ψⁿ) / √5

;; Lemma: ϕ+Ψ = (1 + √5)/2 + (1 - √5)/2 = ((1+√5) + (1-√5))/2 = (2/2) = 1
;;        ϕ+Ψ = 1
;;
;; Lemma: ϕ-Ψ = (1 + √5)/2 - (1 - √5)/2 = ((1+√5) - (1-√5))/2 = (0+2√5)/2= √5
;;
;; Lemma: ϕ² - Ψ² = (ϕ-Ψ)(ϕ+Ψ) = 1*√5 = √5
;; Lemma: ϕⁿ - Ψⁿ = ???
;;    (ϕ-Ψ) Sum[ϕ^(-i + n) Ψ^(-1 + i), {i, 1, n}]
;;    bleh!
;;    
;; To be proved: Fib(n) is the closest integer to ϕⁿ / √5, where ϕ = (1 + √5)/2
;; Universe: all positive integers.
;; Base cases:
;;   P(1): Fib(1) = ϕ¹ / √5
;;     Fib(1) ≟ ϕ¹ / √5
;;     1 ≟ round(ϕ¹ / √5) # by definition of Fib
;;     1 ≟ round(ϕ / √5) # by definition of exponentiation
;;     1 ≟ round((1 + √5) / 2) / √5 # by definition of ϕ
;;     1 ≟ round(.72360679774997896964)
;;     1 = 1
;;     ∎
;;   P(2): Fib(2) = ϕ² / √5
;;     Fib(1) ≟ ϕ² / √5
;;     1 ≟ round(ϕ² / √5) # by definition of Fib
;;     1 ≟ round(1.17082039324993690891) # bc(1)
;;     1 ≟ 1
;;     ∎
;; Inductive case:
;;   Proposition: P(n) is true if P(n-1) is true
;;   Assumptions: Fib(n-2) = round(ϕⁿ⁻² / √5)
;;   Assumptions: Fib(n-1) = round(ϕⁿ⁻¹ / √5)
;;   Definition: Ψ = (1 - √5)/2.  
;;   Lemma (not yet proved): Fib(n) = (ϕⁿ - Ψⁿ) / √5
;;      First base case: Fib(1) = (ϕ¹ - Ψ¹) / √5
;;        Fib(1) ≟ (ϕ¹ - Ψ¹) / √5
;;        Fib(1) ≟ (ϕ - Ψ) / √5 ;; definition of exponentiation
;;        Fib(1) ≟ (ϕ - Ψ) / √5
;;        Fib(1) ≟ √5 / √5 ;; by lemma above
;;        Fib(1) ≟ 1 ;; by lemma above
;;        ∎
;;      Second base case: Fib(2) = (ϕ² - Ψ²) / √5
;;        Fib(2) ≟ (ϕ² - Ψ²) / √5
;;        Fib(2) ≟  √5 / √5
;;        Fib(2) ≟  1
;;        ∎
;;      Inductive case:
;;        Fib(n) ≟ (ϕⁿ - Ψⁿ) / √5
;;        Fib(n) ≟ (ϕⁿ - Ψⁿ) / √5
;;        Fib(n-1) + Fib(n-2) ≟ (ϕⁿ - Ψⁿ) / √5
;;        ((ϕⁿ⁻¹ - Ψⁿ⁻¹) / √5) + ((ϕⁿ⁻² - Ψⁿ⁻²) / √5) ≟ (ϕⁿ - Ψⁿ) / √5 ;; def of Fib and inductive assumption
;;        (ϕⁿ⁻¹ - Ψⁿ⁻¹ + ϕⁿ⁻² - Ψⁿ⁻²) / √5) ≟ (ϕⁿ - Ψⁿ) / √5 ;; algebra
;;        ϕⁿ⁻¹ - Ψⁿ⁻¹ + ϕⁿ⁻² - Ψⁿ⁻² ≟ ϕⁿ - Ψⁿ ;; multiply both sides by √5
;;        (ϕⁿ⁻¹ + ϕⁿ⁻²) - (Ψⁿ⁻¹ + Ψⁿ⁻²) ≟ ϕⁿ - Ψⁿ ;; algebra
;;        (ϕ*ϕⁿ⁻² + ϕⁿ⁻²) - (Ψ*Ψⁿ⁻² + Ψⁿ⁻²) ≟ ϕⁿ - Ψⁿ ;; algebra
;;        (ϕ+1)*ϕⁿ⁻² - (Ψ+1)*Ψⁿ⁻² ≟ ϕⁿ - Ψⁿ ;; algebra
;;        (ϕ+1)*ϕⁿ⁻² - (Ψ+1)*Ψⁿ⁻² ≟ ϕⁿ - Ψⁿ ;; 
;;        Lemma: (ϕ+1) = ϕ²
;;          (ϕ+1) ≟ ϕ²
;;          (ϕ+1) ≟ ϕ²
;;          ((1 + √5)/2) + 1 ≟  ((1 + √5)/2)²
;;          2.61803398874989484820 ≟  2.61803398874989484818 ;; bc
;;          ∎
;;        Lemma: (Ψ+1) = Ψ²
;;          (Ψ+1) ≟ Ψ²
;;          (1 - √5)/2 + 1 ≟ ((1 - √5)/2)²
;;          (1 - sqrt(5))/2 + 1 ≟ ((1 - sqrt(5))/2)*((1 - sqrt(5))/2)
;;          .38196601125010515180 ≟ .38196601125010515178
;;          ∎
;;        (ϕ+1)*ϕⁿ⁻² - (Ψ+1)*Ψⁿ⁻² ≟ ϕⁿ - Ψⁿ
;;        ϕ²*ϕⁿ⁻² - Ψ²*Ψⁿ⁻² ≟ ϕⁿ - Ψⁿ
;;        ϕⁿ - Ψⁿ ≟ ϕⁿ - Ψⁿ
;;        ∎
;;   Lemma: round(ϕⁿ / √5) - round(Ψⁿ / √5) <= 1
;;   Want to prove that: Fib(n) = round(ϕⁿ / √5)
;;      Fib(n) ≟ round(ϕⁿ / √5)
;;      (ϕⁿ - Ψⁿ) / √5 ≟ round(ϕⁿ / √5) ;; by lemma
;;      ϕⁿ/√5 - Ψⁿ/√5 ≟ round(ϕⁿ / √5) ;; by lemma
;;     Definition:
;;        x = nearest(y) implies
;;          x > y - 0.5 AND 
;;          x <= y + 0.5
;;       examples:
;;              1 = round(1) ;; true for a, true for b
;;              1 = round(0.75) ;; true for a, true for b
;;              1 = round(0.5) ;; true for a, true for b
;;              1 != round(0.25) ;; true for a, false for b
;;              1 != round(1.5) ;; false for a, true for b
;;     Lemma: ϕⁿ/√5 - Ψⁿ/√5 > ϕⁿ/√5 - 1/2
;;        ϕⁿ/√5 - Ψⁿ/√5 >? ϕⁿ/√5 - 1/2
;;        ϕⁿ - Ψⁿ >? ϕⁿ - √5/2 ;; multiply by √5
;;        -Ψⁿ >? -√5/2 ;; subtract ϕⁿ
;;        Ψⁿ <? √5/2 ;; multiply by -1
;;        (1 - √5)ⁿ/2ⁿ <? √5/2
;;        (1 - √5)ⁿ/2ⁿ⁻¹ <? √5
;;        (1 - √5)ⁿ <? 2ⁿ⁻¹ * √5
;;        1 - √5 <? nthroot(2ⁿ⁻¹ * √5) ;; just start plugging #s in...
;;        ∎
;;     Lemma: ϕⁿ/√5 - Ψⁿ/√5 <= ϕⁿ / √5 + 1/2
;;        ϕⁿ/√5 - Ψⁿ/√5 <=? ϕⁿ/√5 + 1/2
;;        ϕⁿ/√5 - Ψⁿ/√5 <=? ϕⁿ/√5 + 1/2
;;        ϕⁿ-Ψⁿ <=? ϕⁿ + √5/2
;;        Ψⁿ <=? √5/2
;;        (1 - √5)ⁿ <=? 2ⁿ⁻¹√5
;;        ∎  ;; just start plugging #s in...
;;     ϕⁿ/√5 - Ψⁿ/√5 ≟ round(ϕⁿ / √5) ;; by two lemmas above
;;     ∎

;;        
;;      
;;      
;;   ;; note: sqrt(5) = 2.23606797749978969640
;;   ;; note: Ψ = -.61803398874989484820
;;   ;; note: ϕ = 1.61803398874989484820
;;   ;; note: ϕ + Ψ = 1
;;   ;; note: (ϕ + Ψ)ⁿ = 1
;;   ;; note: ϕ² + 2Ψϕ + Ψ² = 1
;;   ;; note: difference of squares results in binomial coeffiennts matching pascal's triangle
;;   ;; an – bn = (a – b)(an – 1 + an – 2b + an – 3b2 + ··· + abn – 2 + bn – 1)
;;   ;; http://www.mathwords.com/f/factoring_rules.htm




;; Exercise 1.14

(my-count-change 11)
(my-cc 11 5)
  (my-cc 11 4)
    (my-cc 11 3)
      (my-cc 11 2)
        (my-cc 11 1)
          (my-cc 11 0) = 0
          (my-cc 10 1)
            (my-cc 10 0) = 0
            (my-cc 9 1)
              (my-cc 9 0) = 0
              (my-cc 8 1)
                (my-cc 8 0)
                (my-cc 7 1)
                  (my-cc 7 0)
                  (my-cc 6 1)
                    (my-cc 6 0)
                    (my-cc 5 1)
                      (my-cc 5 0)
                      (my-cc 4 1)
                        (my-cc 4 0)
                        (my-cc 3 1)
                          (my-cc 3 0)
                          (my-cc 2 1)
                            (my-cc 2 0)
                            (my-cc 1 1)
                              (my-cc 1 0)
                              (my-cc 0 1).
        (my-cc 6 2)
          (my-cc 6 1)
          (my-cc 1 2)
            (my-cc 1 1)
              (my-cc 1 0)
              (my-cc 0 1)
            (my-cc 1 2)
              (my-cc 1 1)
                (my-cc 1 0) = 0
                (my-cc 0 1) = 1
              (my-cc -blah 2).
      (my-cc 1 3)
        (my-cc 1 2)
          (my-cc 1 1)
            (my-cc 1 0)
            (my-cc 0 1)
          (my-cc 0 1)
        (my-cc (- 1 10) 3) = 0
    (my-cc (- 11 25) 4) = 0
  (my-cc (- 11 50) 5) = 0


;; Exercise 1.15

(defn abs
  [a]
  (if (> a 0)
    a
    (- a)))

(defn cube [x] (* x x x))

(defn p [x]
  (- (* 3 x) (* 4 (cube x))))

(defn sine [angle]
  (if (not (> (abs angle) 0.1))
    angle
    (p (sine (/ angle 3.0)))))

(sine 12.15)

;; a. How many times is the procedure p applied when (sine 12.15) is evaluated?
;; 
;;;; (sine 12.15)
;;;;   (p (sine 4.16666666666666666666))
;;;;   (p (p (sine 1.38888888888888888888)))
;;;;   (p (p (p (sine .46296296296296296296))))
;;;;   (p (p (p (p (sine .15432098765432098765)))))
;;;;   (p (p (p (p (p (sine .05144032921810699588))))))
;;;;   (p (p (p (p (p (p (sine .05144032921810699588)))))))
;;;;   (p (p (p (p (p (p .05144032921810699588))))))

;;;;   answer: 6

;; b: Θ(log n)





;; Order of growth of space is Θ(n); each call uses constant space,
;; but the deepest it goes is to try to use pennies, which is Θ(n).

;; Order of growth of steps:
;;   Steps(n, 5) = 
;;     Steps(n, 4) + Steps(n-1, 5)
;;     Θ(n * Steps(n, 4))
;;     Θ(n * Θ(n⁴))
;;     Θ(n⁵)
;;
;;   Steps(n, 4) = 
;;     Steps(n, 3) + Steps(n-1, 4)
;;     Θ(n³) + Steps(n-1, 4)
;;     Θ(n⁴)
;;
;;   Steps(n, 3) = 
;;     Steps(n, 2) + Steps(n-1, 3)
;;     Θ(n²) + Steps(n-1, 3)
;;     Θ(n³)
;;
;;   Steps(n, 2) = 
;;     Steps(n, 1) + Steps(n-1, 2)
;;     Θ(n²)
;;     
;;
;;   Steps(n, 1) = 
;;     Steps(n, 0) + Steps(n-1, 1) ;; try by reducing once for each penny
;;     Θ(n)

at each level, it uses  of the deepest it goes in 

;; Cost(my-count-change amount) = 
;;    Cost(my-cc amount 5)

;;    Cost(my-cc amount kinds-of-coins) = 
;;      Cost(my-cc amount (dec kinds-of-coins)) +
;;      Cost(my-cc (- amount highest-denom) kinds-of-coins)

;; Cost(my-count-change n) = 
;;      O(2*2*2*...) ...but how many times?
;;      O(2^n)
;;      


;; Section 1.2.4 Exponentiation

;; Exercise 1.16

(defn expt-recursive [b n]
  (if (= n 0)
    1
    (*' b (expt-recursive b (- n 1)))))

(expt-recursive 14 16)

(defn expt-iter-internal [b counter product]
  (if (= counter 0)
    product
    (recur b
           (- counter 1)
           (*' b product))))

(defn expt-iter [b n]
  (expt-iter-internal b n 1))

(expt-iter 14 16)

(defn fast-expt
  [b n]
  (cond (= n 0) 1
        (even? n) (square (fast-expt b (/ n 2)))
        :else (*' b (fast-expt b (- n 1)))))

(defn even? [n]
  (= (mod n 2) 0))

(expt-recursive 50000 50000) ;; stack overflow
(expt-iter 50000000 50000000) ;; comes back fairly fast, but not instantly
(fast-expt 25 25) ; comes back instantly
(fast-expt 50000000 50000000) ;; comes back fairly fast, but not instantly


(defn square-n-times-then-multiply [b times-to-square-result final-multiply]
  (cond (= times-to-square-result 1) 
        (*' final-multiply b b)
        :else (recur b (dec times-to-square-result) (*' final-multiply b b))))

(defn fast-expt-iter
  ([b n]
     (fast-expt-iter b n 0 1))
  ([b n times-to-square-result final-multiply]
     (cond (= n 1) 
           (square-n-times-then-multiply b times-to-square-result final-multiply)
           
           (even? n) 
           (recur b (/ n 2) (inc times-to-square-result) final-multiply)

           :else 
           (recur b (dec n) times-to-square-result (*' b final-multiply)))))


(fast-expt-iter 6 6) ;;  should be 46656.0; bug comes back as 7776
(fast-expt-iter 6 6 0 1) ;;  6^6 squared zero times * 1 = 3888 ; came back as 7776
(fast-expt-iter 6 3 1 1) ;; (6^3)^2 * 1 = 46656.0 ;; check ; came back as 7776
(fast-expt-iter 6 2 1 6) ;; b^n squared x times * final-multiply





(square-n-times-then-multipl)

(fast-expt-iter 50000000 50000000) ; comes back instantly
(fast-expt-iter-2 50000000 50000000) ; comes back instantly
(fast-expt-iter 3 3)
(fast-expt-iter 4 4)
(fast-expt-iter-2 4 4)
(fast-expt-iter 5 5)
(fast-expt-iter-2 5 5)
(fast-expt-iter 6 6)
(fast-expt-iter-2 6 6) ; 46656
(fast-expt-iter 10 10)
(fast-expt-iter-2 10 10)
(fast-expt-iter-2 25 25)
  (fast-expt-iter 3 3 0 1) ;;  3^3 * 1 = 27 ;; check
  (fast-expt-iter 3 2 0 3) ;;  3^2 * 3 = 27 ;; check
  (fast-expt-iter 3 1 1 3) ;;  (3^1)^2 * 3 = 27 ;; check
  (square-n-times-then-multiply 3 1 3) ;; 3^2 * 3 = 27 ;; check
  (square-n-times-then-multiply 3 0 27)

(fast-expt-iter-2 3 3)



;; note: (b^(n/2))^2 = (b^2)^(n/2)

;; 
(defn fast-expt-iter-2
  ([b n]
     (fast-expt-iter-2 b n 1))
  ([b n final-multiply]     
     (cond (= n 0) 
           final-multiply
           
           (even? n)
           (recur (*' b b) (/ n 2) final-multiply)

           :else 
           (recur b (dec n) (*' b final-multiply)))))

(fast-expt-iter-2 1000 1000)
  

(b^2)^(n/2)

b^n = (b^2)^(n/2)


(defn check-multiply [mult]
  (check-function mult-facts mult)

  (assert (= 0
             (mult 0 0)))
  (assert (= 25
             (mult 5 5)))
  (assert (= 45
             (mult 9 5)))
  true)

(check-multiply *)
(check-multiply my*-slow)

(defn my*-slow [a b]
  (if (= b 0)
    0
    (+ a (my*-slow a (dec b)))))

(def mult-facts
  '(0 (0 0)
      25 (5 5)
      45 (9 5)))

(def slightly-wrong-mult-facts
  '(0 (0 0)
      25 (5 5)
      46 (9 5)))

mult-facts

(defn check-function [facts function]
  (if (empty? facts)
    true
    (let [expected-result (first facts)
          args (second facts)
          rest (rest (rest facts))]
      (do
        (if (not (= expected-result (apply function args)))
          (throw (new AssertionError (str "Assert failed: " expected-result " != " 
                                          (pr-str (cons function args)))))
          (recur rest function))))))



'(* my*-slow)

(check-function slightly-wrong-mult-facts *)
(check-function mult-facts *)

(defn check-mult-functions [functions]
  (every? identity (map #(check-function mult-facts %) functions)))

(check-mult-functions (list * my*-slow)) ;; true

(def double-facts
  '(0 (0)
      2 (1)
      66 (33)))

(defn double [n] (+ n n))

(check-function double-facts double) ;; true

(defn halve [n] (/ n 2))

(def halve-facts
  '(13 (26)
      7 (14)
      19 (38)))

(check-function halve-facts halve)

;; identities:
;;;; a*b = (a/2)*(b*2)
;;;; a*b = b + (a-1)*b
(defn my*-faster [a b]
  (cond (= a 0) 
        0

        (even? a) 
        (recur (halve a) (+ b b))

        :else
        (+ b (my*-faster (dec a) b))))
    ;; (+ b b)

(check-mult-functions (list * my*-slow my*-faster)) ;; true

(defn my*-fastest 
  ([a b] (my*-fastest a b 0))
  ([a b sum-so-far]
     (cond (= a 0 sum-so-far) 
           sum-so-far
           
           (even? a) 
           (recur (halve a) (+ b b) sum-so-far)
           
           :else
           (recur (dec a) b (+ b sum-so-far)))))


(check-mult-functions (list * my*-slow my*-faster my*-fastest)) ;; true

;; Facts:
;;   T(p, q) := (a b) => (bq + aq + ap, bp + aq)

;; Lemma:
;;   T(0, 1, a, b)^n = Fib(n)
;;     (already proven)

;;  T(p, q, a, b)^2 := 
;;      T(bq + aq + ap, bp + aq)
;;      ;; a = bq + aq + ap
;;      ;; b = bp + aq
;;      (bp + aq)q + (bq + aq + ap)q + (bq + aq + ap)p,
;;      (bp + aq)p + (bq + aq + ap)q
;;      =
;;      bpq + aqq + bqq + aqq + apq + bpq + apq + app,
;;      bpp + apq + bqq + aqq + apq
;;      =
;;      2bpq + 2aqq + bqq + 2*apq + app,
;;      bpp + 2apq + bqq + aqq
;;      = 
;;      2bpq + 2aqq + bqq + 2*apq + app,
;;      b(pp + qq) + a(2pq + qq)
;;      = 
;;      (app + aqq) + (2bpq + aqq + bqq + 2*apq)
;;      b(pp + qq) + a(2pq + qq)
;;      = 
;;      a(pp + qq) + b(2pq + qq) + a(2pq + qq)
;;      b(pp + qq) + a(2pq + qq)
;;      =
;;      ap' + bq' + aq'
;;      bp' + aq'
;;      =
;;      bq' + aq' + ap' 
;;      bp' + aq'

;; This is equivalent to T(p', q'), where
;;      T(p', q') = (bq' + aq' + ap',
;;                   bp' + aq')
;;      p' = (pp + qq)
;;      q' = (2pq + qq)

(def fib-facts
  '(1 (1)
      1 (2)
      2 (3)
      3 (4)
      5 (5)
      8 (6)
      13 (7)
      28657 (23)))

(fib-slow 23)
(fib-fastest 23)

(defn check-fib-functions [functions]
  (every? identity (map #(check-function fib-facts %) functions)))


(map #(check-function fib-facts %) (list fib-slow fib-faster fib-fastest))

(check-fib-functions (list fib-slow fib-faster fib-fastest))

(check-function fib-facts fib-fastest)

(fib-fastest 3)

(defn fib-fastest 
  ([n] (fib-fastest 1 0 0 1 n))
  ([a b p q count]
     (cond (= count 0) 
           b

           (even? count)
           (recur a 
                  b
                  (+ (* p p) (* q q))
                  (+ (* 2 p q) (* q q))
                  (/ count 2))

           :else
           (recur (+ (* b q) (* a q) (* a p))
                  (+ (* b p) (* a q))
                  p
                  q
                  (- count 1)))))


  





;; playing with lazy sequences

(defn numbers
  ([] (numbers 1))
  ([n]
     (cons n (lazy-seq (numbers (inc n))))))

(take 4 (numbers))






;;; Project 1, 6.001, Spring 2005

;;; idea is to simulate a baseball robot

;; imagine hitting a ball with an initial velocity of v 
;; at an angle alpha from the horizontal, at a height h
;; we would like to know how far the ball travels.

;; as a first step, we can just model this with simple physics
;; so the equations of motion for the ball have a vertical and a 
;; horizontal component

;; the vertical component is governed by
;; y(t) = v sin alpha t + h - 1/2 g t^2 
;; where g is the gravitational constant of 9.8 m/s^2

;; the horizontal component is governed by
;; x(t) = v cos alpha t
;; assuming it starts at the origin

;; First, we want to know when the ball hits the ground
;; this is governed by the quadratic equation, so we just need to know when 
;; y(t)=0 (i.e. for what t_impact is y(t_impact)= 0).
;; note that there are two solutions, only one makes sense physically

(defn square [x] (* x x))


;; these are constants that will be useful to us
(def gravity 9.8)  ;; in m/s
(def pi 3.14159)

;; Problem 1

;; a - initial accelleration
;; v - initial velocity
;; u - initial position
;; t - time
;; position(t) = (a/2)*t*t + v*t + u
(defn position [a v u t]
  (+
   (* (/ a 2.0) t t)
   (* v t)
   u))

;; you need to complete this procedure, then show some test cases

(position 0 0 0 0) ;; should be 0
(position 0 0 20 0) ;; should be 20
(position 0 5 10 10) ;; should be 60
(position 2 2 2 1) ;; should be 5
(position 5 5 5 5) ;; should be ;; 92.5
(position 0 100 0 1) ;; should be 100
(position 0 100 0 2) ;; should be 200

;; Problem 2

;; az^2 + bz + c = 0 ;; solve for z
;; by quadratic formula: https://en.wikipedia.org/wiki/Quadratic_formula
;; z = (-b +/- sqrt(b^2 -4ac))/(2a)
(defn root [operator a b c]
  (let [square (- (* b b)
                  (* 4 a c))]
    (if (< square 0)
      nil
      (/ (operator (- b)
                   (Math/sqrt square))
         (+ a a)))))

(defn root1 [a b c]
  (root + a b c))
       
(defn root2 [a b c]
  (root - a b c))

;; ax^2 + bx + c = 0
;; x^2 + x - 2 = 0
(root1 1 1 -2) ;; should return 1.0 = 1.0^2 + 1.0 - 2.0 = 0
(root2 1 1 -2) ;; should return -2 = -2^2 + -2 - 2 = 0

;; 5x^2 + 3x + 6 = 0
(root1 5 3 6) ;; should return nil
;; should be: (sqrt(-3)-1)/2

;;x^2 + x + 1 = 0

;;1 + 1 + -2

(root2 5 3 6) ;; also nil

;; complete these procedures and show some test cases

;; Problem 3

;; elevation - gravity*t*t + vertical_velocity*t = 0
;; -gravity t^2 + vertical_velicty*t + elevation = 0

-gravity*t*t + vertical_velocity*t + elevation = 0

;; position = (a/2)*t^2 + v*t + elevation
;; 0 = (-9.8/2)*t^2 + initial-velocity*t + elevation
;; (root2 (/ (- gravity) 2) vertical-velocity elevation

;; elevation - gravity*t*t + vertical_velocity*t = 0
;; 

(defn time-to-impact [vertical-velocity elevation]
  (root2 (/ (- gravity) 2) vertical-velocity elevation))

(time-to-impact 200 20) ;; should be positive


;; -gravity*t^2 + vertical_velocity*t + elevation = 0
;; -9.8*t^2 + 200*t + 20 = 0


;; Note that if we want to know when the ball drops to a particular height r 
;; (for receiver), we have

elevation - gravity*t*t + vertical_velocity*t = target-elevation

- gravity*t*t + vertical_velocity*t + elevation-target_elevation  = 0

(defn time-to-height [vertical-velocity elevation target-elevation]
  (root2 (- gravity) vertical-velocity (- elevation target-elevation)))

(time-to-height 200 10 20)


;; Problem 4

;; once we can solve for t_impact, we can use it to figure out how far the ball went

;; conversion procedure
(defn degree2radian [deg]
  (/ (*  deg pi) 180.))

;; velocity * time in air
(defn travel-distance-simple [elevation velocity angle]
  (let [angle-radians (degree2radian angle)
        horizontal-velocity (* velocity (Math/cos angle-radians))
        vertical-velocity (* velocity (Math/sin angle-radians))
        time-in-air (time-to-impact vertical-velocity elevation)]
    (do
;      (println "angle-radians" angle-radians) ;; correct
;      (println "horizontal-velocity" horizontal-velocity)
;      (println "vertical-velocity" vertical-velocity)
;      (println "time-in-air" time-in-air)
      (* horizontal-velocity time-in-air))))

;; let's try this out for some example values.  Note that we are going to 
;; do everything in metric units, but for quaint reasons it is easier to think
;; about things in English units, so we will need some conversions.

(defn meters-to-feet [m]
  (/ (* m 39.6) 12))

(defn feet-to-meters [f]
  (/ (* f 12) 39.6))

(defn hours-to-seconds [h]
  (* h 3600))

(defn seconds-to-hours [s]
  (/ s 3600))

;; what is time to impact for a ball hit at a height of 1 meter
;; with a velocity of 45 m/s (which is about 100 miles/hour)
;; at an angle of 0 (straight horizontal)

(time-to-impact 0 1) ;; 0.4517539514526256
(travel-distance-simple 1 45 0) ;; 20.32892781536815
(meters-to-feet (travel-distance-simple 1 45 0)) ;; 67.0854617907149

;; at an angle of (/ pi 2) radians or 90 degrees (straight vertical)

(time-to-impact 45 1) ;; 9.20584217798685
(travel-distance-simple 1 45 90) ;; 0
(meters-to-feet (travel-distance-simple 1 45 90)) ;; 0

;; at an angle of (/ pi 4) radians or 45 degrees

(time-to-impact 22.5 1) ;; 4.635859132019005
(travel-distance-simple 1 45 45) ;; 207.6278611514906
(meters-to-feet (travel-distance-simple 1 45 45)) ;; 685.171941799919

;; what is the distance traveled in each case?
;; record both in meters and in feet


;; Problem 5

;; these sound pretty impressive, but we need to look at it more carefully

;; first, though, suppose we want to find the angle that gives the best
;; distance
;; assume that angle is between 0 and (/ pi 2) radians or between 0 and 90
;; degrees

(def alpha-increment 0.1)

alpha-increment

(defn all-angles [start finish]
  (take-while 
   #(<= % finish)
   (iterate #(+ % alpha-increment) start)))

(all-angles 0 90)

(defn find-best-angle [velocity elevation]
  ;; ideas: start at elevation 0, start increasing until answer starts decreasing.
  ;; ideas: create list of all elevations broken up by increment, map to each, take max
  (let
      [all-angles-in-radians (all-angles 0 90)
       distance-from-angle (partial travel-distance-simple elevation velocity)
       angles-and-distances (map #(list % (distance-from-angle %))
                                 all-angles-in-radians)]
    (last (sort-by #(second %1) angles-and-distances))))

(find-best-angle 50 2)




;; find best angle
;; try for other velocities
;; try for other heights

;; Problem 6

;; problem is that we are not accounting for drag on the ball (or on spin 
;; or other effects, but let's just stick with drag)
;;
;; Newton's equations basically say that ma = F, and here F is really two 
;; forces.  One is the effect of gravity, which is captured by mg.  The
;; second is due to drag, so we really have
;;
;; a = drag/m + gravity
;;
;; drag is captured by 1/2 C rho A vel^2, where
;; C is the drag coefficient (which is about 0.5 for baseball sized spheres)
;; rho is the density of air (which is about 1.25 kg/m^3 at sea level 
;; with moderate humidity, but is about 1.06 in Denver)
;; A is the surface area of the cross section of object, which is pi D^2/4 
;; where D is the diameter of the ball (which is about 0.074m for a baseball)
;; thus drag varies by the square of the velocity, with a scaling factor 
;; that can be computed

;; We would like to again compute distance , but taking into account 
;; drag.
;; Basically we can rework the equations to get four coupled linear 
;; differential equations
;; let u be the x component of velocity, and v be the y component of velocity
;; let x and y denote the two components of position (we are ignoring the 
;; third dimension and are assuming no spin so that a ball travels in a plane)
;; the equations are
;;
;; dx/dt = u
;; dy/dt = v
;; du/dt = -(drag_x/m + g_x)
;; dv/dt = -(drag_y/m + g_y)
;; we have g_x = - and g_y = - gravity
;; to get the components of the drag force, we need some trig.
;; let speeed = (u^2+v^2)^(1/2), then
;; drag_x = - drag * u /speed
;; drag_y = - drag * v /speed
;; where drag = beta speed^2
;; and beta = 1/2 C rho pi D^2/4
;; note that we are taking direction into account here

;; we need the mass of a baseball -- which is about .15 kg.

;; so now we just need to write a procedure that performs a simple integration
;; of these equations -- there are more sophisticated methods but a simple one 
;; is just to step along by some step size in t and add up the values

;; dx = u dt
;; dy = v dt
;; du = - 1/m speed beta u dt
;; dv = - (1/m speed beta v + g) dt

;; initial conditions
;; u_0 = V cos alpha
;; v_0 = V sin alpha
;; y_0 = h
;; x_0 = 0

;; we want to start with these initial conditions, then take a step of size dt
;; (which could be say 0.1) and compute new values for each of these parameters
;; when y reaches the desired point (<= 0) we stop, and return the distance (x)

(def drag-coeff 0.5)
(def density-for-boston 1.25)  ; kg/m^3
(def mass 0.145)  ; kg
(def diameter 0.074)  ; m
(defn beta [density] (* 0.5 drag-coeff density (* 3.14159 0.25 (square diameter))))

(defn calc-dv
  [m u0 v0 beta g dt]
  (* -1
     (+ (* (/ 1 m)
           (Math/sqrt (+ (* u0 u0)
                         (* v0 v0)))
           v0
           beta)
        g)
     dt))

(defn calc-du
  [m u0 v0 beta g dt]
  (* (/ -1 m)
     beta
     (Math/sqrt (+ (* u0 u0)
                   (* v0 v0)))
     u0
     dt))

(defn integrate-distance [x0 y0 u0 v0 dt g m beta]
  (let
      [dv (calc-dv m u0 v0 beta g dt)
       du (calc-du m u0 v0 beta g dt)
       v (+ v0 dv)
       u (+ u0 du)
       dx (* u dt)
       dy (* v dt)
       x (+ x0 dx)
       y (+ y0 dy)]
    (if (< y 0) 
      x0
      (recur x y u v dt g m beta))))

(defn travel-distance [batters-height velocity angle-in-degrees density-for-area]
  (let [angle-in-radians (degree2radian angle-in-degrees)
        initial-horizontal-velocity (* velocity (Math/cos angle-in-radians))
        initial-vertical-velocity (* velocity (Math/sin angle-in-radians))
        x0 0
        y0 batters-height
        u0 initial-horizontal-velocity
        v0 initial-vertical-velocity
        dt 0.01
        m-in-kg 0.15
        beta-for-area (beta density-for-area)]
    (integrate-distance x0 y0 u0 v0 dt gravity m-in-kg beta-for-area)))

(meters-to-feet (travel-distance 2 45 45)) ;; 309 feet
(meters-to-feet (travel-distance 2 40 45)) ;; 273 feet
(meters-to-feet (travel-distance 2 35 45)) ;; 235 feet


;; assume > 300 feet = outfield fence
;; for what variation in angles will the ball land over the fence?
(defn lands-over-fence [density-for-area angle-in-degrees]
  ;; assume fence is same height as batter
  (let [outfield-length-in-feet 300]
    (> (meters-to-feet (travel-distance 0 45 angle-in-degrees density-for-area))
       outfield-length-in-feet)))

(defn angles-that-go-over-fence [density-for-area]
  (let [angles (all-angles 0 90)]
    (filter (partial lands-over-fence density-for-area) angles)))

(def density-in-boston 1.25)  ; kg/m^3 - in Denver

(angles-that-go-over-fence density-in-boston) ;; (31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47)

(def density-in-denver 1.06)  ; kg/m^3 - in Denver

(angles-that-go-over-fence density-in-denver) ;; (25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55)


(defn atom? [x]
  (not (seq x)))

;; RUN SOME TEST CASES

;; what about Denver?

;; Problem 7
 
;; now let's turn this around.  Suppose we want to throw the ball.  The same
;; equations basically hold, except now we would like to know what angle to 
;; use, given a velocity, in order to reach a given height (receiver) at a 
;; given distance


;; a cather trying to throw someone out at second has to get it roughly 36 m
;; (or 120 ft) how quickly does the ball get there, if he throws at 55m/s,
;;  at 45m/s, at 35m/s?

;; dx = u dt
;; dy = v dt
;; du = - 1/m speed beta u dt
;; dv = - (1/m speed beta v + g) dt

;;;; initial conditions
;;;; u_0 = V cos alpha
;;;; v_0 = V sin alpha
;;;; y_0 = h
;;;; x_0 = 0

;; The above will find the distance travelled given the vertical and horizontal velocity.

;; Now need to find the time until we reach a specified distance


(defn integrate-time-and-distance [x0 y0 u0 v0 dt g m beta time-so-far]
  (let
      [dv (calc-dv m u0 v0 beta g dt)
       du (calc-du m u0 v0 beta g dt)
       v (+ v0 dv)
       u (+ u0 du)
       dx (* u dt)
       dy (* v dt)
       x (+ x0 dx)
       y (+ y0 dy)
       new-time (+ time-so-far dt)]
    (if (< y 0)
      (list time-so-far x0)
      (recur x y u v dt g m beta new-time))))

(defn travel-time-and-distance 
  ([velocity angle-in-degrees density-for-area]
     (travel-time-and-distance velocity angle-in-degrees density-for-area 0))
  ([velocity angle-in-degrees density-for-area initial-height]
     (let [angle-in-radians (degree2radian angle-in-degrees)
           initial-horizontal-velocity (* velocity (Math/cos angle-in-radians))
           initial-vertical-velocity (* velocity (Math/sin angle-in-radians))
           x0 0
           y0 0
           u0 initial-horizontal-velocity
           v0 initial-vertical-velocity
           dt 0.01
           m-in-kg 0.15
           beta-for-area (beta density-for-area)]
       (integrate-time-and-distance x0 y0 u0 v0 dt gravity m-in-kg beta-for-area 0))))

(travel-time-and-distance 45 78 density-in-boston)

(def max-error 5)

(defn time-for-distance [distance velocity angle-in-degrees density-for-area]
  (let [[time travelled-distance] (travel-time-and-distance velocity angle-in-degrees density-for-area)
        distance-error (abs (- distance travelled-distance))]
    (if (< distance-error max-error) time
        0)))

(defn is-valid-angle? [distance velocity density-for-area angle-in-degrees]
  (not (= 0 (time-for-distance distance velocity angle-in-degrees density-for-area))))


(defn valid-angles [distance velocity density-for-area]
  (let [angles (all-angles -90 90)]
    (filter (partial is-valid-angle? distance velocity density-for-area)
            angles)))

(valid-angles 36 45 density-in-boston)

(defn time-to-target [distance velocity density-for-area]
  (let [angles (valid-angles distance velocity density-in-boston)]
    (ffirst
     (map #(travel-time-and-distance velocity % density-for-area) angles))))
;  )

(time-to-target 36 45 density-in-boston) ;; takes 0.8 seconds to reach second base
(time-to-target 36 35 density-in-boston) ;; takes 1.0 seconds to reach second base
(time-to-target 36 55 density-in-boston) ;; takes 0.67 seconds to reach second base

(time-to-target 18 40 density-in-boston) ;; 0.35 seconds from pitcher's mound to catcher
(time-to-target 36 40 density-in-boston) ;; 0.90 seconds from pitcher's mound to catcher
(- 3 (+ 0.35 0.90)) ;; 1.75 seconds to catch and release ball to throw out stealing runner


(time-to-target 30 45 density-in-boston) ;; 0.63 s
(time-to-target 60 45 density-in-boston) ;; 1.63 s
(time-to-target 80 45 density-in-boston) ;; 2.5 s

(time-to-target 30 55 density-in-boston) ;; 0.53 s
(time-to-target 60 55 density-in-boston) ;; 1.33 s
(time-to-target 80 55 density-in-boston) ;; 2.02 s

(time-to-target 30 35 density-in-boston) ;; 0.8 s
(time-to-target 60 35 density-in-boston) ;; 2.2 s
(time-to-target 80 35 density-in-boston) ;; can't reach at all



;; try out some times for distances (30, 60, 90 m) or (100, 200, 300 ft) 
;; using 45m/s

;; Problem 8

(time-to-target 90 35 density-in-boston)

(defn distance-with-bounces [velocity angle-in-degrees density-for-area initial-height num-bounces]
  (if (< num-bounces 0)
    0
    (+
     (travel-time-and-distance velocity angle-in-degrees density-for-area initial-height)
     (distance-with-bounces (/ velocity 2) angle-in-degrees density-for-area 0 (dec num-bounces))))) ;; next bounce

(travel-time-and-distance )
(distance-with-bounces 35 45 density-in-boston 2 0)

;; Problem 9


;; Section 1.2.5 Greatest Common Divisors

;; GCD is largest integeger that divides both A and B with no remainder.

;; Euclid's algorithm for GCD:
;;
;; GCD(a, 0) = a
;; GCD(a, b) = GCD(b, r) where r = a mod b

;; examples:

;; GCD(206, 40)
;; = GCD(40, 6) ;; 206 = 40*5 + 6
;; = GCD(6, 4) ;; 40 = 6*6 + 4
;; = GCD(4, 2) ;; 6 = 4 + 2
;; = GCD(2, 0) ;;

;; Lame's theorem: If Euclid's algorithm requires k steps to compute
;; the GCD of some pairs, then the smaller number in the pair must be
;; greater than or equal to the kth Fibbonnaci number.

;; ... thus Euclid's algorithm is Θ(log n)

;; Exerise 1.20

;; (gcd 226 40)
;; (gcd 40 (remainder 226 40) ) ;; REMAINDER: +1
;; (gcd (remainder 226 40) ;; 6 ;; REMAINDER: +2
;;      (remainder 40 (remainder 226 40)) ;; 4
;; (gcd (remainder 40 (remainder 226 40)) ;; 4 ;; REMAINDER: +4b
;;      (remainder (remainder 226 40) (remainder 40 (remainder 226 40))) ;; 2
;; (gcd (remainder (remainder 226 40) (remainder 40 (remainder 226 40))) ;; 2 ;; REMAINDER: +6
;;      (remainder (remainder 40 (remainder 226 40)) ;; 0
;;                 (remainder (remainder 226 40) (remainder 40 (remainder 226 40)))
;; REMAINDER: +11

;;24 times



) ;; remainder evaluated


;; Normal order would run remainer a ton of times.  Probably an
;; exponential amount.  Applicative order collapses it and runs it
;; only O(log n) times.

((juxt identity clojure.string/upper-case clojure.string/lower-case) "hi")

(defn empty-board
  "Creates a rectangular empty board of the specified width and height."
  [w h]
  (vec (repeat w (vec (repeat h nil)))))

(defn populate
  "Turns :on each of the cells specified as [y, x] coordinates."
  [board living-cells]
  (reduce (fn [board coordinates]
            (assoc-in board coordinates :on))
          board
          living-cells))

(def glider (populate (empty-board 6 6) #{[2 0] [2 1] [2 2] [1 2] [0 1]}))


(clojure.pprint/pprint (assoc-in (empty-board 6 6) [2 0] :on))

(clojure.pprint/pprint glider)

glider


(defn neighbors
  [[x y]]
  (for [dx [-1 0 1] dy [-1 0 1] :when (not= 0 dx dy)]
    [(+ dx x) (+ dy y)]))

(defn count-neighbors
  [board loc]
  (count (filter #(get-in board %) (neighbors loc))))

(defn indexed-step
  "Yields the next state of the board, using indices to determine neighbors,
  liveness, etc."
  [board]
  (let [w (count board)
        h (count (first board))]
    (loop [new-board board
           x 0
           y 0]
      (cond
       (>= x w) new-board
       (>= y h) (recur new-board (inc x) 0)
       :else
       (let [new-liveness
             (case (count-neighbors board [x y])
               2 (get-in board [x y])
               3 :on
               nil)]
         (recur (assoc-in new-board [x y] new-liveness) x (inc y)))))))

glider
(-> (iterate indexed-step glider) (nth 8) clojure.pprint/pprint)

(defn indexed-step2
  [board]
  (let [w (count board)
        h (count (first board))]
    (reduce
     (fn [new-board x]
       (reduce
        (fn [new-board y]
          (let [new-liveness
                (case (count-neighbors board [x y])
                  2 (get-in board [x y])
                  3 :on
                  nil)]
            (assoc-in new-board [x y] new-liveness)))
        new-board (range h)))
     board (range w))))

(-> (iterate indexed-step2 glider) (nth 8) clojure.pprint/pprint)

(defn indexed-step3
  [board]
  (let [w (count board)
        h (count (first board))]
    (reduce
     (fn [new-board [x y]]
       (let [new-liveness
             (case (count-neighbors board [x y])
               2 (get-in board [x y])
               3 :on
               nil)]
         (assoc-in new-board [x y] new-liveness)))
     board (for [x (range h) y (range w)] [x y]))))

(-> (iterate indexed-step3 glider) (nth 8) clojure.pprint/pprint)

;; (defn window
;;   "Returns a lazy sequence of 3-item windows centered around each 
;;   item of coll"
;;   [coll]
;;   (partition 3 1 (concat [nil] coll) [nil]))

;; (defn cell-block
;;   "Creates sequences of 3x3 windows from a triple of 3 sequences"
;;   [[left mid right]]
;;   (window (map vector
;;                (or  left (repeat nil)) mid (or right (repeat nil)))))

(defn window
  "Returns a lazy sequence of 3-item windows cenetered
   around each item of coll, padded as necessary with
   pad or nil."
  ([coll] (window nil coll))
  ([pad coll]
     (partition 3 1 (concat [pad] coll [pad]))))

(defn cell-block
  "Creates sequences of 3x3 windows from a triple of 3 sequences"
  [[left mid right]]
  (window (map vector left mid right)))

(defn liveness
  "Returns the liveness (nil or :on) of the center cell for the next step"
  [block]
  (let [[_ [_ center _] _] block]
    (case (- (count (filter #{:on} (apply concat block)))
             (if (= :on center) 1 0))
      2 center
      3 :on
      nil)))

(defn- step-row
  "Yields the next state of the center row."
  [rows-triple]
  (vec (map liveness (cell-block rows-triple))))

(defn index-free-step
  "Yields the next state of the board."
  [board]
  (vec (map step-row (window (repeat nil) board))))

(step-row (nth (window (repeat nil) glider) 3))

(= (nth (iterate indexed-step glider) 8)
   (nth (iterate index-free-step glider) 8))

(defn step
  "Yields the next state of the world"
  [cells]
  (set (for [[loc n] (frequencies (mapcat neighbors cells))
             :when (or (= n 3) (and (= n 2) (cells loc)))]
         loc)))

(frequencies (mapcat neighbors #{[2 0] [2 1] [2 2] [1 2] [0 1]}))

(->> (iterate step #{[2 0] [2 1] [2 2] [1 2] [0 1]})
     (drop 8)
     first
     (populate (empty-board 6 6))
     clojure.pprint/pprint)

(defn stepper
  "Returns a step function for Life-like cell automata.
   neighbors takes a location and return a sequential collection
   of locations.  survive? and birth? are predicates on the number
   of living neighbors."
  [neighbors birth? survive?]
  (fn [cells]
    (set (for [[loc n] (frequencies (mapcat neighbors cells))
               :when (if (cells loc) (survive? n) (birth? n))]
           loc)))
)
