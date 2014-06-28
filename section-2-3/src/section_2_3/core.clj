(ns section-2-3.core)

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))

(defn memq [item x]
  "If item not contained in list, returns false.  Otherwise returns
  sublist of list beginning with first occurence of symbol"
  (cond (empty? x) false
        (= item (first x)) x
        :else (memq item (rest x))))

(memq 'apple '(pear banana prune)) ;= false

(memq 'apple '(x (apple sauce) y apple pear)) ;= (apple pear)

;; Exercise 2.53

(list 'a 'b 'c) ;= (a b c)

(list (list 'george)) ;= ((george))

(rest '((x1 x2) (y1 y2))) ;=  ((y1 y2))

(second '((x1 x2) (y1 y2))) ;= (y1 y2)

(list? (first '(a short list))) ;= false

(memq 'red '((red shoes) (blue socks))) ;= false

(memq 'red '(red shoes blue socks)) ;= (red shoes blue socks)

;; Exercise 2.54

(defn equal? [coll1 coll2]
  (cond
   (empty? coll1) (empty? coll2)
   (empty? coll2) false
   (not (= (first coll1) (first coll2))) false
   :else (equal? (rest coll1) (rest coll2))))

(equal? '(this is a list) '(this is a list)) ;= true
(equal? '(this is a list) '(this (is a) list)) ;= false

;; Exercise 2.55

''abracadabra == '(quote abracadabra)

;; Section 2.3.2

; d(ax^2 + bx + c, x) = 2ax + b

; dc/dx = 0 # for c a constant

(defn variable? [x] (symbol? x))

(defn same-variable? [v1 v2]
  (and (variable? v1) (variable? v2) (= v1 v2)))

(defn make-sum [a1 a2] (list '+ a1 a2))

(defn make-product [m1 m2] (list '* m1 m2))

(defn sum? [x]
  (and (seq? x) (= (first x) '+)))

(defn addend [s] (second s))

(defn augend [s] (nth s 2))

(defn product? [x] (and (seq? x) (= (first x) '*)))

(defn multiplier [p] (second p))

(defn multiplicand [p] (nth p 2))

(defn deriv [exp var]
  (cond (number? exp) 0
        (variable? exp) (if (same-variable? exp var) 1 0)
        (sum? exp) (make-sum (deriv (addend exp) var)
                             (deriv (augend exp) var))
        (product? exp) (make-sum (make-product (multiplier exp)
                                               (deriv (multiplicand exp) var))
                                 (make-product (deriv (multiplier exp) var)
                                               (multiplicand exp)))
        :else (throw (IllegalArgumentException.
                      "unknown expression type -- DERIV" exp))))

(deriv '(+ x 3) 'x) ;= (+ 1 0)

(deriv '(* x y) 'x) ;= (+ (* x 0) (* 1 y))

(deriv '(* (* x y) (+ x 3)) 'x)
;= user=> (+ (* (* x y) (+ 1 0)) (* (+ (* x 0) (* 1 y)) (+ x 3)))

(defn make-sum [a1 a2]
  (cond (=number? a1 0) a2
        (=number? a2 0) a1
        (and (number? a1) (number? a2)) (+ a1 a2)
        :else (list '+ a1 a2)))

(deriv '(+ x 3) 'x) ;= 1

(deriv '(* x y) 'x) ;= (+ (* x 0) (* 1 y))

(deriv '(* (* x y) (+ x 3)) 'x)
;= user=> (+ (* (* x y) 1) (* (+ (* x 0) (* 1 y)) (+ x 3)))

(defn =number? [exp num]
  (and (number? exp) (= exp num)))

(defn make-product [m1 m2]
  (cond (or (=number? m1 0) (=number? m2 0)) 0
        (=number? m1 1) m2
        (=number? m2 1) m1
        (and (number? m1) (number? m2)) (* m1 m2)
        :else (list '* m1 m2)))


(deriv '(+ x 3) 'x) ;= 1

(deriv '(* x y) 'x) ;= y

(deriv '(* (* x y) (+ x 3)) 'x) ;= (+ (* x y) (* y (+ x 3)))

;; Exercise 2.56

(defn exponentiation? [x] (and (seq? x) (= (first x) '**)))

(defn exponent [e] (nth e 2))

(defn base [e] (nth e 1))

(defn make-exponentiation [base exp]
  (cond (=number? exp 0) 1
        (=number? exp 1) base
        :else (list '** base exp)))

(defn deriv [exp var]
  (cond (number? exp) 0
        (variable? exp) (if (same-variable? exp var) 1 0)
        (sum? exp) (make-sum (deriv (addend exp) var)
                             (deriv (augend exp) var))
        (product? exp) (make-sum (make-product (multiplier exp)
                                               (deriv (multiplicand exp) var))
                                 (make-product (deriv (multiplier exp) var)
                                               (multiplicand exp)))
        (exponentiation? exp)
          (make-product
            (make-product (exponent exp)
                          (make-exponentiation (base exp)
                                               (make-sum (exponent exp),
                                                         -1)))
            (deriv (base exp) var))
        :else (throw (IllegalArgumentException.
                      "unknown expression type -- DERIV" exp))))

(deriv '(** x 2) 'x) ;= (* 2 x)

;; Exercise 2.57

(defn deriv [exp var]
  (cond (number? exp) 0
        (variable? exp) (if (same-variable? exp var) 1 0)
        (sum? exp) (make-sum (deriv (addend exp) var)
                             (deriv (augend exp) var))
        (product? exp) (make-sum (make-product (multiplier exp)
                                               (deriv (multiplicand exp) var))
                                 (make-product (deriv (multiplier exp) var)
                                               (multiplicand exp)))
        (exponentiation? exp)
          (make-product
            (make-product (exponent exp)
                          (make-exponentiation (base exp)
                                               (make-sum (exponent exp),
                                                         -1)))
            (deriv (base exp) var))
        :else (throw (IllegalArgumentException.
                      (str "unknown expression type -- DERIV" exp)))))

(defn augend [s]
  (cond (empty? (rest s)) 0
        (empty? (rest (rest (rest s)))) (nth s 2)
        :else (cons '+ (rest (rest s)))))

(augend '(+ 2 3)) ;= (+ 3 4)
(augend '(+ 2 3 4)) ;= (+ 3 4)

(defn multiplicand [s]
  (cond (empty? (rest s)) 0
        (empty? (rest (rest (rest s)))) (nth s 2)
        :else (cons '* (rest (rest s)))))

(deriv '(* x y (+ x 3)) 'x) ;= (+ (* x y) (* y (+ x 3)))

(product? '(* y (+ x 3))) ;= true

(multiplicand '(* x y (+ x 3))) ;= (* y (+ x 3))

;; Exercise 2.58

(defn sum? [x]
  (and (seq? x) (= (second x) '+)))

(defn third [coll] (nth coll 2))

(defn addend [s] (first s))

(defn augend [s] (third s))

(defn product? [x] (and (seq? x) (= (second x) '*)))

(defn multiplier [p] (first p))

(defn multiplicand [p] (third p))

(defn exponentiation? [x] (and (seq? x) (= (second x) '**)))

(defn exponent [e] (third e))

(defn base [e] (first e))

(defn make-exponentiation [base exp]
  (cond (=number? exp 0) 1
        (=number? exp 1) base
        :else (list base '** exp)))

(defn make-product [m1 m2]
  (cond (or (=number? m1 0) (=number? m2 0)) 0
        (=number? m1 1) m2
        (=number? m2 1) m1
        (and (number? m1) (number? m2)) (* m1 m2)
        :else (list m1 '* m2)))

(defn make-sum [a1 a2]
  (cond (=number? a1 0) a2
        (=number? a2 0) a1
        (and (number? a1) (number? a2)) (+ a1 a2)
        :else (list a1 '+ a2)))



(deriv '(x + (3 * (x + (y + 2)))), 'x) ;= 4

; b

(defn sum? [x]
  (boolean (some #(= '+ %) x)))

(sum? '(5 + 2 * 3)) ;= true
(sum? '(5 * 2 * 3)) ;= false
(sum? '(5 * 2 + 3)) ;= true
(sum? '(5 * 2 + 3 ** 2)) ;= true
(sum? '(5 ** 2 * 3)) ;= false
(sum? '(5 ** 2)) ;= false
(sum? '(5 ** (2 + 29))) ;= false

(defn third [coll] (nth coll 2))

(defn simplify-list [l]
  (if (empty? (rest l))
    (first l)
    (seq l)))

(defn split-operation [s sym id]
  (loop [start (vector) middle (first s) end (rest s)]
    (cond (= middle sym) (list (simplify-list start) (simplify-list end))
          (empty? end) (list (seq (conj start middle)) id)
          :else (recur (conj start middle) (first end) (rest end)))))

(defn split-multiplication [s]
  (split-operation s '* 1))

(defn split-addition [s]
  (split-operation s '+ 0))

(split-addition '(5 + 2 * 3)) ;= (5 (2 * 3)
(split-addition '(5 * 2 * 3)) ;= ((5 * 2 * 3) 0)
(split-addition '(5 * 2 + 3)) ;= ((5 * 2) 3)
(split-addition '(5 * 2 + 3 ** 2)) ;= ((5 * 2) (3 ** 2))
(split-addition '(5 ** 2 * 3)) ;= ((5 ** 2 * 3) 0)
(split-addition '(5 ** 2)) ;= ((5 ** 2) 0)
(split-addition '(5 ** (2 + 29))) ;= ((5 ** (2 + 29)) 0)


(defn addend [s]
  (first (split-addition s)))

(addend '(5 + 2 * 3)) ;= 5
(addend '(5 * 2 * 3)) ;= (5 * 2 * 3)
(addend '(5 * 2 + 3)) ;= (5 * 2)
(addend '(5 * 2 + 3 ** 2)) ;= (5 * 2)
(addend '(5 ** 2 * 3)) ;= (5 ** 2 * 3)
(addend '(5 ** 2)) ;= user=> (5 ** 2)
(addend '(5 ** (2 + 29))) ;= (5 ** (2 + 29))

(defn augend [s]
  (second (split-addition s)))

(augend '(5 + 2 * 3)) ;= (2 * 3)
(augend '(5 * 2 * 3)) ;= 0
(augend '(5 * 2 + 3)) ;= 3
(augend '(5 * 2 + 3 ** 2)) ;= (3 ** 2)
(augend '(5 ** 2 * 3)) ;= 0
(augend '(5 ** 2)) ;= 0
(augend '(5 ** (2 + 29))) ;= 0

(defn product? [x]
  (boolean
    (and (not (sum? x))
         (and (seq? x) (some #(= '* %) x)))))

(product? '(5 + 2 * 3)) ;= false
(product? '(5 * 2 * 3)) ;= true
(product? '(5 * 2 + 3)) ;= false
(product? '(5 * 2 + 3 ** 2)) ;= false
(product? '(5 ** 2 * 3)) ;= true
(product? '(5 ** 2)) ;= false
(product? '(5 ** (2 + 29))) ;= false

(defn multiplier [p]
  (first (split-multiplication p)))

(multiplier '(5 + 2 * 3)) ;= wouldn't be called
(multiplier '(5 * 2 * 3)) ;= 5
(multiplier '(5 * 2 + 3)) ;= wouldn't be called
(multiplier '(5 * 2 + 3 ** 2)) ;= wouldn't be called
(multiplier '(5 ** 2 * 3)) ;= (5 ** 2)
(multiplier '(5 ** 2)) ;= (5 ** 2)
(multiplier '(5 ** (2 + 29))) ;= (5 ** (2 + 29))

(defn multiplicand [p]
  (second (split-multiplication p)))

(multiplicand '(5 + 2 * 3)) ;= wouldn't be called
(multiplicand '(5 * 2 * 3)) ;= (2 * 3)
(multiplicand '(5 * 2 + 3)) ;= wouldn't be called
(multiplicand '(5 * 2 + 3 ** 2)) ;= wouldn't be called
(multiplicand '(5 ** 2 * 3)) ;= 3
(multiplicand '(5 ** 2)) ;= 1
(multiplicand '(5 ** (2 + 29))) ;= 1

(defn exponentiation? [x]
  (and (not (sum? x))
       (not (product? x))
       (and (seq? x) (= (second x) '**))))

(exponentiation? '(5 + 2 * 3)) ;= false
(exponentiation? '(5 * 2 * 3)) ;= false
(exponentiation? '(5 * 2 + 3)) ;= false
(exponentiation? '(5 * 2 + 3 ** 2)) ;= false
(exponentiation? '(5 ** 2 * 3)) ;= false
(exponentiation? '(5 ** 2)) ;= true
(exponentiation? '(5 ** (2 + 29)) ;= true
(exponentiation? '(5 ** 2)) ;= true
(exponentiation? '(5 ** (2 + 29)) ;= true

(defn split-exponentiation [s]
  (split-operation s '** 1))

(split-exponentiation '(5 + 2 * 3)) ;= ((5 + 2 * 3) 1)
(split-exponentiation '(5 * 2 * 3)) ;= ((5 * 2 * 3) 1)
(split-exponentiation '(5 * 2 + 3)) ;= ((5 * 2 + 3) 1)
(split-exponentiation '(5 * 2 + 3 ** 2)) ;= ((5 * 2 + 3) 2)
(split-exponentiation '(5 ** 2 * 3)) ;= (5 (2 * 3))
(split-exponentiation '(5 ** 2)) ;= (5 2)
(split-exponentiation '(5 ** (2 + 29))) ;= (5 (2 + 29))

(defn exponent [e]
  (first (split-exponentiation e)))

(exponent '(5 + 2 * 3)) ;= (5 + 2 * 3)
(exponent '(5 * 2 * 3)) ;= (5 * 2 * 3)
(exponent '(5 * 2 + 3)) ;= (5 * 2 + 3)
(exponent '(5 * 2 + 3 ** 2)) ;= wouldn't be called
(exponent '(5 ** 2 * 3)) ;= wouldn't be called
(exponent '(5 ** 2)) ;= 5
(exponent '(5 ** (2 + 29))) ;= 5

(defn base [e]
  (second (split-exponentiation e)))

(base '(5 + 2 * 3)) ;= 1
(base '(5 * 2 * 3)) ;= 1
(base '(5 * 2 + 3)) ;= 1
(base '(5 * 2 + 3 ** 2)) ;= wouldn't be called
(base '(5 ** 2 * 3)) ;= (2 * 3)
(base '(5 ** 2)) ;= 2
(base '(5 ** (2 + 29))) ;= (2 + 29)

(deriv '(x + 3 * (x + y + 2 ** x)), 'x) ;= 4
(deriv '(y * x), 'x) ;= y

