(ns section-2-5-3.core-test
  (:require [clojure.test :refer :all]
            [clojure.string :as str]
            [section-2-5-3.core :refer :all]))

;; Polynomials having just one variable is called a 'univariate
;; polynomial', which is what this will be dealing with.

;; Polynomial is a sum of terms consisting of coefficient multiplying
;; a power of the "indeterminate".

;; 5x^3 + 3x * 7 ;; simple polynomial in x.

;; (y^3 + 1)*x^3 + (2y)x + 1 ;; polnomial in x whose coefficients are
;; polynomials in y.

;; Doing arithmetic in polynomials.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; general infrastructure
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn log [& args]
;  (println (pretty-format args))
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; module infrastructure
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def operations (atom {}))

(defn put-op [op-sym type-tags op-fn]
  "Add to the global list of operations based on the function
   name (op-sym), a list of type tags describing the arguments of the
   type, and the function (op-fn) which should be called if matched"
  (swap! operations assoc (list op-sym type-tags) op-fn))

(defn get-op [op-sym type-tags]
  (let [op (get @operations (list op-sym type-tags))] 
    (when-not (nil? op) ;; XXX kibit rule
      op)))

(defn type-tag [datum]
  (cond (number? datum) :clj-number
        (seq? datum) (first datum)
        :else (throw (IllegalArgumentException. (str "Bad tagged dataum -- TYPE-TAG "
                                                     datum)))))

(defn attach-tag [type-tag contents]
  (if (= type-tag :clj-number)
    contents
    (list type-tag contents)))

(defn contents [datum]
  (if (= :clj-number (type-tag datum))
    datum
    (second datum)))

(declare apply-generic-no-simplify)

(defn apply-generic [op & args]
  (drop (apply apply-generic-no-simplify op args)))

(defn types-to-str [type-tags]
  (str/join " " type-tags))

(defn cant-resolve-op [op types]
  (throw (Exception. (str "Could not find op " op 
                          " with tags " (types-to-str types)
                          ".  Valid tags would be " (keys @operations)))))

(defn lower-type [type]
  (when-let [fn (get-op :lower-type type)]
    (fn)))

(defn is-lower-type? [type-1 type-2]
  (if-let [lower-type-2 (lower-type type-2)]
    (if (= type-1 lower-type-2)
      true
      (recur type-1 lower-type-2))
    false))

(defn type-comparator [type-1 type-2]
  (if (= type-1 type-2)
    0
    (if (is-lower-type? type-1 type-2)
      -1
      1)))

(defn find-lowest-type [type-tags]
  (first (sort type-comparator type-tags)))

(defn find-index-of-type-to-raise [type-tags]
  (.indexOf type-tags (find-lowest-type type-tags)))

(declare raise)

(defn raise-one-step [args]
  (let [type-tags-vec (vec (map type-tag args))
        args-vec (vec args)
        index-to-raise (find-index-of-type-to-raise type-tags-vec)
        arg-to-raise (nth args index-to-raise nil)]
    (log "type-tags-vec: " (types-to-str type-tags-vec))
    (log "arg-to-raise: " arg-to-raise)
    (when arg-to-raise
      (when-let [raised-arg (raise arg-to-raise)]
        (log "raised-arg: " raised-arg)
        (let [ret (seq (assoc args-vec index-to-raise raised-arg))]
          (log "(raise-one-step " (str/join " " args) " is returning " (str/join " " ret))
          ret)))))

(defn apply-generic-no-simplify [op & args]
  (loop [current-args args]
    (log "Trying apply-generic for " op " with args " (apply str current-args))
    (if (nil? current-args)
      (cant-resolve-op op (map type-tag args))
      (let [type-tags (map type-tag current-args)
            proc (get-op op type-tags)]
        (if proc
          (apply proc (map contents current-args))
          (do 
            (log "Couldn't figure out an op with current args--raising one step "
                 (apply str current-args))
            (let [next-args (raise-one-step current-args)]
              (recur next-args))))))))


(defn apply-generic-or-nil [op & args]
  (let [type-tags (map type-tag args)
        proc (get-op op type-tags)]
    (if proc
      (apply proc (map contents args))
      nil)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; define generic operations
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn real-part [z]
  (apply-generic :real-part z))

(defn imag-part [z]
  (apply-generic :imag-part z))

(defn magnitude [z]
  (apply-generic :magnitude z))

(defn angle [z]
  (apply-generic :angle z))

(defn equ? [a b]
  (log "Call equ? on " a " and " b)
  (apply-generic-no-simplify :equ? a b))

(defn =zero? [num]
  (apply-generic :=zero? num))

(defn raise [num]
  (log "(raise " num ")")
  (apply-generic-or-nil :raise num))

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
  (if (not (gt (abs angle) 0.1))
    angle
    (p (sine (div angle 3.0)))))

(def pi 3.14159265359)

(defn cosine [x]
  (sine (sub (/ pi 2) x)))

(def tolerance 0.00001)
(defn average [a b]
  (div (add a b) 2))
(defn average-damp [f]
  #(average % (f %)))

(defn fixed-point [f first-guess]
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn install-polynomial-package []
 ;; internal procedures
;;          ...
          ;; representation of poly
          
  (letfn [(make-poly [variable term-list] (cons variable term-list))
          (variable [p] (first p))
          (term-list [p] (rest p))
          (variable? [x] (keyword? x))
          (same-variable? [v1 v2] (and (variable? v1) (variable? v2) (= v1 v2)))
          (adjoin-term [term term-list] (if (=zero? (coeff term))
                                          term-list
                                          (cons term term-list)))
          (the-empty-termlist [] '())
          (first-term [term-list] (first term-list))
          (rest-terms [term-list] (rest term-list))
          (empty-termlist? [term-list] (empty? term-list))
          (make-term [order coeff] (list order coeff))
          (order [term] (first term))
          (coeff [term] (second term))
          (mul-term-by-all-terms [t1 list1]
            (if (empty-termlist? list1)
              (the-empty-termlist)
              (let [t2 (first-term list1)]
                (adjoin-term
                 (make-term (+ (order t1) (order t2))
                            (mul (coeff t1) (coeff t2)))
                 (mul-term-by-all-terms t1 (rest-terms list1))))))
          (mul-terms [list1 list2]
            (if (empty-termlist? list1)
              (the-empty-termlist)
              (add-terms (mul-term-by-all-terms (first-term list1) list2)
                         (mul-terms (rest-terms list1) list2))))
          (add-poly [p1 p2]           
            {:pre [(same-variable? (variable p1) (variable p2))]}
            (make-poly (variable p1)
                       (add-terms (term-list p1)
                                  (term-list p2))))
          (mul-poly [p1 p2]
             {:pre [(same-variable? (variable p1) (variable p2))]}
             (make-poly (variable p1)
                        (mul-terms (term-list p1)
                                   (term-list p2))))
          (add-terms [list1 list2]
             (cond (empty-termlist? list1) list2
                   (empty-termlist? list2) list1
                   :else
                  (let [t1 (first-term list1)
                        t2 (first-term list2)]
                    (cond (> (order t1) (order t2))
                               (adjoin-term t1 (add-terms (rest-terms list1) list2))

                              (< (order t1) (order t2))
                              (adjoin-term t2 (add-terms list1 (rest-terms list2)))

                              :else
                              (adjoin-term
                               (make-term (order t1)
                                          (add (coeff t1) (coeff t2)))
                               (add-terms (rest-terms list1)
                                          (rest-terms list2)))))))
          (tag [p] (attach-tag :polynomial p))]
          ;; representation of terms and term lists
          ;; procedures adjoin-term and coeff from text below
;          add-poly [p]
;          ]
    (put-op :add '(:polynomial :polynomial) #(tag (add-poly %1 %2)))
    (put-op :mul '(:polynomial :polynomial) #(tag (mul-poly %1 %2)))
    (put-op :make ':polynomial #(tag (make-poly %1 %2)))
    :done))

(defn make-polynomial [var terms]
  ((get-op :make :polynomial) var terms))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(install-polynomial-package)

(def poly1-in-x (make-polynomial :x '((3 5) (1 2))))

(deftest test-make-new-polynomial
  (testing "FIXME, I fail."
    (is (= '(:polynomial (:x (3 5) (1 2)))
           poly1-in-x))))

(def poly1-in-y (make-polynomial :y '((3 5) (1 2))))

(deftest test-different-variable-assertions
  (testing "FIXME, I fail."
    (is (thrown? java.lang.AssertionError
                 (add poly1-in-x poly1-in-y)))))
