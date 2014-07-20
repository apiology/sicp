(ns section-2-5-3.core
  (:gen-class)
  (require [clojure.string :as str]
           [clojure.math.numeric-tower :as math]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; general infrastructure
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn log [& args]
;  (println (pretty-format args))
)

(defn coll-to-str [coll]
;  (println (str "Called coll-to-str on " (str/join " " coll)))
  (let [ret (str/join " " (flatten coll))]
;    (println (str "Returning " ret))
;    (println (str "...which is of type " (class ret)))
    ret))


(defn types-to-str [type-tags]
  (if (seq? type-tags)
    (do
      ; (println (str "Called types-to-str on " (str/join " " type-tags)))
      (coll-to-str type-tags))
    (str type-tags)))

(defn args-to-str [args]
  (coll-to-str args))

(defn gcd [x y]
  {:pre [(>= x 0)
         (>= y 0)
         (integer? x)
         (integer? y)]
   :post [(> % 0)]}
  (log "Calling (gcd " x " " y ")")
  (cond
   (= x 0) 1
   (= y 0) 1
   (> x y) (gcd (- x y) y)
   (< x y) (gcd x (- y x))
   :else x))

(defmacro xor 
  ([] nil)
  ([a] a)
  ([a b]
    `(let [a# ~a
           b# ~b]
      (if a# 
        (if b# nil a#)
        (if b# b# nil))))
  ([a b & more]
   `(xor (xor ~a ~b) (xor ~@more))))

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

(defn get-op-or-fail [op-sym type-tags]
  (if-let [op (get-op op-sym type-tags)]
    op
    (throw (IllegalArgumentException. (str "Could not find operation " op-sym " on type tags " (types-to-str type-tags))))))

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
(declare drop)

(defn apply-generic [op & args]
  (drop (apply apply-generic-no-simplify op args)))

(defn cant-resolve-op [op types]
  (throw (Exception. (str "Could not find op " op 
                          " with tags " (types-to-str types) " (class " (class types)
                          ").  Valid tags would be " (keys @operations)))))

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

(declare equ?)
(declare raise)
(declare apply-generic-or-nil)

(defn project-one-step [x] 
  (log "(project-one-step " x ")")
  (apply-generic-or-nil :project-one-step x))

(defn drop-item-one-step [num]
  (if-let [projected (project-one-step num)]
    (let [raised (raise projected)]
;      (log "drop-item-one-step: Projected is " projected ", raised is " raised)
      (if (equ? num raised)
        projected
        nil))))

(defn drop [num]
  (if-let [next-step (drop-item-one-step num)]
    (recur next-step)
    num))

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
    (log "Trying apply-generic for " op " with args " (args-to-str current-args))
    (if (nil? current-args)
      (cant-resolve-op op (map type-tag args))
      (let [type-tags (map type-tag current-args)
            proc (get-op op type-tags)]
        (if proc
          (apply proc (map contents current-args))
          (do 
            (log "Couldn't figure out an op with current args--raising one step "
                 (args-to-str current-args))
            (let [next-args (raise-one-step current-args)]
              (recur next-args))))))))


(defn apply-generic-or-nil [op & args]
  (let [type-tags (map type-tag args)
        proc (get-op op type-tags)]
    (when proc
      (apply proc (map contents args)))))



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
  (if (gt (abs angle) 0.1)
    (p (sine (div angle 3.0)))
    angle))
    

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
;; Polynomial module
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
  ((get-op-or-fail :make :polynomial) var terms))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; clj-number module
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn install-clj-number-package []
  (let [tag #(attach-tag :clj-number %)]
    (put-op :add '(:clj-number :clj-number) #(tag (+ %1 %2)))
    (put-op :sub '(:clj-number :clj-number) #(tag (- %1 %2)))
    (put-op :mul '(:clj-number :clj-number) #(tag (* %1 %2)))
    (put-op :div '(:clj-number :clj-number) #(tag (/ %1 %2)))
    (put-op :lt '(:clj-number :clj-number) #(< %1 %2))
    (put-op :gt '(:clj-number :clj-number) #(> %1 %2))
    (put-op :equ? '(:clj-number :clj-number) =)
    (put-op :atan2 '(:clj-number :clj-number) #(Math/atan2 %1 %2))
    (put-op :exp '(:clj-number :clj-number) #(tag (math/expt %1 %2)))
    (put-op :=zero? '(:clj-number) #(= 0.0 %1))
    (put-op :make :clj-number #(tag %)))
  :done)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; polar module
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn install-polar-package []
  (letfn [(magnitude [z] (first z))
          (angle [z] (second z))
          (make-from-mag-ang [mag ang]
            (cons mag ang))
          (real-part [z]
            (mul (magnitude z) (cosine (angle z))))
          (imag-part [z]
            (mul (magnitude z) (sine (angle z))))
          (make-from-real-imag [real imag]
            (cons (Math/sqrt (+ (square real) (square imag)))
                  (Math/atan2 imag real)))
          (tag [x] (attach-tag :polar x))
          (make-from-mag-ang [r a] (list r a))
          (=zero? [z] (= (magnitude z) 0))]
    (put-op :equ? '(:polar :polar) =)
    (put-op :=zero? '(:polar) #(= (magnitude %1) 0))
    (put-op :real-part '(:polar) real-part)
    (put-op :imag-part '(:polar) imag-part)
    (put-op :magnitude '(:polar) magnitude)
    (put-op :angle '(:polar) angle)
    (put-op :make-from-real-imag :polar
            (fn [x y] (tag (make-from-real-imag x y))))
    (put-op :make-from-mag-ang :polar (fn [r a] (tag (make-from-mag-ang r a))))
    :done))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rational module
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn install-rational-package []
  (let [numer #(first %)
        denom #(second %)
        make-rat (fn [n d] (if (and (integer? n) (integer? d))
                             (let [g (gcd (abs n) (abs d))]
                               (list (/ n g) (/ d g)))
                             (list n d)))
        add-rat #(make-rat (+ (* (numer %1) (denom %2))
                              (* (numer %2) (denom %1)))
                           (* (denom %1) (denom %2)))
        sub-rat #(do
                   (log "(sub-rat " %1 %2 ")")
                   (let [ret (make-rat (- (* (numer %1) (denom %2))
                                          (* (numer %2) (denom %1)))
                                       (* (denom %1) (denom %2)))]
                     (log "(sub-rat " %1 %2 ") = " ret)
                     ret))
        mul-rat #(make-rat (* (numer %1) (numer %2))
                           (* (denom %1) (denom %2)))
        div-rat #(make-rat (* (numer %1) (denom %2))
                           (* (denom %1) (numer %2)))
        is-neg?-rat #(let [denom-is-neg? (lt (denom %1) 0)
                           numer-is-neg? (lt (numer %1) 0)
                           ret (true? (xor numer-is-neg? denom-is-neg?))]
                       (log "denom-is-neg? on " %1 " = " denom-is-neg?)
                       (log "numer-is-neg? on " %1 " = " numer-is-neg?)
                       (log "(is-neg-rat? " %1 ") = " ret)
                       ret)
        lt-rat #(is-neg?-rat (sub-rat %1 %2))
        gt-rat #(is-neg?-rat (sub-rat %2 %1))
        tag #(attach-tag :rational %)]
    (put-op :lt '(:rational :rational) #(lt-rat %1 %2))
    (put-op :gt '(:rational :rational) #(gt-rat %1 %2))
    (put-op :add '(:rational :rational) #(tag (add-rat %1 %2)))
    (put-op :atan2 '(:rational :rational) #(atan2 (div (numer %1)
                                                       (denom %1))
                                                  (div (numer %2)
                                                       (denom %2))))
    (put-op :sub '(:rational :rational) #(tag (sub-rat %1 %2)))
    (put-op :mul '(:rational :rational) #(tag (mul-rat %1 %2)))
    (put-op :div '(:rational :rational) #(tag (div-rat %1 %2)))
    (put-op :equ? '(:rational :rational) =)
    (put-op :=zero? '(:rational) #(= (numer %1) 0))
    (put-op :lower-type :rational (fn [] :clj-number))
    (put-op :raise '(:clj-number) #(tag (make-rat %1 1)))
    (put-op :project-one-step '(:rational) #(numer %1))
    (put-op :make :rational #(tag (make-rat %1 %2)))))

(defn make-rational [n d]
  ((get-op-or-fail :make :rational) n d))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rectangular module
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn install-rectangular-package []
  (letfn [(real-part [z] (first z))
          (imag-part [z] (second z))
          (make-from-real-imag [real imag] (list real imag))
          (magnitude [z] (sqrt (add (square (real-part z))
                                    (square (imag-part z)))))
          (angle [z] (atan2 (imag-part z)
                            (real-part z)))
          (make-from-mag-ang [mag ang] (cons (* mag (Math/cos ang))
                                             (* mag (Math/sin ang))))
          (make-from-real-imag [x y] (list x y))
          (tag [x] (attach-tag :rectangular x))]
    (put-op :equ? '(:rectangular :rectangular) =)
    (put-op :=zero? '(:rectangular) (fn [n] (let [r (real-part n) 
                                                  i (imag-part n)]
                                              (and (= r 0) (= i 0)))))
    (put-op :real-part '(:rectangular) real-part)
    (put-op :imag-part '(:rectangular) imag-part)
    (put-op :magnitude '(:rectangular) magnitude)
    (put-op :angle '(:rectangular) angle)
    (put-op :make-from-real-imag :rectangular
            (fn [x y] (tag (make-from-real-imag x y))))
    (put-op :make-from-mag-ang :rectangular
            (fn [r a] (tag (make-from-mag-ang r a))))
    (put-op :project-one-step '(:rectangular) #(real-part %1))
    :done))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; complex module
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn install-complex-package []
  (let [make-from-real-imag #((get-op-or-fail :make-from-real-imag :rectangular) %1 %2)
        make-from-mag-ang #((get-op-or-fail :make-from-mag-ang :polar) %1 %2)
        ;; internal procedures
        add-complex (fn [z1 z2]
                      (make-from-real-imag (add (real-part z1) (real-part z2))
                                           (add (imag-part z1) (imag-part z2))))
        sub-complex (fn [z1 z2]
                      (make-from-real-imag (sub (real-part z1) (real-part z2))
                                           (sub (imag-part z1) (imag-part z2))))
        mul-complex (fn [z1 z2]
                      (make-from-mag-ang (mul (magnitude z1) (magnitude z2))
                                         (add (angle z1) (angle z2))))
        div-complex (fn [z1 z2]
                      (make-from-mag-ang (div (magnitude z1) (magnitude z2))
                                         (sub (angle z1) (angle z2))))
        tag #(attach-tag :complex %)]
    (put-op :add '(:complex :complex) #(tag (add-complex %1 %2)))
    (put-op :sub '(:complex :complex) #(tag (sub-complex %1 %2)))
    (put-op :mul '(:complex :complex) #(tag (mul-complex %1 %2)))
    (put-op :div '(:complex :complex) #(tag (div-complex %1 %2)))
    (put-op :equ? '(:complex :complex) #(equ? %1 %2))
    (put-op :=zero? '(:complex) =zero?)
    (put-op :make-from-real-imag :complex #(tag (make-from-real-imag %1 %2)))
    (put-op :make-from-mag-ang :complex #(tag (make-from-mag-ang %1 %2)))
    ;; below added as part of Exercise 2.77
    (put-op :real-part '(:complex) real-part)
    (put-op :imag-part '(:complex) imag-part)
    (put-op :magnitude '(:complex) magnitude)
    (put-op :angle '(:complex) angle)
    (put-op :lower-type :complex (fn [] :rational))
    (put-op :project-one-step '(:complex) #(project-one-step %1))
    (put-op :raise '(:rational) #(tag (make-from-real-imag (apply make-rational %1) 0)))
    'done))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn make-complex-from-real-imag [x y]
  ((get-op-or-fail :make-from-real-imag :complex) x y))

(defn make-complex-from-mag-ang [x y]
  ((get-op-or-fail :make-from-mag-ang :complex) x y))



