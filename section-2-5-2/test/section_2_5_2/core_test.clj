(ns section-2-5-2.core-test
  (:require [clojure.test :refer :all]
            [section-2-5-2.core :refer :all]
            [clojure.math.numeric-tower :as math]))

;(defn space-out [coll]
;  (map str coll))
(defn pretty-format [args]
  (apply str args))

(defn log [& args]
;  (println (pretty-format args))
)


;; common stuff
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

;; module infrastructure

(def coercions (atom {}))

(defn put-coercion [input-type output-type coercion-fn]
  "Add to the global list of coercion based on the two-element list of
   type tags describing the input and the output of the type, and the
   function (coercion-fn) which should be called if matched"
  (swap! coercions assoc (list input-type output-type) coercion-fn))

(defn get-coercion [input-type output-type]
  (let [coercion (get @coercions (list input-type output-type))] 
    (if (nil? coercion)
      nil
      coercion)))

(def operations (atom {}))

(defn put-op [op-sym type-tags op-fn]
  "Add to the global list of operations based on the function
   name (op-sym), a list of type tags describing the arguments of the
   type, and the function (op-fn) which should be called if matched"
  (swap! operations assoc (list op-sym type-tags) op-fn))

(defn type-tag [datum]
  (cond (number? datum) :clj-number
        (seq? datum) (first datum)
        :else (throw (IllegalArgumentException. (str "Bad tagged dataum -- TYPE-TAG "
                                                     datum)))))


(defn types-to-str [type-tags]
  (apply str (flatten (list type-tags))))

(defn get-op [op-sym type-tags]
  (let [op (get @operations (list op-sym type-tags))] 
    (if (nil? op)
      nil
      op)))

(defn attach-tag [type-tag contents]
  (if (= type-tag :clj-number)
    contents
    (list type-tag contents)))

(defn contents [datum]
  (if (= :clj-number (type-tag datum))
    datum
    (second datum)))

(declare drop)
(declare apply-generic)
(declare apply-generic-no-simplify)
(defn cant-resolve-op [op types]
  (throw (Exception. (str "Could not find op " op 
                          " with tags " (types-to-str types)
                          ".  Valid tags would be " (keys @operations)))))

(defn coerce-value [new-type value]
  (let [old-type (type-tag value)
        coercion-fn (get-coercion old-type new-type)]
    (if coercion-fn
      (coercion-fn value)
      (if (= new-type old-type)
        value
        nil))))

(defn coerce-args [unified-type args]
  (let [new-args (map #(coerce-value unified-type %) args)]
    (if (some nil? new-args)
      nil ;; couldn't coerce at least one arg
      new-args)))

(defn lower-type [type]
  (if-let [fn (get-op :lower-type type)]
    (fn)
    nil))


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

(defn apply-generic-with-coercions [op type-tags args]
  (let [unique-types (set type-tags)]
    (loop [unified-type (first unique-types)
           remaining-types (rest unique-types)]
      (let [unified-type-tags (repeat (count args) unified-type)]
        (if-let [new-args (coerce-args unified-type args)]
          (if-let [proc (get-op op unified-type-tags)]
            (apply proc (map contents new-args))
            (if (empty? remaining-types)
              (cant-resolve-op op type-tags)
              (recur (first remaining-types) (rest remaining-types))))
          (if (empty? remaining-types)
            (cant-resolve-op op type-tags)
            (recur (first remaining-types) (rest remaining-types))))))))


(defn apply-generic-or-nil [op & args]
  (let [type-tags (map type-tag args)
        proc (get-op op type-tags)]
    (if proc
      (apply proc (map contents args))
      nil)))


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

(defn project-one-step [x] 
  (log "(project-one-step " x ")")
  (apply-generic-or-nil :project-one-step x))

(defn raise-one-step [args]
  (let [type-tags-vec (vec (map type-tag args))
        args-vec (vec args)
        index-to-raise (find-index-of-type-to-raise type-tags-vec)
        arg-to-raise (nth args index-to-raise nil)]
    (log "type-tags-vec: " (apply str type-tags-vec))
    (log "arg-to-raise: " arg-to-raise)
    (if arg-to-raise
      (if-let [raised-arg (raise arg-to-raise)]
        (do
          (log "raised-arg: " raised-arg)
          (let [ret (seq (assoc args-vec index-to-raise raised-arg))]
            (log "(raise-one-step " (apply str args) " is returning " (apply str ret))
            ret))
        nil)
      nil)))

(defn apply-generic [op & args]
  (drop (apply apply-generic-no-simplify op args)))

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

(defn lt [x y] (let [ret (apply-generic-no-simplify :lt x y)]
                 (log "(lt " x " " y ") = " ret)
                 ret))
(defn gte [x y] (let [ret (not (lt x y))]
                  (log "(gte " x " " y ") = " ret)
                  ret))
(defn add [x y] (apply-generic :add x y))
(defn sub [x y] (apply-generic :sub x y))
(defn mul [x y] (apply-generic :mul x y))
(defn div [x y] (apply-generic :div x y))
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
        tag #(attach-tag :rational %)]
    (put-op :lt '(:rational :rational) #(lt-rat %1 %2))
    (put-op :add '(:rational :rational) #(tag (add-rat %1 %2)))
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
  ((get-op :make :rational) n d))

;;;;;;;;;;;;;;;;;;

;; complex module

(defn install-complex-package []
  (let [make-from-real-imag #((get-op :make-from-real-imag :rectangular) %1 %2)
        make-from-mag-ang #((get-op :make-from-mag-ang :polar) %1 %2)
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

(defn make-complex-from-real-imag [x y]
  ((get-op :make-from-real-imag :complex) x y))

(defn make-complex-from-mag-ang [x y]
  ((get-op :make-from-mag-ang :complex) x y))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; rectangular module

(defn install-rectangular-package []
  (letfn [(real-part [z] (first z))
          (imag-part [z] (second z))
          (make-from-real-imag [real imag] (list real imag))
          (magnitude [z] (sqrt (add (square (real-part z))
                                    (square (imag-part z)))))
          (angle [z] (Math/atan2 (imag-part z)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn install-clj-number-package []
  (let [tag #(attach-tag :clj-number %)]
    (put-op :add '(:clj-number :clj-number) #(tag (+ %1 %2)))
    (put-op :sub '(:clj-number :clj-number) #(tag (- %1 %2)))
    (put-op :mul '(:clj-number :clj-number) #(tag (* %1 %2)))
    (put-op :div '(:clj-number :clj-number) #(tag (/ %1 %2)))
    (put-op :lt '(:clj-number :clj-number) #(< %1 %2))
    (put-op :equ? '(:clj-number :clj-number) =)
    (put-op :exp '(:clj-number :clj-number) #(tag (math/expt %1 %2)))
    (put-op :=zero? '(:clj-number) #(= 0.0 %1))
    (put-op :make :clj-number #(tag %)))
  :done)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn install-polar-package []
  (letfn [(magnitude [z] (first z))
          (angle [z] (second z))
          (make-from-mag-ang [mag ang]
            (cons mag ang))
          (real-part [z]
            (* (magnitude z) (Math/cos (angle z))))
          (imag-part [z]
            (* (magnitude z) (Math/sin (angle z))))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;
;install packages
;;;;;;;;;;;;;;;;;;

(install-clj-number-package)
(install-rectangular-package)
(install-polar-package)
(install-complex-package)
(install-rational-package)

;;;; test a little


;; the bad way


;; slightly better way

(defn clj-number->complex [n]
  (make-complex-from-real-imag (contents n) 0))

(deftest test-lt
  (testing "positive"
    (is (= false
           (lt 1 0))))
  (testing "negative"
    (is (= true
           (lt -1 0)))))

(deftest test-abs
  (testing "positive 1"
    (is (= 1
           (abs 1))))
  (testing "positive 2"
    (is (= 2
           (abs 2))))
  (testing "negative"
    (is (= 1
           (abs -1))))
  (testing "negative 2"
    (is (= 2
           (abs -2)))))


(deftest test-slightly-better-approach
  (testing "FIXME, I fail."
    (is (= (make-complex-from-real-imag 5 0)
           (clj-number->complex 5)))))



(put-coercion :clj-number :complex clj-number->complex)

(deftest make-rational-test
  (testing "can create negative rationals"
    (is (= '(:rational (-1 1))
           (make-rational -1 1)))))

(deftest test-slightly-better-approach
  (testing "FIXME, I fail."
    (is (= (make-complex-from-real-imag 5 0)
           (clj-number->complex 5)))))

(deftest test-adding-mixed
  (testing "FIXME, I fail."
    (is (= (make-complex-from-real-imag 6 2) 
           (add (make-complex-from-real-imag 1 2)
                5)))))

;;
;; Exercise 2.81
;;

;; Will it try to coerce into each other's type even if types are same?

; well, yes, but only if the op doesn't exist already.

;; With like-to-like coercion installed, what happens if apply-generic
;; is called with two args of clj-number or complex?

; If the function is installed, we'll use it.  If not, we'll look for
; coercion and try it...infinitely, as we keep finding it and trying
; it.  it would be bad.

(deftest test-exponentiating-clj-number
  (testing "exponentiation clojure numbers"
    (is (= (exp 2 5) 32))))

;; what happens if we call exp with two complex numbers as arguments?

(deftest test-exponentiating-complexes
  (testing "exponentiation complex numbers"
    (is (thrown? java.lang.Exception
           (exp (make-complex-from-real-imag 6 2)
                (make-complex-from-real-imag 6 2))))))
         
;; b. Is Louis correct that something had to be done abou coercion
;; with arguments of the same type, or does apply-generic work
;; correctly as is?

; probably wouldn't be a bad idea.

;;
;; Exercise 2.82
;;

;; When trying to to coerce all to the same type, you're going to run
;; into problems where mixed types don't work correctly.  For
;; instance, if we had a complex->integer function foo(complex,
;; integer), and tried to call it with two integers, we wouldn't find
;; the value.

;; See above for implementation

;; Exercise 2.83

;; Design a procedure that raises objects of that type one level in
;; the tower.  Show how to install a generic raise operation that will
;; work for each type (except complex)


(deftest test-raise
  (testing "can raise clj-number"
    (is (= '(:rational (3 1))
           (raise 3))))
  (testing "can raise zero clj-number"
    (is (= '(:rational (0 1))
           (raise 0))))
  (testing "can raise negative clj-number"
    (is (= '(:rational (-1 1))
           (raise -1))))
  (testing "can raise floating clj-number"
    (is (= '(:rational (0.5 1))
           (raise 0.5))))
  (testing "can raise negative floating clj-number"
    (is (= '(:rational (-0.5 1))
           (raise -0.5))))
  (testing "can raise rational"
    (is (= '(:complex (:rectangular ((:rational (3 1)) 0)))
           (raise (raise 3))))))


;; Exercise 2.84

;; Modify apply-generic so that it coerces its arguments to have the
;; same type by the method of successive raisign.  Need to be able to
;; test which of two types is higher.  Need to be "compatible" with
;; rest of system and not lead to rpoblems adding new levels to tower.

(deftest test-lower-type
  (testing "lower-type one"
    (is (= nil
           (lower-type :clj-number))))
  (testing "lower-type two"
    (is (= :clj-number
           (lower-type :rational))))
  (testing "lower-type three"
    (is (= :rational
           (lower-type :complex)))))

(deftest test-is-lower
  (testing "is-lower-type? one"
    (is (= false
           (is-lower-type? :clj-number :clj-number))))
  (testing "is-lower-type? two"
    (is (= true
           (is-lower-type? :clj-number :rational))))
  (testing "is-lower-type? three"
    (is (= true
           (is-lower-type? :rational :complex))))
  (testing "is-lower-type? three"
    (is (= false
           (is-lower-type? :rational :clj-number))))
  (testing "is-lower-type? four"
    (is (= true
           (is-lower-type? :clj-number :complex)))))

(deftest test-type-comparator
  (testing "type-comparator one"
    (is (= 0
           (type-comparator :clj-number :clj-number))))
  (testing "type-comparator two"
    (is (= 0
           (type-comparator :rational :rational))))
  (testing "type-comparator three"
    (is (= 0
           (type-comparator :rational :rational))))
  (testing "type-comparator three"
    (is (= -1
           (type-comparator :rational :complex))))
  (testing "type-comparator four"
    (is (= 1
           (type-comparator :rational :clj-number)))))

(deftest test-find-lowest-type
  (testing "find-lowest-type one"
    (is (= :clj-number
           (find-lowest-type #{:clj-number}))))
  (testing "find-lowest-type two"
    (is (= :rational
           (find-lowest-type #{:rational}))))
  (testing "find-lowest-type three"
    (is (= :rational
           (find-lowest-type '(:rational :rational)))))
  (testing "find-lowest-type three"
    (is (= :rational
           (find-lowest-type '(:rational :complex)))))
  (testing "find-lowest-type four"
    (is (= :clj-number
           (find-lowest-type '(:rational :clj-number :complex))))))

(deftest test-find-index-of-type-to-raise
  (testing "find-index-of-type-to-raise one"
    (is (= 0
           (find-index-of-type-to-raise '(:clj-number)))))
  (testing "find-index-of-type-to-raise two"
    (is (= 0
           (find-index-of-type-to-raise '(:rational)))))
  (testing "find-index-of-type-to-raise three"
    (is (= 0
           (find-index-of-type-to-raise '(:rational :rational)))))
  (testing "find-index-of-type-to-raise three"
    (is (= 0
           (find-index-of-type-to-raise '(:rational :complex)))))
  (testing "find-index-of-type-to-raise four"
    (is (= 1
           (find-index-of-type-to-raise '(:rational :clj-number))))))

(deftest test-raise-one-step
  (testing "raise-one-step one"
    (is (= '((:rational (2 1)))
           (raise-one-step (list 2)))))
  (testing "raise-one-step one"
    (is (= '((:rational (2 1)) 2)
           (raise-one-step (list 2 2)))))
  (testing "raise-one-step two"
    (is (= '((:complex (:rectangular ((:rational (1 2)) 0))))
           (raise-one-step (list (make-rational 1 2))))))
  (testing "raise-one-step three"
    (is (= '((:complex (:rectangular ((:rational (1 2)) 0))) (:rational (3 4)))
           (raise-one-step (list (make-rational 1 2) (make-rational 3 4))))))
  (testing "raise-one-step three"
    (is (= '((:complex (:rectangular ((:rational (1 2)) 0))) (:complex (:rectangular (3 4))))
           (raise-one-step (list (make-rational 1 2) (make-complex-from-real-imag 3 4))))))
  (testing "raise-one-step four"
    (is (= '((:rational (1 2)) (:rational (2 1)))
           (raise-one-step (list (make-rational 1 2) 2)))))
  (testing "raise-one-step five"
    (is (= nil
           (raise-one-step (list (make-complex-from-real-imag 3 4) (make-complex-from-real-imag 3 4))))))
  (testing "raise-one-step six"
    (is (= '((:rational 0 1) (:rational (-0.5 1)))
           (raise-one-step '((:rational 0 1) -0.5))))))


;;
;; Exercise 2.85
;;


(deftest test-project-one-step
  (testing "project a clj-number unsuccessfully"
    (is (= nil
           (project-one-step 5))))
  (testing "project a rational"
    (is (= 5
           (project-one-step (make-rational 5 1)))))
  (testing "project a rational unsuccessfully"
    (is (= 5
           (project-one-step (make-rational 5 2)))))
  (testing "project a complex to a rational"
    (is (= '(:rational (1 2))
           (project-one-step (make-complex-from-real-imag (make-rational 1 2) 0)))))
  (testing "project a complex to a clj-number"
    (is (= 2
           (project-one-step (make-complex-from-real-imag 2 0)))))
  (testing "project a complex unsuccessfully"
    (is (= 2
           (project-one-step (make-complex-from-real-imag 2 1))))))

(defn drop-item-one-step [num]
  (if-let [projected (project-one-step num)]
    (let [raised (raise projected)]
;      (log "drop-item-one-step: Projected is " projected ", raised is " raised)
      (if (equ? num raised)
        projected
        nil))))

(deftest test-drop-item-one-step
  (testing "drop a clj-number unsuccessfully"
    (is (= nil
           (drop-item-one-step 5))))
  (testing "drop a rational"
    (is (= 5
           (drop-item-one-step (make-rational 5 1)))))
  (testing "drop a rational unsuccessfully"
    (is (= nil
           (drop-item-one-step (make-rational 5 2)))))
  (testing "drop a complex to a rational"
    (is (= '(:rational (1 2))
           (drop-item-one-step (make-complex-from-real-imag (make-rational 1 2) 0)))))
  (testing "drop a complex to a clj-number"
    (is (= (make-rational 2 1)
           (drop-item-one-step (make-complex-from-real-imag (make-rational 2 1) 0)))))
  (testing "drop a complex unsuccessfully"
    (is (= nil
           (drop-item-one-step (make-complex-from-real-imag (make-rational 2 1) 1))))))

(defn drop [num]
  (if-let [next-step (drop-item-one-step num)]
    (recur next-step)
    num))

(deftest test-drop
  (testing "drop a clj-number unsuccessfully"
    (is (= 5
           (drop 5))))
  (testing "drop a rational"
    (is (= 5
           (drop (make-rational 5 1)))))
  (testing "drop a rational unsuccessfully"
    (is (= (make-rational 5 2)
           (drop (make-rational 5 2)))))
  (testing "drop a complex to a rational"
    (is (= '(:rational (1 2))
           (drop (make-complex-from-real-imag (make-rational 1 2) 0)))))
  (testing "drop a complex to a clj-number"
    (is (= 2
           (drop (make-complex-from-real-imag (make-rational 2 1) 0)))))
  (testing "drop a complex unsuccessfully"
    (is (= (make-complex-from-real-imag (make-rational 2 1) 1)
           (drop (make-complex-from-real-imag (make-rational 2 1) 1))))))

;;
;; Exercise 2.86
;;

(deftest test-complex-generic-ops-magnitude
  (testing "magnitude of a regular rectangular complex"
    (is (= (sqrt 2)
           (magnitude (make-complex-from-real-imag 1 1)))))
  (testing "magnitude of a rational rectangular complex"
    (is (= (sqrt (add (square (make-rational 1 2)) (square (make-rational 1 2))))
           (magnitude (make-complex-from-real-imag (make-rational 1 2) 
                                                   (make-rational 1 2))))))
  (testing "magnitude of a regular polar complex"
    (is (= 1
           (magnitude (make-complex-from-mag-ang 1 1)))))
  (testing "magnitude of a rational polar complex"
    (is (= 1
           (magnitude (make-complex-from-mag-ang (make-rational 1 1) (make-rational 1 1)))))))

(deftest test-complex-generic-ops-real
  (testing "real-part of a regular rectangular complex"
    (is (= 1
           (real-part (make-complex-from-real-imag 1 1)))))
  (testing "real-part of a rational rectangular complex"
    (is (= '(:rational (1 2))
           (real-part (make-complex-from-real-imag (make-rational 1 2) 
                                                   (make-rational 1 2))))))
  (testing "real-part of a regular polar complex"
    (is (= 0.5403023058681398
           (real-part (make-complex-from-mag-ang 1 1)))))
;  (testing "real-part of a rational polar complex"
;    (is (= nil
;           (real-part (make-complex-from-mag-ang (make-rational 1 1) (make-rational 1 1))))))
)

;; todo: track down references to Math/... above and debug here
