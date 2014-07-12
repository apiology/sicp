(ns section-2-5-2.core-test
  (:require [clojure.test :refer :all]
            [section-2-5-2.core :refer :all]
            [clojure.math.numeric-tower :as math]))


;; common stuff
(defn gcd [x y]
  (cond
   (= x 0) 1
   (= y 0) 1
   (> x y) (recur (- x y) y)
   (< x y) (recur x (- y x))
   :else x))

(defn square [x] (* x x))

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

(declare apply-generic)
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

(defn apply-generic [op & args]
  (let [type-tags (map type-tag args)
        proc (get-op op type-tags)]
    (if proc
      (apply proc (map contents args))
      (apply-generic-with-coercions op type-tags args))))


;(defn make-from-real-imag [real imag]
;  (apply-generic :make-from-real-imag real imag))

(defn real-part [z]
  (apply-generic :real-part z))

(defn imag-part [z]
  (apply-generic :imag-part z))

(defn magnitude [z]
  (apply-generic :magnitude z))

(defn angle [z]
  (apply-generic :angle z))

(defn equ? [a b]
  (apply-generic :equ? a b))

(defn =zero? [num]
  (apply-generic :=zero? num))

(defn add [x y] (apply-generic :add x y))
(defn sub [x y] (apply-generic :sub x y))
(defn mul [x y] (apply-generic :mul x y))
(defn div [x y] (apply-generic :div x y))
(defn exp [x y] (apply-generic :exp x y))


;;;;;;;;;;;;;;;;;;

;; complex module

(defn install-complex-package []
  (let [make-from-real-imag #((get-op :make-from-real-imag '(:rectangular)) %1 %2)
        make-from-mag-ang #((get-op :make-from-mag-ang '(:polar)) %1 %2)
        ;; internal procedures
        add-complex (fn [z1 z2]
                      (make-from-real-imag (+ (real-part z1) (real-part z2))
                                           (+ (imag-part z1) (imag-part z2))))
        sub-complex (fn [z1 z2]
                      (make-from-real-imag (- (real-part z1) (real-part z2))
                                           (- (imag-part z1) (imag-part z2))))
        mul-complex (fn [z1 z2]
                      (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                                         (+ (angle z1) (angle z2))))
        div-complex (fn [z1 z2]
                      (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                                         (- (angle z1) (angle z2))))
        tag #(attach-tag :complex %)]
    (put-op :add '(:complex :complex) #(tag (add-complex %1 %2)))
    (put-op :sub '(:complex :complex) #(tag (sub-complex %1 %2)))
    (put-op :mul '(:complex :complex) #(tag (mul-complex %1 %2)))
    (put-op :div '(:complex :complex) #(tag (div-complex %1 %2)))
    (put-op :equ? '(:complex :complex) #(equ? %1 %2))
    (put-op :=zero? '(:complex) =zero?)
    (put-op :make-from-real-imag '(:complex) #(tag (make-from-real-imag %1 %2)))
    (put-op :make-from-mag-ang '(:complex) #(tag (make-from-mag-ang %1 %2)))
    ;; below added as part of Exercise 2.77
    (put-op :real-part '(:complex) real-part)
    (put-op :imag-part '(:complex) imag-part)
    (put-op :magnitude '(:complex) magnitude)
    (put-op :angle '(:complex) angle)
    'done))

(defn make-complex-from-real-imag [x y]
  ((get-op :make-from-real-imag '(:complex)) x y))

(defn make-complex-from-mag-ang [x y]
  ((get-op :make-from-mag-ang '(:complex)) x y))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; rectangular module

(defn install-rectangular-package []
  (letfn [(real-part [z] (first z))
          (imag-part [z] (second z))
          (make-from-real-imag [real imag] (list real imag))
          (magnitude [z] (Math/sqrt (+ (square (real-part z))
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
    (put-op :make-from-real-imag '(:rectangular)
            (fn [x y] (tag (make-from-real-imag x y))))
    (put-op :make-from-mag-ang '(:rectangular)
            (fn [r a] (tag (make-from-mag-ang r a))))
    :done))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn install-clj-number-package []
  (let [tag #(attach-tag :clj-number %)]
    (put-op :add '(:clj-number :clj-number) #(tag (+ %1 %2)))
    (put-op :sub '(:clj-number :clj-number) #(tag (- %1 %2)))
    (put-op :mul '(:clj-number :clj-number) #(tag (* %1 %2)))
    (put-op :div '(:clj-number :clj-number) #(tag (/ %1 %2)))
    (put-op :equ? '(:clj-number :clj-number) =)
    (put-op :exp '(:clj-number :clj-number) #(tag (math/expt %1 %2)))
    (put-op :=zero? '(:clj-number) #(= 0.0 %1))
    (put-op :make :clj-number #(tag %)))
  :done)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;
;install packages
;;;;;;;;;;;;;;;;;;

(install-clj-number-package)
(install-complex-package)
(install-rectangular-package)

;;;; test a little


;; the bad way


;; slightly better way

(defn clj-number->complex [n]
  (make-complex-from-real-imag (contents n) 0))

(deftest test-slightly-better-approach
  (testing "FIXME, I fail."
    (is (= (make-complex-from-real-imag 5 0)
           (clj-number->complex 5)))))



(put-coercion :clj-number :complex clj-number->complex)

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
