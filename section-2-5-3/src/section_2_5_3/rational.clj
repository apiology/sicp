(ns section-2-5-3.rational
  (:gen-class)
  (:require [section-2-5-3.log :refer :all]
            [section-2-5-3.module :refer :all]
            [section-2-5-3.math :refer :all]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rational module
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmacro ^:private xor 
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

(defn- gcd [x y]
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
