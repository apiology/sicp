(ns section-2-5-2.core-test
  (:require [clojure.test :refer :all]
            [section-2-5-2.core :refer :all]))

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
      (throw (Exception. (str "Could not find op " op-sym " with tags " (types-to-str type-tags) ".  Valid tags would be " (keys @operations))))
      op)))

(defn attach-tag [type-tag contents]
  (if (= type-tag :clj-number)
    (list contents)
    (list type-tag contents)))

(defn contents [datum]
  (if (= :clj-number (type-tag datum))
    datum
    (second datum)))

(defn apply-generic [op & args]
  (let [type-tags (map type-tag args)
        proc (get-op op type-tags)]
    (if proc
      (apply proc (map contents args))
      (throw (IllegalArgumentException.
              (str "No method for these types - apply-generic"
                   (list op type-tags)))))))

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
        bad-add-complex-to-cljnum (fn [z x]
                                    (make-from-real-imag (+ (real-part z) x)
                                                         (imag-part z)))
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
    (put-op :add '(:complex :clj-number) #(tag (bad-add-complex-to-cljnum %1 %2)))
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

;;;;;;;;;;;;;;;;;;
;install packages
;;;;;;;;;;;;;;;;;;

(install-complex-package)
(install-rectangular-package)

;;;; test a little


;; the bad way

(deftest test-bad-approach
  (testing "FIXME, I fail."
    (is (= (make-complex-from-real-imag 6 2) 
           (add (make-complex-from-real-imag 1 2)
                5)))))

;; slightly better way

(defn clj-number->complex [n]
  (make-complex-from-real-imag (contents n) 0))

(deftest test-slightly-better-approach
  (testing "FIXME, I fail."
    (is (= (make-complex-from-real-imag 5 0)
           (clj-number->complex 5)))))

;; (defn apply-generic [op & args]
;;   (let [type-tags (map type-tag args)
;;         proc (get-op op type-tags)]
;;     (if proc
;;       (apply proc (map contents args))
;;       (if (= (length args) 2)
;;         (let [type1 (first type-tags)
;;               type2 (second type-tags)
;;               a1 (XXX)]))
      
;;       (throw (IllegalArgumentException.
;;               (str "No method for these types - apply-generic"
;;                    (list op type-tags)))))))



