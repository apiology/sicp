(ns section-2-5-1.core-test
  (:require [clojure.test :refer :all]
            [section-2-5-1.core :refer :all]))

;;;        
;;; code a little
;;;
(defn gcd [x y]
  (cond
   (> x y) (recur (- x y) y)
   (< x y) (recur x (- y x))
   :else x))

(defn square [x] (* x x))

(def operations (atom {}))

(defn put-op [op-sym type-tags op-fn]
  "Add to the global list of operations based on the function
   name (op-sym), a list of type tags describing the arguments of the
   type, and the function (op-fn) which should be called if matched"
  (swap! operations assoc (list op-sym type-tags) op-fn))

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

(defn type-tag [datum]
  (if (seq datum)
    (let [fitem (first datum)]
      (if (number? fitem)
        :clj-number
        fitem))
    (throw (IllegalArgumentException. (str "Bad tagged dataum -- TYPE-TAG "
                                             datum)))))

(defn contents [datum]
  (if (seq datum)
    (if (= :clj-number (type-tag datum))
      (first datum)
      (second datum))
    (throw (IllegalArgumentException. (str "Bad tagged dataum -- CONTENTS "
                                           datum)))))

(defn apply-generic [op & args]
  (let [type-tags (map type-tag args)
        proc (get-op op type-tags)]
    (if proc
      (apply proc (map contents args))
      (throw (IllegalArgumentException.
              (str "No method for these types - apply-generic"
                   (list op type-tags)))))))

(defn add [x y] (apply-generic :add x y))
(defn sub [x y] (apply-generic :sub x y))
(defn mul [x y] (apply-generic :mul x y))
(defn div [x y] (apply-generic :div x y))

(defn install-clj-number-package []
  (let [tag #(attach-tag :clj-number %)]
    (put-op :add '(:clj-number :clj-number) #(tag (+ %1 %2)))
    (put-op :sub '(:clj-number :clj-number) #(tag (- %1 %2)))
    (put-op :mul '(:clj-number :clj-number) #(tag (* %1 %2)))
    (put-op :div '(:clj-number :clj-number) #(tag (/ %1 %2)))
    (put-op :make :clj-number #(tag %)))
  :done)

(defn make-clj-number [n]
  ((get-op :make :clj-number) n))

(defn install-rational-package []
  (let [numer #(first %)
        denom #(second %)
        make-rat (fn [n d] (let [g (gcd n d)] (list (/ n g) (/ d g))))
        add-rat #(make-rat (+ (* (numer %1) (denom %2))
                              (* (numer %2) (denom %1)))
                           (* (denom %1) (denom %2)))
        sub-rat #(make-rat (- (* (numer %1) (denom %2))
                              (* (numer %2) (denom %1)))
                           (* (denom %1) (denom %2)))
        mul-rat #(make-rat (* (* (numer %1) (numer %2))
                              (* (denom %1) (denom %2))))
        div-rat #(make-rat (* (numer %1) (denom %2))
                           (* (denom %1) (denom %2)))
        tag #(attach-tag :rational %)]
    (put-op :add '(:rational :rational) #(tag (add-rat %1 %2)))
    (put-op :sub '(:rational :rational) #(tag (sub-rat %1 %2)))
    (put-op :mul '(:rational :rational) #(tag (mul-rat %1 %2)))
    (put-op :div '(:rational :rational) #(tag (div-rat %1 %2)))
    (put-op :make '(:rational) #(tag (make-rat %1 %2)))))

(defn make-rational [n d]
  ((get-op :make :rational) n d))

(defn real-part [z]
  (apply-generic :real-part z))

(defn imag-part [z]
  (apply-generic :imag-part z))

(defn magnitude [z]
  (apply-generic :magnitude z))

(defn angle [z]
  (apply-generic :angle z))


(defn real-part-rectangular [z] (first z))

(defn imag-part-rectangular [z] (second z))

(defn magnitude-rectangular [z]
  (Math/sqrt (+ (square (real-part-rectangular z))
                (square (imag-part-rectangular z)))))

(defn angle-rectangular [z]
  (Math/atan2 (imag-part-rectangular z)
              (real-part-rectangular z)))

(defn make-from-real-imag-rectangular [real imag]
  (cons real imag))

(defn make-from-mag-ang-rectangular [mag ang]
  (cons (* mag (Math/cos ang)) (* mag (Math/sin ang))))

(defn install-rectangular-package []
  (letfn [(real-part [z] (first z))
          (imag-part [z] (second z))
          (make-from-real-imag [real imag] (list real imag))
          (magnitude [z] (Math/sqrt (+ (square (real-part z))
                                       (square (imag-part z)))))
          (angle [z] (Math/atan2 (imag-part-rectangular z)
                                 (real-part-rectangular z)))
          (make-from-mag-ang [mag ang] (cons (* mag (Math/cos ang))
                                             (* mag (Math/sin ang))))
          (tag [x] (attach-tag :rectangular x))]
    (put-op :real-part '(:rectangular) real-part)
    (put-op :imag-part '(:rectangular) imag-part)
    (put-op :magnitude '(:rectangular) magnitude)
    (put-op :angle '(:rectangular) angle)
    (put-op :make-from-real-imag '(:rectangular)
            (fn [x y] (tag (make-from-real-imag x y))))
    (put-op :make-from-mag-ang '(:rectangular)
            (fn [r a] (tag (make-from-mag-ang r a))))
    :done))

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
          (tag [x] (attach-tag :polar x))]
    (put-op :real-part :polar real-part)
    (put-op :imag-part :polar imag-part)
    (put-op :magnitude :polar magnitude)
    (put-op :angle :polar angle)
    (put-op :make-from-real-imag '(:polar)
            (fn [x y] (tag (make-from-real-imag x y))))
    (put-op :make-from-mag-ang :polar (fn [r a] (tag (make-from-mag-ang r a))))
    :done))


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




    
        
;;;        
;;; test a little
;;;
    
(install-clj-number-package)
(def just-four (make-clj-number 4))
(def just-five (make-clj-number 5))
(def just-nine (make-clj-number 9))

(deftest add-clj-numbers
  (testing "4+5 == 9"
    (is (= (add just-four just-five)
           just-nine))))

;;
;; Do an exercise
;;

;; Exercise 2.77

(install-rectangular-package)
(install-complex-package)

(def z (make-complex-from-real-imag 3 4))

(deftest real-imag-constructed
  (testing "make-complex-from-real-imag creates correctly"
    (is (= z
           '(:complex (:rectangular (3 4)))))))

(deftest real-imag-constructed
  (testing "make-complex-from-real-imag creates correctly"
    (is (= 5.0
           (magnitude z)))))

;; this works because the ultimate data gets unwrapped one step at a
;; time, from '(:complex (:rectangular (3 4)))))), to (:rectangular (3
;; 4))


;;
;; test a little
;;


(def three-plus-4-i (make-complex-from-real-imag 3 4))

