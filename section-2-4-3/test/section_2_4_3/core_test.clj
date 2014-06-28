(ns section-2-4-3.core-test
  (:require [clojure.test :refer :all]
            [section-2-4-3.core :refer :all]))

;; Exercise 2.73

;; a: Because the operator function isn't smart enough to detect
;; whether something is a simple number.  We chose to put that
;; complexity into deriv instead of operator and operands.

;; b:

;;
;; dispatch methods
;;

(def operations (atom {}))

(defn get-op [op-sym type-tag]
  (let [op (get @operations (list op-sym type-tag))]
    (if op
      op
      (throw (IllegalArgumentException.
              (str "No function registered for " op-sym " with tags "
                   type-tag))))))

(defn put-op [op-sym type-tag op-fn]
  (swap! operations assoc (list op-sym type-tag) op-fn))

(defn operator [exp] (first exp))

(defn operands [exp] (rest exp))


;;
;; dispatch-oriented symbolic differentiation
;;


(defn variable? [x] (symbol? x))

(defn same-variable? [v1 v2]
  (and (variable? v1) (variable? v2) (= v1 v2)))

(defn deriv [exp var]
  (cond (number? exp) 0
        (variable? exp) (if (same-variable? exp var) 1 0)
        :else ((get-op 'deriv (operator exp)) (operands exp) var)))

(defn =number? [exp num]
  (and (number? exp) (= exp num)))

(defn make-sum [a1 a2]
  (cond (=number? a1 0) a2
        (=number? a2 0) a1
        (and (number? a1) (number? a2)) (+ a1 a2)
        :else (list '+ a1 a2)))

(defn make-product [m1 m2]
  (cond (or (=number? m1 0) (=number? m2 0)) 0
        (=number? m1 1) m2
        (=number? m2 1) m1
        (and (number? m1) (number? m2)) (* m1 m2)
        :else (list '* m1 m2)))

(put-op 'deriv '*
        (fn [[multiplier multiplicand] var]
          (make-sum (make-product multiplier
                                  (deriv multiplicand var))
                    (make-product (deriv multiplier var)
                                  multiplicand))))


(put-op 'deriv '+
        (fn [[addend augend] var]
          (make-sum (deriv addend var)
                    (deriv augend var))))

(deftest complex-derivs
  (testing
    (is (= (deriv '(* (* x y) (+ x 3)) 'x) ;= (+ (* x y) (* y (+ x 3))), 'x)
           '(+ (* x y) (* y (+ x 3)))))))


;; c

(defn make-exponentiation [base exp]
  (cond (=number? exp 0) 1
        (=number? exp 1) base
        :else (list '** base exp)))

(put-op 'deriv '**
        (fn [[base exponent] var]
          (make-product
           (make-product exponent
                         (make-exponentiation base
                                              (make-sum exponent -1)))
           (deriv base var))))

(deftest exponent-derivs
  (testing
    (is (= (deriv '(** x 2) 'x)
           '(* 2 x)))))

;; d

;; we'd just change the calls to put-op to put things in the opposite
;; order as well

;; Exercise 2.74

;; a

(defn division-id [personnel-file]
  (first personnel-file))

(defn base-file [personnel-file]
  (second personnel-file))

(defn get-record [personnel-file]
  ((get-op 'get-record (division-id personnel-file))
   (base-file personnel-file)))

(put-op 'get-record 'division-a
        (fn [personnel-file] (:record personnel-file)))

(deftest get-record-test
  (let [division-a-record '(la la la)
        division-a-file (list 'division-a {:record division-a-record})]
    (testing
        (is (= (get-record division-a-file)
               '(la la la))))))

;; the records are structured as a tuple--first item is a division
;; identifier, second is whatever data that division stores.

;; type information is the division name as a symbol.

;; b

(defn get-salary [personnel-file]
  ((get-op 'get-salary (division-id personnel-file))
   (base-file personnel-file)))


(put-op 'get-salary 'division-a
        (fn [personnel-file] (:salary personnel-file)))

(deftest get-salary-test
  (let [salary 1000000
        division-a-record '(la la la)
        division-a-file (list 'division-a {:record division-a-record
                                           :salary salary})]
    (testing
        (is (= (get-salary division-a-file)
               salary)))))

;; same as above, and then division-supplied

;; c

(defn get-name [personnel-file]
  ((get-op 'get-name (division-id personnel-file))
   (base-file personnel-file)))

(put-op 'get-name 'division-a
        (fn [personnel-file] (:name personnel-file)))

(defn find-employee-record [name files]
  (get-record (first (filter #(= name (get-name %)) files))))

(deftest find-employee-record-test
  (let [salary 1000000
        division-a-record '(la la la)
        name "Keyser Söze"
        division-a-file (list 'division-a {:name name
                                           :record division-a-record
                                           :salary salary})]
    (testing
        (is (= (find-employee-record name (list division-a-file))
               division-a-record)))))

;; d.

;; calls to put-op with 'get-name, 'get-salary and 'get-record should
;; be created

;; Message passing

(defn square [x] (* x x))

(defn make-from-real-imag [x y]
  (fn [op]
    (case op
      :real-part x
      :imag-part y
      :magnitude (Math/sqrt (+ (square x) (square y)))
      :angle (Math/atan2 y x)
      :else
      (throw (IllegalArgumentException.
              (str "Didn't know how to handle " op))))))

(defn apply-generic [op arg]
  (arg op))

(defn real-part [x]
  (apply-generic :real-part x))

(defn magnitude [x]
  (apply-generic :magnitude x))

(defn angle [x]
  (apply-generic :angle x))

(defn imag-part [x]
  (apply-generic :imag-part x))

(deftest make-from-real-imag-test
  (testing
      (let [real 5
            imag 2]
        (is (= (real-part (make-from-real-imag real imag))
               real)))))

;; Exercise 2.75

(defn make-from-mag-ang [r a]
   (fn [op]
     (case op
       :real-part (* r (Math/cos a))
       :imag-part (* r (Math/sin a))
       :magnitude r
       :angle a
       :else
       (throw (IllegalArgumentException.
               (str "Didn't know how to handle " op))))))

(deftest make-from-mag-ang-test
  (let [mag 5
        ang 2
        polar (make-from-mag-ang mag ang)]
    (is (= (magnitude polar) mag))
    (is (= (angle polar) ang))
    (is (= (real-part polar) (* mag (Math/cos ang))))
    (is (= (imag-part polar) (* mag (Math/sin ang))))))

;; Exercise 2.76

;; three styles:
;; a. Generic operations with explicit dispatch
;; b. Data-directed style
;; c. message-passing style

;; where new types must often be added - message-passing style--keeps
;; all type information cleanly separate from each other.

;; new operations must often be added - data-direced style
