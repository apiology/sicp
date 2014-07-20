(ns section-2-5-3.polynomial
  (:gen-class)
  (:require [section-2-5-3.math :refer :all]
            [section-2-5-3.log :refer :all]
            [section-2-5-3.module :refer :all]))

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
          (same-variable? [v1 v2] (and (variable? v1) (variable? v2)
                                       (or (= v1 :any)
                                           (= v2 :any)
                                           (= v1 v2))))
          (adjoin-term [term term-list] (if (=zero? (coeff term))
                                          term-list
                                          (cons term term-list)))
          (the-empty-termlist [] '())
          (first-term [term-list] (first term-list))
          (rest-terms [term-list] (rest term-list))
          (empty-termlist? [term-list] (empty? term-list))
          (make-term [order coeff] (list order coeff))
          (order [term] (first term))
          (coeff [term] (do
                          (log "Calling coeff on" term)
                          (second term)))
          (negate-term [term] (do
                                (log "Starting negate-term on " term)
                                (let [o (order term)
                                      c (coeff term)
                                      nc (negate c)]
                                  (log "negate-term called with " term ", o=" o ", c=" c ", nc=" nc)
                                  (make-term o nc))))
          (negate-term-list [term-list] (map negate-term term-list))
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
    (put-op :negate '(:polynomial) #(tag (make-poly (variable %1) (negate-term-list (term-list %1)))))
    (put-op :lower-type :polynomial (fn [] :complex))
    (put-op :raise '(:complex) #(do
                                  (log "Trying to raise a complex type: " %1)
                                  (let [dropped-type (drop-type (attach-tag :complex %1))]
                                    (log "raise on complex turned complex back into " 
                                         dropped-type)
                                    (tag (make-poly :any (list (list 0 dropped-type)))))))
    (put-op :make ':polynomial #(tag (make-poly %1 %2)))
    (put-op :=zero? '(:polynomial) #(= (order (term-list %1)) nil))
    :done))

(defn make-polynomial [var terms]
  ((get-op-or-fail :make :polynomial) var terms))
