(ns section-2-5-3.polynomial
  (:gen-class)
  (:require [section-2-5-3.math :refer :all]
            [section-2-5-3.log :refer :all]
            [section-2-5-3.module :refer :all]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Polynomial module
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn install-polynomial-package []
  
  (letfn [;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; POLYNOMIAL REPRESENTATION
          (make-poly [variable term-list] (cons variable term-list))
          (variable [p] (first p))
          (term-list [p] 
            {:post [(valid-termlist? %)]}
            (rest p))
          (variable? [x] (keyword? x))
          (same-variable? [v1 v2] (and (variable? v1) (variable? v2)
                                       (or (= v1 :any)
                                           (= v2 :any)
                                           (= v1 v2))))
          (valid-poly? [poly]
            (valid-termlist? (term-list poly)))
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; TERM REPRESENTATION
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          (make-term [order coeff] (list order coeff))
          (order [term] 
            (first term))
          (coeff [term] 
            (do
              (log "Calling coeff on" term)
              (second term)))
          (negate-term [term] 
            {:pre [(valid-term? term)]}
            (do
              (log "Starting negate-term on " term)
              (let [o (order term)
                    c (coeff term)
                    nc (negate c)]
                (log "negate-term called with " term ", o=" o ", c=" c 
                     ", nc=" nc)
                (make-term o nc))))
          (valid-term? [term]
            (let [ret (and 
                       (= 2 (count term))
                       (number? (order term))
                       (sicp-number? (coeff term)))]
              (log "(valid-term? " term ") = " ret)
              ret))
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; SPARSE TERM LIST REPRESENTATION
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;;
          ;; (adjoin-term [term term-list] 
          ;;   (log "Calling adjoin-term on (" term "), with (" term-list ")")
          ;;   (if (=zero? (coeff term))
          ;;     (do
          ;;       (log "coefficient was zero - skipping!")
          ;;       term-list)
          ;;     (do
          ;;       (log "Turns out that " (coeff term) "-" (class (coeff term)) 
          ;;            "-is not zero")
          ;;       (cons term term-list))))
          ;; (the-empty-termlist [] '())
          ;; (first-term [term-list] (first term-list))
          ;; (rest-terms [term-list] (rest term-list))
          ;; (empty-termlist? [term-list] (empty? term-list))
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; DENSE TERM LIST REPRESENTATION
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          (empty-termlist? [term-list] (empty? term-list))          
          (expand-to-order [new-order term-list]
            (log "(expand-to-order " new-order term-list ")")
            (if (and (not (empty-termlist? term-list))
                     (>= (order (first-term term-list)) new-order))
              term-list
              (recur new-order (cons 0 term-list))))
          (valid-termlist? [term-list]
            (log "(valid-termlist?" term-list ")")
            (let [ret (or (empty-termlist? term-list)
                          (and 
                           (valid-term? (first-term term-list))
                           (valid-termlist? (rest term-list))))]
              (log "(valid-termlist? " term-list ") = " ret)
              ret))
          (adjoin-term [term term-list] 
            {:pre [(valid-term? term)
                   (valid-termlist? term-list)]}
            (log "(adjoin-term " term " " term-list ")")
            (let [term-order (order term)
                  term-coeff (coeff term)
                  term-list-order (if (empty-termlist? term-list) 
                                    -1
                                    (order (first-term term-list)))]
              (log "term-order: " term-order)
              (log "term-coeff: " term-coeff)
              (log "term-list-order: " term-list-order)
              (cond

               (> term-order term-list-order)
               (recur term (expand-to-order (order term) term-list))
               
               (< term-order term-list-order)
               (let [ret (cons (first term-list) (adjoin-term term (rest term-list)))]
                 (log "(adjoin-term " term " " term-list ") = " ret)
                 ret)

               :else
               (let [first-in-term-list (first term-list)]
                 (log "first-in-term-list: " first-in-term-list)
                 (log "term-coeff: " term-coeff)
                 (let [new-term (add first-in-term-list term-coeff)]
                   (log "new-term: " new-term)
                   (let [ret (cons new-term (rest term-list))]
                     (log "(adjoin-term " term " " term-list ") =2 " ret)                     
                     ret))))))
          (the-empty-termlist [] '())
          (first-term [term-list] 
            (if (empty? term-list)
              (throw (Exception. "Empty term list!"))
              (let [ret (list (dec (count term-list))
                              (first term-list))]
                (log "(first-term " term-list ") = " ret)
                ret)))
          (rest-terms [term-list]
            {:pre [(valid-termlist? term-list)]
             :post [#(valid-termlist? %)]}
            (rest term-list))
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; operations on term lists
          (negate-term-list [term-list]
            (if (empty-termlist? term-list)
              term-list
              (adjoin-term (negate-term (first-term term-list)) (negate-term-list (rest-terms term-list)))))
          (mul-term-by-all-terms [t1 list1]
            (log "mul-term-by-all-terms")
            (if (empty-termlist? list1)
              (the-empty-termlist)
              (let [t2 (first-term list1)]
                (adjoin-term
                 (make-term (+ (order t1) (order t2))
                            (mul (coeff t1) (coeff t2)))
                 (mul-term-by-all-terms t1 (rest-terms list1))))))
          (mul-terms [list1 list2]
            {:pre [(valid-termlist? list1)
                   (valid-termlist? list2)]}
            (log "mul-terms")
            (if (empty-termlist? list1)
              (the-empty-termlist)
              (add-terms (mul-term-by-all-terms (first-term list1) list2)
                         (mul-terms (rest-terms list1) list2))))
          ;; operations on polynomials
          (add-poly [p1 p2]           
            {:pre [(same-variable? (variable p1) (variable p2))
                   (valid-poly? p1)
                   (valid-poly? p2)]}
            (make-poly (variable p1)
                       (add-terms (term-list p1)
                                  (term-list p2))))
          (mul-poly [p1 p2]
            {:pre [(same-variable? (variable p1) (variable p2))]}
            (make-poly (variable p1)
                       (mul-terms (term-list p1)
                                  (term-list p2))))
          (add-terms [list1 list2]
            {:pre [(valid-termlist? list1)
                   (valid-termlist? list2)]}
            (log "Calling add-terms on (" list1 "), and (" list2 ")")
            (cond (empty-termlist? list1) list2
                  (empty-termlist? list2) list1
                  :else
                  (do
                    (log "add-terms else")
                    (let [t1 (first-term list1)
                          t2 (first-term list2)]
                      (log "t1 is " t1)
                      (log "t2 is " t2)
                      (cond (> (order t1) (order t2))
                            (adjoin-term t1 (add-terms (rest-terms list1) list2))
                            
                            (< (order t1) (order t2))
                            (adjoin-term t2 (add-terms list1 (rest-terms list2)))

                            :else
                            (adjoin-term
                             (make-term (order t1)
                                        (add (coeff t1) (coeff t2)))
                             (add-terms (rest-terms list1)
                                        (rest-terms list2))))))))
          (tag [p] (attach-tag :polynomial p))]
    (put-op :add '(:polynomial :polynomial) #(tag (add-poly %1 %2)))
    (put-op :mul '(:polynomial :polynomial) #(tag (mul-poly %1 %2)))
    (put-op :negate '(:polynomial) #(tag (make-poly (variable %1) 
                                                    (negate-term-list (term-list %1)))))
    (put-op :lower-type :polynomial (fn [] :complex))
    (put-op :raise '(:complex) #(do
                                  (log "Trying to raise a complex type: " %1)
                                  (let [dropped-type (drop-type (attach-tag :complex %1))]
                                    (log "raise on complex turned complex back into " 
                                         dropped-type)
                                    (tag (make-poly :any (adjoin-term (list 0 dropped-type) (the-empty-termlist)))))))
    (put-op :make ':polynomial
            (with-meta 
              #(tag 
                (do
                  (log "make-polynomial variable is " %1)
                  (log "make-polynomial orig term list is " %2)
                  (let [adjoin-term-2 (fn [term-list term] 
                                        (adjoin-term term term-list))
                        adjoin-term-debug (fn [termlist term]
                                             (log "debug (adjoin-term " term termlist")")
                                             (adjoin-term term termlist))
                        tlist (reduce adjoin-term-debug (the-empty-termlist) %2)]
                    (log "make-polynomial xformed term list is " tlist)
                    (let [ret (make-poly %1 tlist)]
                      (log "(make :polynomial) returning " ret)
                      ret))))
              {:post [#(valid-poly? %)]}))
    (put-op :=zero? '(:polynomial) #(= (order (term-list %1)) nil))
    :done))

(defn make-polynomial [var terms]
  ((get-op-or-fail :make :polynomial) var terms))
