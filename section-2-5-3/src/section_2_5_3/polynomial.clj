(ns section-2-5-3.polynomial
  (:gen-class)
  (:require [section-2-5-3.math :refer :all]
            [section-2-5-3.log :refer :all]
            [section-2-5-3.module :refer :all]))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TERM REPRESENTATION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn make-term [order coeff] (list order coeff))

(defn order [term] (first term))

(defn coeff [term] 
  (do
    (log "Calling coeff on" term)
    (second term)))

(defn valid-term? [term]
  (let [ret (and 
             (= 2 (count term))
             (number? (order term))
             (sicp-number? (coeff term)))]
    (log "(valid-term? " term ") = " ret)
    ret))

(defn negate-term [term] 
  {:pre [(valid-term? term)]}
  (do
    (log "Starting negate-term on " term)
    (let [o (order term)
          c (coeff term)
          nc (negate c)]
      (log "negate-term called with " term ", o=" o ", c=" c 
           ", nc=" nc)
      (make-term o nc))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GENERIC TERMLIST FUNCTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn empty-termlist? [term-list] 
  (log "(empty-termlist? " term-list ")")
  (apply-generic-no-simplify :empty-termlist? term-list))
(defn first-term [term-list] (apply-generic-no-simplify :first-term term-list))
(defn rest-terms [term-list] (apply-generic-no-simplify :rest-terms term-list))
(defn adjoin-term [term term-list]
  ((get-op-or-fail :adjoin-term (type-tag term-list)) term (contents term-list)))
;; XXX how do I as a consumer pick between representations?

(defn valid-termlist? [term-list]
  {:pre [(keyword? (first term-list))]}
  (log "(valid-termlist?" term-list ")")
  (let [ret (or (empty-termlist? term-list)
                (and 
                 (valid-term? (first-term term-list))
                 (valid-termlist? (rest-terms term-list))))]
    (log "(valid-termlist? " term-list ") = " ret)
    ret))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DENSE TERMLIST MODULE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn install-dense-termlist-package []
  (letfn [(empty-termlist? [term-list] (empty? term-list))
          (expand-to-order [new-order term-list]
            (log "(expand-to-order " new-order term-list ")")
            (if (and (not (empty-termlist? term-list))
                     (>= (order (first-term term-list)) new-order))
              term-list
              (recur new-order (cons 0 term-list))))
          (adjoin-term [term term-list] 
            {:pre [(valid-term? term)
                   (valid-termlist? (tag term-list))]}
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
            {:pre [(valid-termlist? (tag term-list))]
             :post [#(valid-termlist? (tag %))]}
            (rest term-list))
          (tag [p] (attach-tag :dense-termlist p))]
    (put-op :the-empty-termlist :dense-termlist #(tag (the-empty-termlist)))
    (put-op :empty-termlist? '(:dense-termlist) #(empty-termlist? %))
    (put-op :valid-termlist? '(:dense-termlist) #(valid-termlist? %))
    (put-op :first-term '(:dense-termlist) #(first-term %))
    (put-op :rest-terms '(:dense-termlist) #(tag (rest-terms %)))
    (put-op :adjoin-term :dense-termlist #(tag (adjoin-term %1 %2))))
  (defn the-empty-termlist [] ((get-op-or-fail :the-empty-termlist :dense-termlist))))

  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SPARSE TERMLIST MODULE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn install-sparse-termlist-package []
  (letfn [(empty-termlist? [term-list] (empty? term-list))          
          (adjoin-term [term term-list] 
            (log "Calling adjoin-term on (" term "), with (" term-list ")")
            (if (=zero? (coeff term))
              (do
                (log "coefficient was zero - skipping!")
                term-list)
              (do
                (log "Turns out that " (coeff term) "-" (class (coeff term)) 
                     "-is not zero")
                (cond
                 (empty-termlist? term-list)
                 (list term)

                 (< (order term) (order (first-term term-list)))
                 (cons (first-term term-list) (adjoin-term term (rest-terms term-list)))

                 (> (order term) (order (first-term term-list)))
                 (cons term term-list)

                 :else
                 (cons (make-term (order term)
                                  (add (coeff term) (coeff (first-term term-list))))
                       (rest-terms term-list))))))
            
          (the-empty-termlist [] '())
          (first-term [term-list] (first term-list))
          (rest-terms [term-list]
            {:post [#(valid-termlist? (tag %))]}
            (rest term-list))
          (tag [p] (attach-tag :sparse-termlist p))]
    (put-op :the-empty-termlist :sparse-termlist #(tag (the-empty-termlist)))
    (put-op :empty-termlist? '(:sparse-termlist) #(empty-termlist? %))
    (put-op :valid-termlist? '(:sparse-termlist) #(valid-termlist? %))
    (put-op :first-term '(:sparse-termlist) #(first-term %))
    (put-op :rest-terms '(:sparse-termlist) #(tag (rest-terms %)))
    (put-op :adjoin-term :sparse-termlist #(tag (adjoin-term %1 %2))))
  (defn the-empty-termlist [] ((get-op-or-fail :the-empty-termlist :sparse-termlist))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; POLYNOMIAL MODULE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn install-polynomial-package []
  (letfn [;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; POLYNOMIAL REPRESENTATION
          (make-poly [variable term-list] (cons variable term-list))
          (variable [p] (first p))
          (term-list [p] 
            {:post [(valid-termlist? %)]}
            (let [ret (rest p)]
              (log "(term-list " p ") = " ret)
              ret))
          (variable? [x] (keyword? x))
          (same-variable? [v1 v2] (and (variable? v1) (variable? v2)
                                       (or (= v1 :any)
                                           (= v2 :any)
                                           (= v1 v2))))
          (valid-poly? [poly]
            (log "(valid-poly?" poly ")")
            (valid-termlist? (term-list poly)))
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
    (put-op :=zero? '(:polynomial) #(empty-termlist? (term-list %1)))
    :done))

(defn make-polynomial [var terms]
  ((get-op-or-fail :make :polynomial) var terms))


