(ns section-2-3-3.core)

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))

(def myset1 '(1 2 3 4 5))
(def myset2 '(4 5 6 7 8))


(defn element-of-set? [x set]
  "Predicate that determines whether a given element is a member of a
  set"
  (cond (empty? set) false
        (= x (peek set)) true
        :else (element-of-set? x (pop set))))

(element-of-set? 3 myset1) ;= true
(element-of-set? 8 myset1) ;= false

(defn adjoin-set [x set]
  "Takes an object and a set as arguments and returns a set that
  contains the elements of the original set and also the adjoined
  element"
  (if (element-of-set? x set)
    set
    (conj set x)))

(adjoin-set 1 myset1) ;= (1 2 3 4 5)
(adjoin-set 6 myset1) ;= (6 1 2 3 4 5)
(element-of-set? 6 myset1) ;= false

(defn intersection-set [set1 set2]
  "Computes the intersection of two sets, which is the set containing
  each element that appears in both arguments"
  (cond (or (empty? set1) (empty? set2))
        (empty set1)

        (element-of-set? (peek set1) set2)
        (conj (intersection-set (pop set1) set2) (peek set1))

        :else
        (intersection-set (pop set1) set2)))

(intersection-set myset1 myset2) ;= (4 5)
(intersection-set myset1 myset1) ;= (1 2 3 4 5)
(intersection-set myset1 '(99 100)) ;= ()


;; Exercise 2.59
(defn union-set [set1 set2]
  "computes the union of two sets, which is the set containing each
  elements that appears in either argument"
  (loop [ret (list)
         sets (rest (concat set1 set2))
         init (first (concat set1 set2))]
    (cond
     (empty? sets)
     ret

     (element-of-set? init ret)
     (recur ret (rest sets) (first sets))

     :else
     (recur (conj ret init) (rest sets) (first sets)))))

(union-set myset1 myset1) ;= (5 4 3 2 1)
(union-set myset1 myset2) ;= (7 6 5 4 3 2 1)
(first (concat myset1 myset1)) ;= 1
(rest (concat myset1 myset1))


;; Exercise 2.60

;; now as lists that allow duplicates.

;; same, but constant factor can increase depending on how many other
;; operations have increased 'dirtiness'
(defn element-of-set? [x set]
  "Predicate that determines whether a given element is a member of a
  set"
  (cond (empty? set) false
        (= x (peek set)) true
        :else (element-of-set? x (pop set))))

(element-of-set? 3 myset1) ;= true
(element-of-set? 8 myset1) ;= false

;; now O(1)
(defn adjoin-set [x set]
  "Takes an object and a set as arguments and returns a set that
  contains the elements of the original set and also the adjoined
  element"
  (conj set x))

(adjoin-set 1 myset1) ;= (1 2 3 4 5)
(adjoin-set 6 myset1) ;= (6 1 2 3 4 5)
(element-of-set? 6 myset1) ;= false

;; same
(defn intersection-set [set1 set2]
  "Computes the intersection of two sets, which is the set containing
  each element that appears in both arguments"
  (cond (or (empty? set1) (empty? set2))
        (empty set1)

        (element-of-set? (peek set1) set2)
        (conj (intersection-set (pop set1) set2) (peek set1))

        :else
        (intersection-set (pop set1) set2)))

(intersection-set myset1 myset2) ;= (4 5)
(intersection-set myset1 myset1) ;= (1 2 3 4 5)
(intersection-set myset1 '(99 100)) ;= ()


;; O(1)
(defn union-set [set1 set2]
  "computes the union of two sets, which is the set containing each
  elements that appears in either argument"
  (concat set1 set2))

;; Useful if you do a lot more adjoining than set lookupl.

;; Sets as ordered lists

(def myset1 '(1 2 3 4 5))
(def myset2 '(4 5 6 7 8))

(defn element-of-set? [x set]
  (cond (empty? set) false
        (= x (first set)) true
        (< x (first set)) false
        :else (element-of-set? x (rest set))))

(element-of-set? 3 myset1) ;= true
(element-of-set? 8 myset1) ;= false

;; Now O(n)
(defn intersection-set [set1 set2]
  (if (or (empty? set1) (empty? set2))
    '()
    (let [x1 (first set1) x2 (first set2)]
      (cond (= x1 x2) (cons x1 (intersection-set (rest set1) (rest set2)))
            (< x1 x2) (recur (rest set1) set2)
            (< x2 x1) (recur set1 (rest set2))))))

(intersection-set myset1 myset2) ;= (4 5)
(intersection-set myset1 myset1) ;= (1 2 3 4 5)
(intersection-set myset1 '(99 100)) ;= ()

;; Exercise 2.61

(defn adjoin-set [x set]
  "Takes an object and a set as arguments and returns a set that
  contains the elements of the original set and also the adjoined
  element"
  (if (empty? set)
    (list x)
    (let [firstinset (first set)]
      (cond (= x firstinset) set
            (< x firstinset) (cons x set)
            (> x firstinset) (cons (first set) (adjoin-set x (rest set)))))))


(adjoin-set 1 myset1) ;= (1 2 3 4 5)
(adjoin-set 6 myset1) ;= (1 2 3 4 5 6)
(element-of-set? 6 myset1) ;= false

(defn union-set [set1 set2]
  "computes the union of two sets, which is the set containing each
  elements that appears in either argument"
  (cond (empty? set1) set2
        (empty? set2) set1
        :else
        (let [first1 (first set1) first2 (first set2)]
          (cond (= first1 first2)
                (cons first1 (union-set (rest set1) (rest set2)))

                (< first1 first2) (cons first1 (union-set (rest set1) set2))
                (< first2 first1) (cons first2 (union-set set1 (rest set2)))))))

(union-set myset1 myset1) ;= (1 2 3 4 5)
(union-set myset1 myset2) ;= (1 2 3 4 5 6 7 8)
(first (concat myset1 myset1)) ;= 1
(rest (concat myset1 myset1))

;; Sets as binary trees

(defn third [coll] (nth coll 2))
(defn entry [tree] (first tree))
(defn left-branch [tree] (second tree))
(defn right-branch [tree] (third tree))
(defn make-tree [entry left right]
  (list entry left right))

(def myset1 '(1 2 3 4 5))
(def myset1 '(3 (2 (1 nil nil) nil) (4 nil (5 nil nil))))
(def myset2 '(6 (4 () (5 () ())) (7 () (8 () ()))))

(defn element-of-set? [x set]
  (cond (or (nil? set) (empty? set)) false
        (= x (entry set)) true
        (< x (entry set)) (recur x (left-branch set))
        (> x (entry set)) (recur x (right-branch set))))

(entry myset1) ;= 3
(left-branch myset1) ;= (2 (1 nil nil) nil)
(right-branch myset1) ;= (4 nil (5 nil nil))
(element-of-set? 3 myset1) ;= true
(element-of-set? 8 myset1) ;= false


(defn adjoin-set [x set]
  (cond (empty? set) (make-tree x '() '())
        (= x (entry set)) set
        (< x (entry set)) (make-tree (entry set)
                                     (adjoin-set x (left-branch set))
                                     (right-branch set))
        (> x (entry set)) (make-tree (entry set)
                                     (left-branch set)
                                     (adjoin-set x (right-branch set)))))

(adjoin-set 1 myset1) ;= (3 (2 (1 nil nil) nil) (4 nil (5 nil nil)))
(adjoin-set 6 myset1) ;= (3 (2 (1 nil nil) nil) (4 nil (5 nil (6 () ()))))
(element-of-set? 6 myset1) ;= false

;; Exercise 2.63

(defn tree->list-1 [tree]
  (if (or (nil? tree) (empty? tree))
    '()
    (concat (tree->list-1 (left-branch tree))
            (cons (entry tree)
                  (tree->list-1 (right-branch tree))))))

(tree->list-1 myset1) ;= (1 2 3 4 5)
(tree->list-1 myset2) ;= (4 5 6 7 8)
(right-branch myset2)

(defn tree->list-2 [tree]
  (defn copy-to-list [tree result-list]
    (if (or (nil? tree) (empty? tree))
      result-list
      (recur (left-branch tree)
             (cons (entry tree)
                   (copy-to-list (right-branch tree)
                                 result-list)))))
  (copy-to-list tree '()))

(tree->list-2 myset1) ;= (1 2 3 4 5)
(tree->list-2 myset2) ;= (4 5 6 7 8)

(def book-example-1 '(7 (3 (1 () ()) (5 () ())) (9 () (11 () ()))))
(def book-example-2 '(3 (1 () ()) (7 (5 () ()) (9 () (11 () ())))))
(def book-example-3 '(5 (3 (1 () ()) ()) (9 (7 () ()) (11 () ()))))

(tree->list-1 book-example-1) ;= (1 3 5 7 9 11)
(tree->list-2 book-example-1) ;= (1 3 5 7 9 11)
(tree->list-1 book-example-2) ;= (1 3 5 7 9 11)
(tree->list-2 book-example-2) ;= (1 3 5 7 9 11)
(tree->list-1 book-example-3) ;= (1 3 5 7 9 11)
(tree->list-2 book-example-3) ;= (1 3 5 7 9 11)

;; second will be much more efficient, as it only travels down one
;; branch at a time.

;; Element 2.64

(defn partial-tree [elts n]
  (if (= n 0)
    (cons '() elts)
    (let [left-size (quot (dec n) 2)
          left-result (partial-tree elts left-size)
          left-tree (first left-result)
          non-left-elts (rest left-result)
          right-size (- n (inc left-size))
          this-entry (first non-left-elts)
          right-result (partial-tree (rest non-left-elts) right-size)
          right-tree (first right-result)
          remaining-elts (rest right-result)]
      (cons (make-tree this-entry left-tree right-tree)
            remaining-elts))))

(defn list->tree [elements]
  (first (partial-tree elements (count elements))))

(list->tree '(1 2 3 4 5)) ;= (3 (1 () (2 () ())) (4 () (5 () ())))



(defn union-set [set1 set2]
  "computes the union of two sets, which is the set containing each
  elements that appears in either argument"
  (cond (or (nil? set1) (empty? set1))
        set2

        (or (nil? set2) (empty? set2))
        set1

        (let [entry1 (entry set1)
              entry2 (entry set2)
              left1 (left-branch set1)
              left2 (left-branch set2)
              right1 (right-branch set1)
              right2 (right-branch set2)]

          (cond (= entry1 entry2)
                (make-tree
                 entry1
                 (union-set left1 left2)
                 (union-set right1 right2))

                (< entry1 entry2)
                (join-sets
                 (union-set left1-with-top left2))
                 (union-set right1 right2-with-top)

                (> entry1 entry2))
                (join-sets
                 (union-set left1-with-top left2))
                 (union-set right1 right2-with-top)
                 "BLAH!!!")))

(defn union-set [set1 set2]
  "computes the union of two sets, which is the set containing each
  elements that appears in either argument"
  (loop [list1 (tree->list-2 set1)
         list2 (tree->list-2 set2)
         retlist []]
    (cond
     (and (empty? list1) (empty? list2))
     (list->tree retlist)

     (empty? list1)
     (list->tree (vector (concat retlist list2)))

     (empty? list2)
     (list->tree (vector (concat retlist list1)))

     (= (first list1) (first list1))
     (recur (rest list1) (rest list2) (conj retlist (first list1)))

     (> (first list1) (first list2))
     (recur list1 (rest list2) (conj retlist (first list2)))

     (< (first list1) (first list2))
     (recur (rest list1) list2 (conj retlist (first list2))))))

(tree->list-2 (union-set book-example-1 book-example-2))


(defn intersection-set [set1 set2]
  "Computes the intersection of two sets, which is the set containing
  each element that appears in both arguments"
  (loop [list1 (tree->list-2 set1)
         list2 (tree->list-2 set2)
         retlist []]
    (cond
     (or (empty? list1) (empty? list2))
     (list->tree retlist)

     (= (first list1) (first list2))
     (recur (rest list1) (rest list2) (conj retlist (first list1)))

     (> (first list1) (first list2))
     (recur list1 (rest list2) retlist)

     (< (first list1) (first list2))
     (recur (rest list1) list2 retlist))))

(tree->list-2 (intersection-set book-example-1 book-example-2))
;= (1 3 5 7 9 11)

(tree->list-2 (intersection-set myset1 myset1))
;= (1 2 3 4 5)

(tree->list-2 (intersection-set myset1 myset2)) ;= (4 5)

;;;
;;; Sets and information retrieval
;;;

(defn lookup [given-key set-of-records]
  (cond (empty? set-of-records) false
        (equal? given-key (key (first set-of-records))) (first set-of-records)
        :else (lookup given-key (rest set-of-records))))

; Exercise 2.66

(defn lookup [given-key set-of-records]
  (let [root (entry set-of-records)
        rootkey (key root)]
    (cond (empty? set-of-records) false
          (= given-key rootkey)
          root

          (< given-key rootkey)
          (lookup given-key (left-branch set-of-records))

          (> given-key rootkey)
          (lookup given-key (right-branch set-of-records)))))


