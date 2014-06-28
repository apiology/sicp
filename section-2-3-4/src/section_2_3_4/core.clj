(ns section-2-3-4.core)

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))

;; Generating a Huffman tree

;; Begin with set of all leaf nodes (symbols and their frequencies).

;; Find two leaves with lowest weights and merge together to produce a
;; node with two nodes as left and right branches.

;; Remove two leaves from original set and replace with new node.

;; Continue process.  At each step, merge two nodes with the smallest
;; weights, removing them from the set and replacing them with a node
;; that has these two as its left and right branches.

;; The process stops when there is only one node left, which is the
;; root of the entire tree.

(defn make-leaf [symbol weight]
  (list 'leaf symbol weight))

(defn leaf? [object]
  (= (first object) 'leaf))

(defn third [x] (nth x 2))

(defn fourth [x] (nth x 3))

(defn symbol-leaf [x] (second x))

(defn weight-leaf [x] (third x))

(defn symbols [tree] (if (leaf? tree)
                       (list (symbol-leaf tree))
                       (third tree)))

(defn weight [tree]
  (if (leaf? tree)
    (weight-leaf tree)
    (fourth tree)))

(defn make-code-tree [left right]
  (list left
        right
        (concat (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(defn left-branch [tree] (first tree))
(defn right-branch [tree] (second tree))

(defn choose-branch [bit branch]
  (cond (= bit 0) (left-branch branch)
        (= bit 1) (right-branch branch)
        :else (throw (IllegalArgumentException.
                      (str "bad bit -- CHOOSE-BRANCH" bit)))))

(defn decode [bits tree]
  (defn- decode-1 [bits current-branch]
    (if (empty? bits)
      '()
      (let [next-branch (choose-branch (first bits) current-branch)]
        (if (leaf? next-branch)
          (cons (symbol-leaf next-branch)
                (decode-1 (rest bits) tree))
          (decode-1 (rest bits) next-branch)))))
  (decode-1 bits tree))

(defn adjoin-set [x set]
  (cond (empty? set) (list x)
        (< (weight x) (weight (first set))) (cons x set)
        :else (cons (first set)
                    (adjoin-set x (rest set)))))

(defn make-leaf-set [pairs]
  (if (empty? pairs)
    '()
    (let [pair (first pairs)]
      (adjoin-set (make-leaf (first pair)
                             (second pair))
                  (make-leaf-set (rest pairs))))))

(def sample-tree (make-code-tree (make-leaf 'A 4)
                                 (make-code-tree
                                  (make-leaf 'B 2)
                                  (make-code-tree (make-leaf 'D 1)
                                                  (make-leaf 'C 1)))))
sample-tree
(def sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(decode sample-message sample-tree)
;= (A D A B B C A)

;; Exercise 2.68

(defn in-branch? [tree symbol]
  (some #{symbol} (symbols tree)))

(defn encode-symbol [symbol tree]
  (loop [tree tree bits '()]
    (cond (leaf? tree) (if (= (symbol-leaf tree) symbol)
                         bits
                         (throw (IllegalArgumentException.
                                 (str "could not find symbol in tree--"
                                      symbol))))
          (in-branch? (left-branch tree) symbol) (recur (left-branch tree)
                                                        (concat bits '(0)))
          :else (recur (right-branch tree) (concat bits '(1))))))

(defn encode [message tree]
  (if (empty? message)
    '()
    (concat (encode-symbol (first message) tree)
            (encode (rest message) tree))))

(encode-symbol 'A sample-tree)

(encode-symbol 'D sample-tree)
(in-branch? (left-branch sample-tree) 'D)

(def my-encoded-message
  (encode (decode sample-message sample-tree) sample-tree))

my-encoded-message

(= sample-message my-encoded-message) ;= true

;; Exercise 2.69

(defn successive-merge [leaf-set]
  (println (str "successive-merge " leaf-set))
  (if (empty? (rest leaf-set)) (first leaf-set)
      (recur (adjoin-set (make-code-tree (second leaf-set)
                                         (first leaf-set))
                         (rest (rest leaf-set))))))

(defn generate-huffman-tree [pairs]
  (successive-merge (make-leaf-set pairs)))

; Exercise 2.70

(def song-frequencies '((A 2) (BOOM 1) (GET 2) (JOB 2)
                                      (NA 16) (SHA 3) (YIP 9) (WAH 1)))

(make-leaf-set song-frequencies)
;= ((leaf WAH 1) (leaf BOOM 1) (leaf JOB 2) (leaf GET 2) (leaf A 2)
;  (leaf SHA 3) (leaf YIP 9) (leaf NA 16))



(def song-tree (generate-huffman-tree song-frequencies))

song-tree

;= ((((((leaf GET 2) (leaf JOB 2) (GET JOB) 4) (leaf SHA 3) (GET JOB
;        SHA) 7) (((leaf BOOM 1) (leaf WAH 1) (BOOM WAH) 2) (leaf A 2)
;        (BOOM WAH A) 4) (GET JOB SHA BOOM WAH A) 11) (leaf YIP 9)
;        (GET JOB SHA BOOM WAH A YIP) 20) (leaf NA 16) (GET JOB SHA
;        BOOM WAH A YIP NA) 36)

(encode-symbol 'YIP song-tree) ;= (0 1)

(encode-symbol 'NA song-tree) ;= (1)

(encode-symbol 'WAH song-tree) ;= (0 0 1 0 1)

(def song '(GET A JOB SHA NA NA NA NA NA NA NA NA
            GET A JOB SHA NA NA NA NA NA NA NA NA
            WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP
            SHA BOOM))

(def encoded-song (encode song song-tree))
encoded-song

;= (0 0 0 0 0 0 0 1 1 0 0 0 0 1 0 0 0 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0
;1 1 0 0 0 0 1 0 0 0 1 1 1 1 1 1 1 1 1 0 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0
;1 0 1 0 1 0 1 0 0 0 1 0 0 1 0 0)

(count encoded-song) ;= 84

(count song) ;= 38
;; for fixed-symbol alphabet, 3 bits per symbol = 38*3 = 114 bits

;; Exercise 2.71


(def frequencies '((ONE 1) (TWO 2) (FOUR 4) (EIGHT 8) (SIXTEEN 16)))

(frequencies )
;; most frequent symbol takes 1 bit.  least frequent symbol takes n-1 bits.

;; Exercise 2.72

; O(n^2) to search number of symbols.
