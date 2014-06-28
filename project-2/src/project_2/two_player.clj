(ns project-2.two-player
  (:use project-2.core))

;; How to manage history of plays

(def game-association-list
  ;; format is that first sublist identifies the players' choices
  ;; with "c" for cooperate and "d" for defect; and that second sublist
  ;; specifies payout for each player
  '((("c" "c") (3 3))
    (("c" "d") (0 5))
    (("d" "c") (5 0))
    (("d" "d") (1 1))))


;; Problem 1:

;; see Core/extract-entry



;; game
(defn make-play [move0 move1] (list move0 move1))

(defn get-point-list [game]
  (second (extract-entry game game-association-list)))

(defn get-player-points [num game]
  (nth (get-point-list game) num))

(defn get-scores [history0 history1]
  "Given two histories, determine scores of two players as a list"
  (loop [history0 history0 history1 history1 score0 0 score1 0]
    (if (empty-history? history0) (list score0 score1)
        (let [game (make-play (most-recent-play history0)
                              (most-recent-play history1))]
          (recur (rest-of-plays history0)
                 (rest-of-plays history1)
                 (+ (get-player-points 0 game) score0)
                 (+ (get-player-points 1 game) score1))))))

(defn print-out-results [history0 history1 number-of-games]
  (let [scores (get-scores history0 history1)]
    (newline)
    (println "Player 1 Score:  ")
    (println (* 1.0 (/ (first scores) number-of-games)))
    (newline)
    (println "Player 2 Score:  ")
    (println (* 1.0 (/ (second scores) number-of-games)))
    (newline)))

(defn play-loop [strat0 strat1]
  "The play-loop procedure takes as its arguments two prisoner's"
  "dilemma strategies, and plays an iterated game of approximately"
  "one hundred rounds.  A strategy is a procedure that takes"
  "two arguments: a history of the player's previous plays and"
  "a history of the other player's previous plays.  The procedure"
  "returns either a \"c\" for cooperate or a \"d\" for defect."
  (loop [strat0 strat0
         strat1 strat1
         count 0
         history0 the-empty-history
         history1 the-empty-history
         limit (+ 90 (rand-int 21))]
    (if (= count limit) (print-out-results history0 history1 limit)
        (let [result0 (strat0 history0 history1)
              result1 (strat1 history1 history0)]
          (recur strat0
                 strat1
                 (inc count)
                 (extend-history result0 history0)
                 (extend-history result1 history1)
                 limit)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  The following procedures are used to compute and print
;;  out the players' scores at the end of an iterated game
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A sampler of strategies

(defn NASTY [my-history other-history]
  "d")

(defn PATSY [my-history other-history]
  "c")

(defn SPASTIC [my-history other-history]
  (if (= (rand-int 2) 0)
      "c"
      "d"))

(defn EGALITARIAN [my-history other-history]
  (let [count-instances-of
        (fn count-instances-of [test hist]
          (cond (empty-history? hist)
                0

                (= (most-recent-play hist) test)
                (+ (count-instances-of test (rest-of-plays hist)) 1)

                :else
                (count-instances-of test (rest-of-plays hist))))
        ds (count-instances-of "d" other-history)
        cs (count-instances-of "c" other-history)]
    (if (> ds cs) "d" "c")))

(defn EYE-FOR-EYE [my-history other-history]
  (if (empty-history? my-history)
      "c"
      (most-recent-play other-history)))

; (play-loop SPASTIC PATSY)
;; Player 1 Score:
;; 3.903846153846154

;; Player 2 Score:
;;1.644230769230769

;; nil

; (play-loop SPASTIC EYE-FOR-EYE)

;; Player 1 Score:
;; 2.345454545454545

;; Player 2 Score:
;; 2.3

;; nil

;; eye-for-eye will generally tie score

; (play-loop SPASTIC NASTY)

;; Player 1 Score:
;; 0.4736842105263158

;; Player 2 Score:
;; 3.105263157894737

;; nil

;; Spastic loses against someone who always defects.


;; Problem 3

;; Egalitarian needs to view the whole history O(n) to determine its next move.

(defn EGALITARIAN-FASTER [_my-history other-history]
  (loop [cs 0 ds 0 hist other-history]
    (cond (empty-history? hist)
          (if (> ds cs) "d" "c")

          (= (most-recent-play hist) "c")
          (recur (+ 1 cs) ds (rest-of-plays hist))

          :else
          (recur cs (+ 1 ds) (rest-of-plays hist)))))

;; they are both O(n) in time, but EGALITARIAN-FASTER is O(1) in space
;; as it is tail recursive.

;; Problem 4

(defn EYE-FOR-TWO-EYES [my-history other-history]
  "The strategy should always cooperate unless the opponent defected
   on both of the previous two rounds"
  (cond (empty-history? other-history) "c"
        (empty-history? (rest-of-plays other-history)) "c"
        :else
        (let [last (most-recent-play other-history)
              second-last (most-recent-play (rest-of-plays other-history))]
          (if (= "d" last second-last)
            "d"
            "c"))))


; (play-loop SPASTIC EYE-FOR-TWO-EYES)

;; Player 1 Score:
;; 3.117021276595745

;; Player 2 Score:
;; 2.053191489361702

;; nil
; (play-loop EGALITARIAN EYE-FOR-TWO-EYES)
;; Player 1 Score:
;; 3.0

;; Player 2 Score:
;; 3.0

;; nil

; (play-loop EYE-FOR-EYE EYE-FOR-TWO-EYES)
;; Player 1 Score:
;; 3.0

;; Player 2 Score:
;; 3.0

;; nil

; (play-loop EYE-FOR-TWO-EYES NASTY)

;; Player 1 Score:
;; 0.981651376146789

;; Player 2 Score:
;; 1.073394495412844

;; nil

;; Problem 5

(defn- history-length-of-n? [n my-history]
  (= n (history-length my-history)))

(defn- most-recent-n-are-defections [n my-history]
  (if (= n 0) true
      (if (= "d" (most-recent-play my-history))
        (recur (dec n) (rest-of-plays my-history))
        false)))

(defn make-eye-for-n-eyes [n]
  (fn [my-history other-history]
    (if (and (history-length-of-n? n my-history)
             (most-recent-n-are-defections n my-history))
      "d"
      "c")))

(def EYE-FOR-FIVE-EYES (make-eye-for-n-eyes 5))

; (play-loop SPASTIC EYE-FOR-FIVE-EYES)

;; Player 1 Score:
;; 3.84

;; Player 2 Score:
;; 1.74

;; nil

; (play-loop EGALITARIAN EYE-FOR-FIVE-EYES)

;; Player 1 Score:
;; 3.0

;; Player 2 Score:
;; 3.0

;; nil

; (play-loop EYE-FOR-EYE EYE-FOR-FIVE-EYES)

;; Player 1 Score:
;; 3.0

;; Player 2 Score:
;; 3.0

;; nil


; (play-loop EYE-FOR-FIVE-EYES NASTY)

;; Player 1 Score:
;; 0.0

;; Player 2 Score:
;; 5.0

;; nil

;; Problem 6

(defn make-rotating-strategy [strat0 strat1 freq0 freq1]
  "freq0 and freq1 are integers."
  (let [total-freq (+ freq0 freq1)]
    (fn [my-history other-history]
      (let [num-rounds-so-far (history-length my-history)
            point-in-cycle (rem num-rounds-so-far total-freq)]
        (if (< point-in-cycle freq0)
          (strat0 my-history other-history)
          (strat1 my-history other-history))))))

(def EYE-THEN-EGAL (make-rotating-strategy EYE-FOR-EYE EGALITARIAN 3 3))

; (play-loop EYE-THEN-EGAL NASTY)

;; Player 1 Score:  
;; 0.9907407407407407

;; Player 2 Score:  
;; 1.037037037037037

;; nil

; (play-loop EYE-THEN-EGAL SPASTIC)

;; Player 1 Score:  
;; 2.432989690721649

;; Player 2 Score:  
;; 1.65979381443299

;; nil

;; Player 1 Score:  
;; 2.021276595744681

;; Player 2 Score:  
;; 2.872340425531915

;; nil

;; Problem 7

(defn- rotating-nth [coll index]
  (let [my-vec (vec coll)
        vec-size (count my-vec)
        rotating-index (mod index vec-size)]
    (nth my-vec rotating-index)))

(rotating-nth '(1 2 3) 999)
(rotating-nth [1 2 3] 8)

(defn make-higher-order-spastic [strategies]
  (fn [my-history other-history]
    (let [next-strategy-fn (rotating-nth strategies (count my-history))]
      (next-strategy-fn my-history other-history))))

;; Problem 8

(defn gentle [strat gentleness-factor]
  (fn [my-history other-history]
    (let [natural-result (strat my-history other-history)]
      (if (not= natural-result "d")
        natural-result
        (let [be-gentle? (< (rand) gentleness-factor)]
          (if be-gentle? "c" "d"))))))

