(ns project-2.three-player
  (:use project-2.core)
  (:require project-2.two-player))

;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; code to use in 3 player game
;;

;(def *game-association-list*
;  (list (list (list "c" "c" "c") (list 4 4 4))
;        (list (list "c" "c" "d") (list 2 2 5))
;        (list (list "c" "d" "c") (list 2 5 2))
;        (list (list "d" "c" "c") (list 5 2 2))
;        (list (list "c" "d" "d") (list 0 3 3))
;        (list (list "d" "c" "d") (list 3 0 3))
;        (list (list "d" "d" "c") (list 3 3 0))
;        (list (list "d" "d" "d") (list 1 1 1))))


(def game-association-list
  ;; format is that first sublist identifies the players' choices
  ;; with "c" for cooperate and "d" for defect; and that second sublist
  ;; specifies payout for each player
  '((("c" "c" "c") (4 4 4))
;  (list (list (list "c" "c" "c") (list 4 4 4))
    (("c" "c" "d") (2 2 5))
;        (list (list "c" "c" "d") (list 2 2 5))
    (("c" "d" "c") (2 5 2))
;        (list (list "c" "d" "c") (list 2 5 2))
    (("d" "c" "c") (5 2 2))
;        (list (list "d" "c" "c") (list 5 2 2))
    (("c" "d" "d") (0 3 3))
;        (list (list "c" "d" "d") (list 0 3 3))
    (("d" "c" "d") (3 0 3))
;        (list (list "d" "c" "d") (list 3 0 3))
    (("d" "d" "c") (3 3 0))
;        (list (list "d" "d" "c") (list 3 3 0))
    (("d" "d" "d") (1 1 1))))
;        (list (list "d" "d" "d") (list 1 1 1))))

(defn get-point-list [game]
  (second (extract-entry game game-association-list)))

(defn get-player-points [num game]
  (nth (get-point-list game) num))

(defn make-play [move0 move1 move2] (list move0 move1 move2))

(defn get-scores [history0 history1 history2]
   "Given three histories, determine scores of three players as a list"
   (loop [history0 history0 history1 history1 history2 history2 score0 0 score1 0 score2 0]
     (if (empty-history? history0) (list score0 score1 score2)
         (let [game (make-play (most-recent-play history0)
                               (most-recent-play history1)
                               (most-recent-play history2))]
           (recur (rest-of-plays history0)
                  (rest-of-plays history1)
                  (rest-of-plays history2)
                  (+ (get-player-points 0 game) score0)
                  (+ (get-player-points 1 game) score1)
                  (+ (get-player-points 2 game) score2))))))

(defn print-out-results [history0 history1 history2 number-of-games]
  (let [scores (get-scores history0 history1 history2)]
    (newline)
    (println "Player 1 Score:  ")
    (println (* 1.0 (/ (nth scores 0) number-of-games)))
    (newline)
    (println "Player 2 Score:  ")
    (println (* 1.0 (/ (nth scores 1) number-of-games)))
    (newline)
    (println "Player 3 Score:  ")
    (println (* 1.0 (/ (nth scores 2) number-of-games)))))

(defn play-loop [strat0 strat1 strat2]
  "The play-loop procedure takes as its arguments three prisoner's"
  "dilemma strategies, and plays an iterated game of approximately"
  "one hundred rounds.  A strategy is a procedure that takes"
  "two arguments: a history of the player's previous plays and"
  "a history of the other player's previous plays.  The procedure"
  "returns either a \"c\" for cooperate or a \"d\" for defect."
  (loop [strat0 strat0
         strat1 strat1
         strat2 strat2
         count 0
         history0 the-empty-history
         history1 the-empty-history
         history2 the-empty-history
         limit (+ 90 (rand-int 21))]
    (if (= count limit) (print-out-results history0 history1 history2 limit)
        (let [result0 (strat0 history0 history1 history2)
              result1 (strat1 history1 history0 history2)
              result2 (strat2 history2 history0 history1)]
          (recur strat0
                 strat1
                 strat2
                 (inc count)
                 (extend-history result0 history0)
                 (extend-history result1 history1)
                 (extend-history result2 history2)
                 limit)))))


; (test-entry '('c' 'c' 'c') '('c' 'c' 'd'))

;; Problem 10


(defn PATSY [my-history history-a history-b]
  "c")

(defn NASTY [my-history history-a history-b]
  "d")

(defn SPASTIC [my-history history-a history-b]
  (if (= (rand-int 2) 0)
      "c"
      "d"))

(defn TOUGH-EYE-FOR-EYE [my-history history-a history-b]
  (if (empty-history? my-history)
      "c"
      (let [a (most-recent-play history-a)
            b (most-recent-play history-b)]
        (if (or (= a "d") (= b "d"))
          "d"
          "c"))))

(defn SOFT-EYE-FOR-EYE [my-history history-a history-b]
  (if (empty-history? my-history)
      "c"
      (let [a (most-recent-play history-a)
            b (most-recent-play history-b)]
        (if (and (= a "d") (= b "d"))
          "d"
          "c"))))


;; (play-loop SOFT-EYE-FOR-EYE PATSY PATSY)

;; Player 1 Score:  
;; 4.0

;; Player 2 Score:  
;; 4.0

;; Player 3 Score:  
;; 4.0
;; nil


;; (play-loop SOFT-EYE-FOR-EYE PATSY NASTY)

;; Player 1 Score:  
;; 2.0

;; Player 2 Score:  
;; 2.0

;; Player 3 Score:  
;; 5.0
;; nil

;; (play-loop TOUGH-EYE-FOR-EYE PATSY NASTY)

;; ;; Player 1 Score:  
;; ;; 2.989473684210526

;; ;; Player 2 Score:  
;; ;; 0.02105263157894737

;; ;; Player 3 Score:  
;; ;; 3.021052631578947
;; ;; nil

;; (play-loop PATSY PATSY PATSY)

;; Player 1 Score:  
;; 4.0

;; Player 2 Score:  
;; 4.0

;; Player 3 Score:  
;; 4.0
;; nil


;; (play-loop PATSY NASTY PATSY)

;; Player 1 Score:  
;; 2.0

;; Player 2 Score:  
;; 5.0

;; Player 3 Score:  
;; 2.0
;; nil

(defn make-combined-strategies [two-player-strat-a two-player-strat-b combiner-fn]
  (fn [my-history history-a history-b]
    (let [a-result (two-player-strat-a my-history history-a)
          b-result (two-player-strat-b my-history history-b)]
      (combiner-fn a-result b-result))))

(make-combined-strategies project-2.two-player/EYE-FOR-EYE project-2.two-player/EYE-FOR-EYE
                          (fn [r1 r2] (if (or (= r1 "d") (= r2 "d"))
                                        "d" "c")))

(make-combined-strategies project-2.two-player/EYE-FOR-EYE project-2.two-player/EGALITARIAN
                          (fn [r1 r2] (if (= (rand-int 2) 0) r1 r2)))


;; problem 12

(defn canonicalize [play-a play-b]
  (apply str (sort (list play-a play-b))))

; (canonicalize "a" "b")
; (canonicalize "b" "a")

(defn make-formatted-sublist [summary opponent-moves]
  (let [submap (get summary opponent-moves)
        c (get submap "c")
        d (get submap "d")
        ttl (+ c d)]
    (list c d ttl)))

(defn make-formatted-list [summary]
  (list (make-formatted-sublist summary "cc")
        (make-formatted-sublist summary "cd")
        (make-formatted-sublist summary "dd")))

(defn make-history-summary [hist-0 hist-1 hist-2]
  ;; don't care about my first play...just my reactions, so line them up
  (loop [hist-0 hist-0
         hist-1 (rest-of-plays hist-1)
         hist-2 (rest-of-plays hist-2)
         summary {"cc" {"c" 0 "d" 0}
                  "cd" {"c" 0 "d" 0}
                  "dd" {"c" 0 "d" 0}}]
    (let [play-0 (most-recent-play hist-0)
          play-1 (most-recent-play hist-1)
          play-2 (most-recent-play hist-2)
          opponent-plays (canonicalize play-1 play-2)
          updated-summary (update-in summary [opponent-plays play-0] inc)]
      (if (empty-history? (rest-of-plays hist-1))
        (make-formatted-list updated-summary)
        (recur (rest-of-plays hist-0) (rest-of-plays hist-1) (rest-of-plays hist-2)
               updated-summary)))))
        
(defn subprobability [[c d ttl]]
  (if (= ttl 0)
    nil
    (/ c ttl)))

(defn get-probability-of-c [summary]
  (let [both-c (nth summary 0)
        only-one-c (nth summary 1)
        both-d (nth summary 2)
        c-given-both-c (subprobability both-c)
        c-given-only-one-c (subprobability only-one-c)
        c-given-both-d (subprobability both-d)]
    (list c-given-both-c c-given-only-one-c c-given-both-d)))

;; Problem 14

;; in expected-values: #f = don't care
;;                      X = actual-value needs to be #f or X
(defn test-entry [expected-values actual-values]
  (cond (empty? expected-values) (empty? actual-values)

        (empty? actual-values) false
        
        (nil? (first expected-values)) (nil? (first actual-values)) 

        (nil? (first actual-values)) false

        (or (not (first expected-values))
            (not (first actual-values))
            (= (first expected-values) (first actual-values)))
        (test-entry (rest expected-values) (rest actual-values))

        :else false))


(defn is-he-a-fool? [hist0 hist1 hist2]
  (test-entry (list 1 1 1)
              (get-probability-of-c
               (make-history-summary hist0 hist1 hist2))))

(defn could-he-be-a-fool? [hist0 hist1 hist2]
  (test-entry (list 1 1 1)
              (map #(cond (nil? %) 1 
                          (= % 1) 1
                          :else 0)
                   (get-probability-of-c (make-history-summary hist0
                                                               hist1
                                                               hist2)))))

;; Use the get-probability-of procedure to write a predicate that
;; tests whether another player is using the soft-eye-for-eye
;; strategy.

(defn is-he-soft-eye-for-eye? [hist0 hist1 hist2]
  (test-entry (list 1 1 0)
              (get-probability-of-c
               (make-history-summary hist0 hist1 hist2))))

;; Write a new strategy called DONT-TOLERATE-FOOLS that should
;; cooperate for the first ten rounds; on subsequent rounds it checks
;; (on each round) to see whether the other players might both be
;; playing Patsy.  If our strategy finds that both other players seem
;; to be cooperating uniformly, it defects; otherwise it cooperates.

(defn DONT-TOLERATE-FOOLS [my-history history-a history-b]
  (if (<= (history-length my-history) 10)
      "c"
      (if (and (could-he-be-a-fool? history-a my-history history-b)
               (could-he-be-a-fool? history-b my-history history-a))
        "d"
        "c")))

; (play-loop PATSY PATSY PATSY)

;; Player 1 Score:  
;; 4.0

;; Player 2 Score:  
;; 4.0

;; Player 3 Score:  
;; 4.0
;; nil

; (play-loop DONT-TOLERATE-FOOLS PATSY PATSY)

;; Player 1 Score:  
;; 4.891089108910891

;; Player 2 Score:  
;; 2.217821782178218

;; Player 3 Score:  
;; 2.217821782178218
;; nil

; (play-loop DONT-TOLERATE-FOOLS PATSY TOUGH-EYE-FOR-EYE)

;; Player 1 Score:  
;; 3.98

;; Player 2 Score:  
;; 3.92

;; Player 3 Score:  
;; 3.98
;; nil

; (play-loop PATSY PATSY TOUGH-EYE-FOR-EYE)

;; Player 1 Score:  
;; 4.0

;; Player 2 Score:  
;; 4.0

;; Player 3 Score:  
;; 4.0
;; nil

; (play-loop PATSY NASTY TOUGH-EYE-FOR-EYE)

;; Player 1 Score:  
;; 0.01818181818181818

;; Player 2 Score:  
;; 3.018181818181818

;; Player 3 Score:  
;; 2.990909090909091
;; nil

; (play-loop DONT-TOLERATE-FOOLS NASTY TOUGH-EYE-FOR-EYE)

;; Player 1 Score:  
;; 0.0202020202020202

;; Player 2 Score:  
;; 3.02020202020202

;; Player 3 Score:  
;; 2.98989898989899
