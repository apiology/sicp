(ns project-2.three-player)

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
  '((("c" "c") (3 3))
    (("c" "d") (0 5))
    (("d" "c") (5 0))
    (("d" "d") (1 1))))

;; in expected-values: #f = don't care
;;                      X = actual-value needs to be #f or X
;(defn test-entry [expected-values actual-values]
;   (cond ((null? expected-values) (null? actual-values))
;         ((null? actual-values) #f)
;         ((or (not (car expected-values))
;              (not (car actual-values))
;              (= (car expected-values) (car actual-values)))
;          (test-entry (cdr expected-values) (cdr actual-values)))
;         (else #f)))


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


;
;(defn (is-he-a-fool? [hist0 hist1 hist2]
;   (test-entry (list 1 1 1)
;               (get-probability-of-c
;                (make-history-summary hist0 hist1 hist2))))
;
;(defn could-he-be-a-fool? [hist0 hist1 hist2]
;  (test-entry (list 1 1 1)
;              (map (lambda (elt)
;                      (cond ((null? elt) 1)
;                            ((= elt 1) 1)
;                            (else 0)))
;                   (get-probability-of-c (make-history-summary hist0
;                                                               hist1
;                                                               hist2)))))
