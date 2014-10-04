(ns section-3-3-4.core
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

;; a wire consists of:
;;; signal-value
;;; action-procedures - list of fns to call when things change


(defrecord Wire [signal-value action-procedures])

(defn make-wire []
  (Wire. (atom 0) (atom '())))

(defn get-signal [wire] @(:signal-value wire))

(defn- call-each [procedures]
  (doseq [proc procedures]
    (proc)))

(defn set-signal! [wire new-value] 
  (let [old-value (get-signal wire)]
    (if (not= old-value new-value)
      (compare-and-set! (:signal-value wire) old-value new-value)
      (call-each @(:action-procedures wire)))))

(defn add-action! [wire procedure-of-no-args]
  (swap! (:action-procedures wire)
         #(conj % procedure-of-no-args))
  (procedure-of-no-args))

(defrecord TimeSegment [time queue])

(defn make-time-segment [time queue]
  (TimeSegment. time queue))

(defn segment-time [s] (:time s))

(defn segment-queue [s] (:queue s))

(defrecord Agenda [time segments])

(defn make-agenda [] (Agenda. (atom 0) (atom '())))

(defn set-current-time! [agenda time]
  (reset! (:time agenda) time))

(defn segments [agenda]
  (:segments agenda))

(defn empty-agenda? [agenda] 
  (empty? @(segments agenda)))

(defn make-queue [] (atom '()))

(defn insert-queue! [queue item] (swap! queue conj item))

(defn add-to-agenda! [time action agenda] 
  (letfn [(belongs-before? [segments]
            (or (empty? segments)
                (< time (segment-time (first segments)))))
          (make-new-time-segment [time action]
            (let [q (make-queue)]
              (insert-queue! q action)
              (make-time-segment time q)))
          (add-to-segments [segments]
            (if (empty? segments)
              segments
              (if (= (segment-time (first segments)) time)
                (do
                  (insert-queue! (segment-queue (first segments)) 
                                 action)
                  segments) ;; twiddled existing segment inside
                (let [remaining (rest segments)]
                  (if (belongs-before? remaining)
                    (cons (first segments) 
                          (cons (make-new-time-segment time action)
                                remaining))
                    (cons (first segments) (add-to-segments remaining)))))))
          (add-to-segments! [segments-atom]
            (swap! segments-atom add-to-segments))]
    (let [segments-atom (segments agenda)]
      (if (belongs-before? @segments-atom)
        (swap! segments-atom #(cons (make-new-time-segment time action) %))
        (add-to-segments! segments-atom)))))

(defn delete-first-in-queue! [q] (swap! q rest))
(defn empty-queue? [q] (empty? @q))
(defn front-queue [q] (first @q))

(defn first-segment [agenda]
  (first @(segments agenda)))

(defn remove-first-agenda-item! [agenda] 
  (let [segments-atom (segments agenda)
        first-segment (first @segments-atom)
        q (segment-queue first-segment)]
    (delete-first-in-queue! q) ;; aka delete-queue!
    (if (empty-queue? q)
      (swap! segments-atom #(rest %)))))


(defn error [msg] (throw (IllegalStateException. msg)))

(defn first-agenda-item [agenda]
  (if (empty-agenda? agenda)
    (error "Agenda is empty -- FIRST-AGENDA-ITEM")
    (let [first-seg (first-segment agenda)]
      (set-current-time! agenda (segment-time first-seg))
      (front-queue (segment-queue first-seg)))))

;; exercise 3.32

;; order must be used so that items which depend on actions in the
;; past happen inthe righ order of delay

(defn current-time [agenda] @(:time agenda))

(def the-agenda (make-agenda))

(defn after-delay [delay action] 
  (add-to-agenda! (+ delay (current-time the-agenda))
                  action
                  the-agenda))

(defn propogate []
  (if (empty-agenda? the-agenda)
    :done
    (let [first-item (first-agenda-item the-agenda)]
      (first-item)
      (remove-first-agenda-item! the-agenda)
      (recur))))

(defn probe [name wire]
  (add-action! wire
               #(println name " " (current-time the-agenda) " New-value = "
                         (get-signal wire))))

(def inverter-delay 2)
(def and-gate-delay 3)
(def or-gate-delay 5)

(defn logical-not [s]
  (cond (= s 0) 1
        (= s 1) 0
        :else (error "Invalid signal" s)))

(defn inverter [input output]
  (let [invert-input (fn []
                       (let [new-value (logical-not (get-signal input))]
                         (after-delay inverter-delay #(set-signal! output new-value))))]
    (add-action! input invert-input)
    :ok))

(defn logical-and [a b]
  (cond (= a 0) 0
        (= b 0) 0
        (= a b 1) 1
        :else (error "Invalid signal.  a=" a ", b=" b)))

(defn and-gate [a1 a2 output]
  (letfn [(and-action-procedure  []
            (let [new-value (logical-and (get-signal a1) (get-signal a2))]
              (after-delay and-gate-delay #(set-signal! output new-value))))]
    (add-action! a1 and-action-procedure)
    (add-action! a2 and-action-procedure)
    :ok))
    
;; exercise 3.29

(defn logical-or [a b]
  (cond (= a 1) 1
        (= b 1) 1
        (= a b 0) 0
        :else (error "Invalid signal: a=" a ", b=" b)))

(defn or-gate [a1 a2 output]
  (let [or-action-procedure 
        (fn []
          (let [new-value (logical-or (get-signal a1) (get-signal a2))]
            (after-delay or-gate-delay #(set-signal! output new-value))))]
    (add-action! a1 or-action-procedure)
    (add-action! a2 or-action-procedure)
    :ok))

(defn compound-or-gate [a1 a2 output]
  (inverter (and-gate (inverter a1)
                      (inverter a2))))

;; delay time: 2*inverter-time + 1*and-gate

; exercise 3.30

(defn half-adder [a b s c]
  (let [d (make-wire)
        e (make-wire)]
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    :ok))

;; half-adder-delay = max(and-gate-delay, or-gate-delay) + inverter-delay + and-gate-delay

(defn full-adder [a b c-in sum c-out]
  (let [s (make-wire)
        c1 (make-wire)
        c2 (make-wire)]
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out))
  :ok)

(defn ripple-carry-adder [a-wires b-wires s-wires c]
  (if-not (empty? a-wires)
    (let [new-carry (make-wire)]
      (ripple-carry-adder (rest a-wires)
                          (rest b-wires)
                          (rest s-wires)
                          new-carry)
      (full-adder (first a-wires)
                  (first b-wires)
                  new-carry
                  (first s-wires)
                  c))))

;; delay needed to get complete output:
;; ripple-carry-adder-delay = n*(2*max(and-gate-delay, or-gate-delay) + 2*inverter-delay + 2*and-gate-delay + or-gate-delay)
;; ripple-carry-adder-delay = 2n*max(and-gate-delay, or-gate-delay) + 2n*inverter-delay + 2n*and-gate-delay + n*or-gate-delay

; SICP complection of physics, calculus and electrical engineering knowledge with functional programming is definitely not diversity-friendly.

(def input-1 (make-wire))
(def input-2 (make-wire))
(def sum (make-wire))
(def carry (make-wire))
(probe 'sum sum)
;= sum 0 New-value = 0
(probe 'carry carry)
;= carry 0 New-value = 0

(half-adder input-1 input-2 sum carry)
;= :ok
(set-signal! input-1 1)
;= :done

(propogate)
;= sum 8 New-value = 1
;= :done

(set-signal! input-2 1)
;= :done
(propogate)
;= carry 11 New-value = 1
;= sum 16 New-value = 0
;= :done

;; Exercise 3.31

;; To initialize it with the current value fo the wire


(def a (make-wire))
(def b (make-wire))
(def c (make-wire))
(def d (make-wire))
(def e (make-wire))
(def s (make-wire))

(probe 'a a)
(probe 'b b)
(probe 'c c)
(probe 'd d)
(probe 'e e)
(probe 's s)
(probe 'd d)
;= a   8  New-value =  0
(set-signal! a 1)
(propogate)

(set-signal! b 1)
(or-gate a b d)
(and-gate a b c)
(propogate)

(inverter c e)
(and-gate d e s)
(propogate)

;; full-adder-delay = 2*half-addder-delay + or-delay
;; full-adder-delay = 2*(max(and-gate-delay, or-gate-delay) + inverter-delay + and-gate-delay) + or-gate-delay
;; full-adder-delay = 2*max(and-gate-delay, or-gate-delay) + 2*inverter-delay + 2*and-gate-delay + or-gate-delay
