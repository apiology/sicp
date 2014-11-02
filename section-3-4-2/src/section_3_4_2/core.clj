(ns section-3-4-2.core
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

;; ex 3.39

;; NOPE - 110: P_2 changes x from 10 to 11 between the two times that P_1 accesses the value of x during the evaluation of (* x x).

;; 101: P_1 sets sets x to 100 and then P2 increments x to 101.
;; 121: P_2 increments x to 11 and then P1 sets x to x times x.
;;  11: P_2 accesses x, then P_1 sets x to 100, then P2 sets x. 
;; 100: P_1 accesses x (twice), then P2 sets x to 11, then P_1 sets x.

#_ #_ #_
 (define x 10)
 (define s (make-serializer))
(parallel-execute (lambda () (set! x ((s (lambda () (* x x)))))) ; P_1
                  (s (lambda () (set! x (+ x 1))))) ; P_2

;; ex 3.40

; a - all possible values from executing:

; (define x 10)
; (parallel-execute (lambda () (set! x (* x x)))
;                   (lambda () (set! x (* x x x))))

a1 = access
a2 = access
a3 = set
b1 = access
b2 = access
b3 = access
b4 = set

;; a1=10 a2=10 a3 (100) b1 b2 b3 b4 (1,000,000)
;; b1=10 b2=10 b3=10 b4 (1000) a1 a2 a3 (1,000,000)
;; a1=10 b1=10 b2=10 b3=10 b4 (1000) a2=1000 a3 (10,000)
;; b1=10 a1=10 a2=10 a3 (100) b2=100 b3=100 b4 = (100,000)
;; b1=10 b2=10 a1=10 a2=10 a3 (100) b3=100 b4 (10,000)
;; a1=10 a2=10 b1=10 b2=10 b3=10 b4 (1000) a3 (100)
;; a1=10 a2=10 b1=10 b2=10 b3=10 a3 (100) b4 (1,000)


; b Which of these possibilities remain if we instead use serialized
;   procedures:

;; a1=10 a2=10 a3 (100) b1 b2 b3 b4 (1,000,000)
;; b1=10 b2=10 b3=10 b4 (1000) a1 a2 a3 (1,000,000)

;; ex 3.41

; not a bad idea for the future, but shouldn't happen if assignment is atomic.

;; ex 3.42

;; yes, safe--no difference

;; ex 3.43

;; end of week


