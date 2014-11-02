(ns section-3-5-3.stream-math-test
  (:require [clojure.test :refer :all]
            [section-3-5-3.stream-math :refer :all]
            [section-3-5-3.streams :refer :all]))

(deftest a-test
  (testing "FIXME, I fail."
    (is (= '(1 1) (stream-ref int-pairs 0)))
    (is (= '(1 2) (stream-ref int-pairs 1)))
    (is (= '(2 2) (stream-ref int-pairs 2)))
    ;; (is (= '(1 2) (stream-ref int-pairs 3))) ;; FAILS
    ))

(def sqrt-2 (sqrt-stream 2))

; (stream-ref sqrt-2 0)
; (stream-ref sqrt-2 1)
; (stream-ref sqrt-2 2)
; (stream-ref sqrt-2 3)
; (stream-ref sqrt-2 4)
; (stream-ref sqrt-2 5)


  ; (pairs s t)

;; all integers i,j with i<=j such that i+j is prime
(stream-filter #(prime? (+ (first %) (second %)))
               int-pairs)

(stream-ref int-pairs 0)
(stream-ref int-pairs 1)
; (stream-ref int-pairs 2) ; XXX this blows up

;; need plan to fix this:
;; 1) break components into correct namespaces
;; 2) come up with list of things that int-pairs uses
;; 3) come up with unit tests for each until I have problem fixed
;; (stream-ref int-pairs 3) ; XXX this blows up
