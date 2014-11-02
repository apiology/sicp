(ns section-3-5-3.core
  (:require
   [section-3-5-3.streams :refer :all]
   [section-3-5-3.stream-math :as sm]
   ))

; (display-stream (sqrt-stream 2))

;; pi/4 = 1 - 1/3 + 1/5 - 1/7 + ...

;; summands are the recipricols of the odd integers, with alternating signs


;; each value is pi to more and more precision.



; (display-stream (euler-transform pi-stream))


; (display-stream (accelerated-sequence euler-transform pi-stream))

;; ex 3.63 - alyssa is right.  yes, this relies on memo-proc working.

;; ex 3.64


;; (sm/sqrt 2 0.00000000000001)

;; ex 3.65

;; ln 2 = 1 - 1/2 + 1/3 - 1/4 + ...


; (display-stream (ln-2-summands 1))
; (display-stream ln-2-stream)

;; does not converge rapidly...

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Infinite streams of pairs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; int-pairs - all integers i, j where i<= j

;; s and t are the infinite series that i and j are pulled from....
;;
;; composed of three parts:
;; 1) s0, t0
;; 2) rest of the pairs in the first rows
;; 3) pair(stream-cdr s, stream-cdr t)

;; first piece is:

; (stream-car s) (stream-car t) ;; #1

;; second piece is:
; (stream-map #(list (stream-car s) %)
;             (stream-cdr t))

  


