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

;; [done to here]

;; ex 3.64

;; see stream-limit and (sqrt)

;; (sm/sqrt 2 0.00000000000001)

;; ex 3.65

;; ln 2 = 1 - 1/2 + 1/3 - 1/4 + ...


; (display-stream (ln-2-summands 1))
; (display-stream ln-2-stream)

;; does not converge rapidly...

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Infinite streams of pairs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; int-pairs - all integers (i, j) where i<= j
;;
;; s and t are the infinite series that i and j are pulled from....

;; see sm/prime-sum-pairs-stream

;; convenience for repl

; (stream-take 5 sm/prime-sum-pairs-stream)
; (finite 5 sm/prime-sum-pairs-stream)
; (finite 5 sm/int-pairs)

; (finite 200 (sm/pairs-on-or-above-diagonal sm/integers sm/integers))

;; (stream->seq (stream-take 5 sm/prime-sum-pairs-stream))

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

  
;; Exercise 3.66

;; first element grows very slowly
;; about 200 preceed 1,100


;; (stream-find-index-of '(1 100) (sm/pairs-on-or-above-diagonal sm/integers sm/integers))
;;=> 197

;; (stream-find-index-of '(2 100) (sm/pairs-on-or-above-diagonal sm/integers sm/integers))
;;=> 392

;; (stream-find-index-of '(3 100) (sm/pairs-on-or-above-diagonal sm/integers sm/integers))
;;=> 778

;; (stream-find-index-of '(4 100) (sm/pairs-on-or-above-diagonal sm/integers sm/integers))
;;=> 1542

;; (stream-find-index-of '(n 100) (sm/pairs-on-or-above-diagonal sm/integers sm/integers))
;;=>200^n

; (stream-find-index-of '(99 100) (sm/pairs-on-or-above-diagonal sm/integers sm/integers))
;;=>200^100

;; Exercise 3.67

;; see (finite 50 (sm/all-pairs sm/integers sm/integers))

;; Exercise 3.68

(defn probably-bad-pairs [s t]
  (stream-interleave
   (stream-map #(list (stream-car s) %) t)
   (probably-bad-pairs (stream-cdr s) (stream-cdr t))))

;; there's no base case to the recursion - it recurses forever.
;; 
;; (finite 50 (probably-bad-pairs sm/integers sm/integers))
;;;=> StackOverflow

;; Exercise 3.69

;; see (three-d-pairs-on-or-above-diagonal)


;; x^2 + y^2 = z^2
; (finite 10 sm/int-triples)
; (finite 2 sm/pythagorean-triples)

