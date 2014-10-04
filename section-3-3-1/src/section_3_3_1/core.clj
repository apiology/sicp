(ns section-3-3-1.core
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

;; Exercise 3.12

; (define x (list 'a 'b))
; (define y (list 'c 'd))
; (define z (list x y))
; z
; RESPONSE: (a b c d)
; (cdr x)
; RESPONSE: (b)
; (define w (append! x y))
; w
; RESPONSE: (a b c d)
; (cdr x)
; RESPONSE: (b c d)

;; intuitively obvious to even the most casual observer

;; Exercise 3.13

; [a -]> [b -]> [c -]
;  ^---------------+

;; infinite loop

;; Exercise 3.14

;; reverses the list in-place

; v would be one element list of (a)
; w would be (d c b a)



