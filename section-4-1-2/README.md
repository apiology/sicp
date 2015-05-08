# section-4-1-2

A Clojure library designed to ... well, that part is up to you.

## Usage

FIXME

## License

Copyright © 2015 FIXME

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.

;; Exercise 4.2

;; a

;; Won't work--special forms like (define) need to be special forms
;; and not regular function calls

;; b

;;
;; to do that:
;;
;; (defn application? [exp]
;;   (util/tagged-list? exp 'call))

;; (defn operator [exp]
;;   (second exp))

;; (defn operands [exp]
;;   (rest (rest exp)))

;; Exercise 4.3

