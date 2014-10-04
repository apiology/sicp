(ns section-3-3-2.core
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(define (make-queue)
  (let ((front-ptr '())
        (rear-ptr '()))
  (define (set-rear-ptr item) (setq rear-ptr item))
  (define (dispatch m)
    (cond
       (eq? m 'empty-queue?) (null? front-ptr)
       (eq? m 'front-queue) (car front-ptr)
       (eq? m 'insert-queue!) (lambda (item) (let new-pair (cons item '())
                                                  (cond ((empty-queue?)
                                                          (set-front-ptr! new-pair)
                                                          (set-rear-ptr! new-pair))
                                                        ((:else
                                                          (set-cdr! rear-ptr new-pair)
                                                          (set-rear-ptr! new-pair))))))
       (eq? m 'delete-queue!) (set-front-ptr! (cdr front-ptr)))
                                                          

;; Exercise 3.23

