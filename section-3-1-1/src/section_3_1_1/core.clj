(ns section-3-1-1.core
  (:gen-class))

;; Exercise 3.1

(defn make-accumulator [num]
  (let [num (atom num)]
    (fn [amt]
      (swap! num + amt)
      @num)))

(def a (make-accumulator 5))
(a 10) ;= 15
(a 10) ;= 25

;; Exerise 3.2
(defn make-monitored [fn-to-monitor]
  (let [count (atom 0)]
    (fn [& args]
      (if (= (first args) :how-many-calls?)
        @count
        (do
          (swap! count inc)
          (apply fn-to-monitor args))))))

(def s (make-monitored #(Math/sqrt %)))
(s 100) ;= 10.0
(s :how-many-calls?) ;= 1

;; Exercise 3.3

(defn swap*!
  "Like swap! but returns a vector of [old-value new-value]"
  [atom f & args]
  (loop [] 
    (let [ov @atom 
          nv (apply f ov args)]
      (if (compare-and-set! atom ov nv)
        [ov nv]
        (recur)))))

(defn make-account [initial-balance initial-password]
  (let [balance (ref initial-balance)]
    (letfn [(withdraw [amount]
              (dosync
               (ensure balance)
               (if (>= @balance amount)
                 (alter balance #(- % amount))
                 "Insufficient funds!")))]
      (fn [password m] (if (= password initial-password)
                         (case m :withdraw withdraw
                               m :deposit deposit
                               :else (throw (Exception. "Bad message")))
                         (fn [_] "Incorrect password!"))))))
      
(def acc (make-account 100 :secret-password))

((acc :secret-password :withdraw) 40) ;= 60
((acc :not-the-secret-password :withdraw) 40) ;= "Incorrect password"


(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
