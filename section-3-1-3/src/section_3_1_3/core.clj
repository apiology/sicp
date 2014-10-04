(ns section-3-1-3.core
  (:gen-class))

(defn make-account-internal [balance initial-password]
  (letfn [(withdraw [amount]
            (dosync
             (ensure balance)
             (if (>= @balance amount)
               (alter balance #(- % amount))
               "Insufficient funds!")))
          (make-joint [new-password]
            (make-account-internal balance new-password))
          (balance-check [] @balance)]
    (fn [password m] (if (= password initial-password)
                       (case m 
                         :withdraw withdraw
                         :make-joint make-joint
                         :balance balance-check
                         :else (throw (Exception. "Bad message")))
                       (fn [_] "Incorrect password!")))))

(defn make-account [initial-balance initial-password]
  (make-account-internal (ref initial-balance) initial-password))

(defn make-joint [password-protected-account password new-password]
  ((password-protected-account password :make-joint) new-password))

(def joe-account (make-account 20 :joe-password))
(def pete-account (make-joint joe-account :joe-password :pete-password))
((joe-account :joe-password :balance)) ;= 20
((pete-account :pete-password :balance)) ;= 20
((joe-account :joe-password :withdraw) 10) ;= 10
((joe-account :joe-password :balance)) ;= 10
((pete-account :pete-password :balance)) ;= 10

(def f-state (atom nil))

(defn f [x]
  (if (nil? @f-state)
    (swap! f-state (constantly x)))
  @f-state)
    
    


(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
