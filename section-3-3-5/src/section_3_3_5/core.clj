(ns section-3-3-5.core
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(defn error [& msg] (throw (IllegalStateException. (apply str msg))))

(defn inform-about-value [constraint]
  (constraint :I-have-a-value))

(defn inform-about-no-value [constraint]
  (constraint :I-lost-my-value))

(defn has-value? [connector]
  "Tells whether the connector has a value"
  (connector :has-value?))

(defn get-value [connector]
  "Returns the connector's current value"
  (connector :value))

(defn set-value! [connector new-value informant]
  "Indicates that the informant is requesting the connector to set its value to the new value"
  ((connector :set-value!) new-value informant))

(defn forget-value! [connector retractor]
  "Tells the connector that the retractor is requesting it to forget this value"
  ((connector :forget) retractor))

(defn connect [connector new-constraint]
  "Tells the connector to participate in this new constraint"
  ((connector :connect) new-constraint))

(defn adder [a1 a2 sum]
  (letfn [(process-new-value []
            (cond (and (has-value? a1) (has-value? a2))
                  (set-value! sum
                              (+ (get-value a1) (get-value a2))
                              me)
                  
                  (and (has-value? a1) (has-value? sum))
                  (set-value! a2
                              (- (get-value sum) (get-value a1))
                              me)

                  (and (has-value? a2) (has-value? sum))
                  (set-value! a1
                              (- (get-value sum) (get-value a2))
                              me)))
          (process-forget-value []
            (forget-value! sum me)
            (forget-value! a1 me)
            (forget-value! a2 me)
            (process-new-value))
          (me [request]
            (cond (= request :I-have-a-value)
                  (process-new-value)

                  (= request :I-lost-my-value)
                  (process-forget-value)

                  :else
                  (error "Unknown request -- ADDER" request)))]
    (connect a1 me)
    (connect a2 me)
    (connect sum me)
    me))

(defn multiplier [m1 m2 product]
  (letfn [(process-new-value []
            (cond (or (and (has-value? m1) (= (get-value m1) 0))
                      (and (has-value? m2) (= (get-value m2) 0)))
                  (set-value! product 0 me)

                  (and (has-value? m1) (has-value? m2))
                  (set-value! product
                              (* (get-value m1) (get-value m2))
                              me)

                  (and (has-value? product) (has-value? m1))
                  (set-value! m2
                              (/ (get-value product) (get-value m1))
                              me)

                  (and (has-value? product) (has-value? m2))
                  (set-value! m1
                              (/ (get-value product) (get-value m2))
                              me)))
          (process-forget-value []
            (forget-value! product me)
            (forget-value! m1 me)
            (forget-value! m2 me)
            (process-new-value))
          (me [request]
            (cond (= request :I-have-a-value)
                  (process-new-value)

                  (= request :I-lost-my-value)
                  (process-forget-value)

                  :else
                  (error "Unknown request -- MULTIPLIER" request)))]
    (connect m1 me)
    (connect m2 me)
    (connect product me)
    me))

(defn constant [value connector]
  (letfn [(me [request]
            (error "Unknown request -- CONSTANT" request))]
    (connect connector me)
    (set-value! connector value me)
    me))

(defn probe [name connector]
  (letfn [(print-probe [value]
            (println "Probe: " name " = " value))
          (process-new-value []
            (print-probe (get-value connector)))
          (process-forget-value []
            (print-probe "?"))
          (me [request]
            (cond (= request :I-have-a-value)
                  (process-new-value)

                  (= request :I-lost-my-value)
                  (process-forget-value)

                  :else
                  (error "Unknown request -- PROBE" request)))]
    (connect connector me)
    me))

(defn for-each-except [exception procedure list]
  (letfn [(loop-through [items]
            (cond (empty? items) :done

                  (= (first items) exception) (recur (rest items))

                  :else (do (procedure (first items))
                            (recur (rest items)))))]
    (loop-through list)))
                                   
(defn make-connector []
  (let [value (atom nil)
        informant (atom nil)
        constraints (atom '())]
    (letfn [(me [request]
              (cond (= request :has-value?)
                    (if @informant true false)
                    
                    (= request :value)
                    @value
                    
                    (= request :set-value!)
                    set-my-value
                    
                    (= request :forget)
                    forget-my-value
                    
                    (= request :connect)
                    connect

                    :else
                    (error "Unknown operation -- CONNECTOR " request)))
            (set-my-value [newval setter]
              (cond (not (has-value? me))
                    (do
                      (reset! value newval)
                      (reset! informant setter)
                      (for-each-except setter
                                       inform-about-value
                                       @constraints))
                    
                    (not (= @value newval))
                    (error "Contradiction--value=" @value ", newval ="
                    newval " (class @value)=" (class @value) ", (class
                    newval)=" (class newval))
                    
                    :else
                    :ignored))
            (forget-my-value [retractor]
              (if (= retractor @informant)
                (do (reset! informant nil)
                    (for-each-except retractor
                                     inform-about-no-value
                                     @constraints))
                :ignored))
            (connect [new-constraint]
              (when-not (some #{new-constraint} @constraints)
                (swap! constraints conj new-constraint))
              (if (has-value? me)
                (inform-about-value new-constraint))
              :done)]
      me)))
        

;; deflection*area*elastic-modulus = force-on-rod*length
;; dAE = FL

;; 9C = 5(F-32)
(defn celsius-fahrenheit-converter [c f]
  (let [u (make-connector)
        v (make-connector)
        w (make-connector)
        x (make-connector)
        y (make-connector)]
    (multiplier c w u)
    (multiplier v x u)
    (adder v y f)
    (constant 9 w)
    (constant 5 x)
    (constant 32 y)))


(def C (make-connector))
(def F (make-connector))
(celsius-fahrenheit-converter C F)
;= :ok

(probe "Celcius temp" C)
(probe "Fahrenheit temp" F)

(set-value! C 25 :user)
;= Probe: Celsius temp = 25
;= Probe: Fahrenheit temp = 77
;= :done

; (set-value! F 212 :user)
;= Error! Contradiction (77 212)

(forget-value! C :user)
;= Probe: Celsius temp = ?
;= Probe: Fahrenheit temp = ?
;= :done

(set-value! F 212 :user)
;= Probe: Celsius temp = 212
;= Probe: Fahrenheit temp = 100
;= :done

;; Exercise 3.33:

(defn averager [a b c]
  "c is the average of a and b"
  (letfn [(process-new-value []
            ;; a and b set - c = (a+b)/2
            ;; a and c set - b = 2*c - a
            ;; b and c set - a = 2*c - b
            (cond (and (has-value? a) (has-value? b))
                  (set-value! c (/ (+ (get-value a) (get-value b)) 2) me)

                  (and (has-value? a) (has-value? c))
                  (set-value! b (- (* 2 (get-value c)) (get-value a)) me)
                  
                  (and (has-value? b) (has-value? c))
                  (set-value! a (- (* 2 (get-value c)) (get-value b)))) me)
          (process-forget-value []
            (forget-value! a me)
            (forget-value! b me)
            (forget-value! c me)
            (process-new-value))
          (me [request]
            (cond (= request :I-have-a-value)
                  (process-new-value)

                  (= request :I-lost-my-value)
                  (process-forget-value)

                  :else
                  (error "Unknown request -- AVERAGER" request)))]
    (connect a me)
    (connect b me)
    (connect c me)
    me))

(def A (make-connector))
(def B (make-connector))
(def C (make-connector))

(probe "Num1" A)
(probe "Num2" B)
(probe "Average" C)
(averager A B C)

(set-value! A 100 :user)
(set-value! B 0 :user)
(set-value! C 50 :user)

;; Exercise 3.34

;; for starters, there's no knowledge of the additional constraint
;; that and a b in the multiplier must always be the same -- if you
;; set c, and b won't e set.

;; Exercise 3.35

(defn abs [x]
  (if (> 0 x) (- x) x))

(def ^:private tolerance 0.00001)

(defn fixed-point [f first-guess]
  (letfn [(close-enough? [v1 v2]
            (< (abs (- v1 v2)) tolerance))
          (my-try [guess]
            (let [next (f guess)]
              (if (close-enough? guess next)
                next
                (my-try next))))]
    (my-try first-guess)))

(defn average [a b]
  (/ (+ a b) 2))

(defn sqrt [x]
  (fixed-point #(average % (/ x %))
               1.0))

(defn squarer [a b]
  (letfn [(process-new-value []
            (cond (has-value? b)
                  (if (< (get-value b) 0)
                    (error "square less than 0 -- SQURARER")
                    (set-value! a (sqrt (get-value b)) me))

                  (has-value? a)
                  (set-value! b (* (get-value a) (get-value a)) me)))
          (process-forget-value []
            (forget-value! a me)
            (forget-value! b me)
            (process-new-value))
          (me [request]
            (cond (= request :I-have-a-value)
                  (process-new-value)

                  (= request :I-lost-my-value)
                  (process-forget-value)

                  :else
                  (error "Unknown request -- SQURARER" request)))]
    (connect a me)
    (connect b me)
    me))

(def X (make-connector))
(def Y (make-connector))

(probe "X" X)
(probe "Y" Y)
(squarer X Y)

(set-value! Y 25 :user)
(set-value! X 5 :user)
(set-value! B 0 :user)
(set-value! C 50 :user)

;; Exercise 3.36


;; see notepad

;; Exercise 3.37

(defn c+ [x y]
  (let [z (make-connector)]
    (adder x y z)
    z))

(defn c- [x y]
  (let [z (make-connector)]
    (adder z y x)
    z))

(defn c* [x y]
  (let [z (make-connector)]
    (multiplier x y z)
    z))

(defn cdiv [x y]
  (let [z (make-connector)]
    (multiplier z y x)
    z))

(defn cv [n]
  (let [x (make-connector)]
    (constant n x)
    x))

(defn celsius-fahrenheit-converter [x]
  (c+ (c* (cdiv (cv 9) (cv 5))
          x)
      (cv 32)))


(def C (make-connector))
(def F (celsius-fahrenheit-converter C))
;= :ok

(probe "Celcius temp" C)
(probe "Fahrenheit temp" F)

(set-value! C 25 :user)
;= Probe: Celsius temp = 25
;= Probe: Fahrenheit temp = 77
;= :done

; (set-value! F 212 :user)
;= Error! Contradiction (77 212)

(forget-value! C :user)
;= Probe: Celsius temp = ?
;= Probe: Fahrenheit temp = ?
;= :done

(set-value! F 212 :user)
;= Probe: Celsius temp = 212
;= Probe: Fahrenheit temp = 100
;= :done
