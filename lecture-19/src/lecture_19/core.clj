(ns lecture-19.core
  (:gen-class))

;; lexer:
;;;; - break up input string into "words" called tokens

;; parser:
;;;; - convert linear sequence of tokens into a tree
;;;; - like diagramming sentences in elementary school
;;;; - also convert self-evaluating tokens to their internal values
;;;;     eg., #f is converted to the internal false value

;; evaluator:
;;;; follow language rules to convert parse tree to a value
;;;; read and modify the environment as needed

;; printer:
;;;; convert value to human-readable output string


(defn tag-check [e sym] (and (seq? e) (= (first e) sym)))

(defn sum? [e] (tag-check e 'plus*))
(declare eval-sum)
(defn error [& msg] (throw (IllegalStateException. (apply str msg))))
;; (defn eval [exp]
;;   (cond
;;     (number? exp) exp
;;    (sum? exp) (eval-sum exp)
;;    :else (error "unknown expression " exp)))

(defn eval-sum [exp]
  (+ (eval (nth exp 1))
     (eval (nth exp 2))))

(eval '(plus* 24 (plus* 5 6)))


(defn define? [exp] (tag-check exp 'define*))

(declare lookup)

(declare eval-define)

;; (defn eval [exp]
;;   (cond
;;     (number? exp) exp
;;     (sum? exp) (eval-sum exp)
;;     (symbol? exp) (lookup exp)
;;     (define? exp) (eval-define exp)
;;     :else
;;     (error "unknown expression " exp)))

;; variation on table ADT from March 2 lecture (only difference is
;; that table-get returns a binding, while original version
;; returned a value):
;; make-table void -> table
;; table-get table, symbol -> (binding | null)
;; table-put! table, symbol, anytype -> undef
;; binding-value binding -> anytype
(defn make-table [] (atom {}))
;; (def environment (make-table))

;; (defn lookup [name]
;;   (let [binding (get @environment name :not-found)]
;;     (if (= :not-found binding)
;;       (error "unbound variable: " name)
;;       binding)))

;; (defn eval-define [exp]
;;   (let [name (nth exp 1)
;;         defined-to-be (nth exp 2)]
;;     (swap! environment assoc name (eval defined-to-be))
;;     'undefined))

(eval '(define* x* (plus* 4 5)))
(eval '(plus* x* 2))

(defn eval-greater [exp]
  (> (eval (nth exp 1))
     (eval (nth exp 2))))

;; (defn eval-if [exp]
;;   (let [predicate (nth exp 1)
;;         first-result (eval predicate)
;;         then (nth exp 2)
;;         else (nth exp 3)]
;;     (cond (= first-result true) (eval then)
;;           (= first-result false) (eval else)
;;           :else (error "predicate not a conditional: " predicate))))

(defn greater? [exp] (tag-check exp 'greater*))

(defn if? [exp] (tag-check exp 'if*))

;; (defn eval [exp]
;;   (cond
;;     (number? exp) exp
;;     (symbol? exp) (lookup exp)
;;     (sum? exp) (eval-sum exp)
;;     (define? exp) (eval-define exp)
;;     (greater? exp) (eval-greater exp)
;;     (if? exp) (eval-if exp)
;;     :else
;;     (error "unknown expression " exp)))

(eval '(define* y* 3))
(eval '(if* (greater* y* 6) (plus* y* 2) 15))


(defn application? [exp] (list? exp))

(def clojure-apply apply)

;; primitive: an ADT that stores native clojure procedures
(def prim-tag 'primitive)
(defn make-primitive [clojure-proc] (list prim-tag clojure-proc))
(defn primitive? [e] (tag-check e prim-tag))
(defn get-clojure-procedure [prim] (second prim))

;; (defn apply [operator operands]
;;   (cond
;;     (primitive? operator)
;;     (clojure-apply (get-clojure-procedure operator) operands)
;;     :else (error "operator not a procedure: " operator)))

;; (swap! environment assoc 'plus* (make-primitive +))
;; (swap! environment assoc 'greater* (make-primitive >))
;; (swap! environment assoc 'true* true)

;; (defn eval [exp]
;;   (cond
;;     (number? exp) exp
;;     (symbol? exp) (lookup exp)
;;     (define? exp) (eval-define exp)
;;     (if? exp) (eval-if exp)
;;     (application? exp) (apply (eval (first exp)) (map eval (rest exp)))
;;     :else
;;     (error "unknown expression " exp)))

(eval '(define* z* 9))
(eval '(plus* 9 6))
(eval '(if* true* 10 15))

;; (defn eval [exp env]
;;   (cond
;;     (number? exp) exp
;;     (symbol? exp) (lookup exp env)
;;     (define? exp) (eval-define exp env)
;;     (if? exp) (eval-if exp env)
;;     (application? exp) (apply (eval (first exp) env)
;;                               (map #(eval % env) (rest exp)))
;;     :else
;;     (error "unknown expression " exp)))

(defn table-get [env name]
  (get @env name :not-found))

;; (defn lookup [name env]
;;   (let [binding (table-get env name)]
;;     (if (= :not-found binding)
;;       (error "unbound variable: " name " in environment " @env)
;;       binding)))

(defn table-put! [env name value]
  (swap! env assoc name value))

;; (defn eval-define [exp env]
;;   (let [name (nth exp 1)
;;         defined-to-be (nth exp 2)]
;;     (table-put! env name (eval defined-to-be env))
;;     'undefined))

(defn eval-if [exp env]
  (let [predicate (nth exp 1)
        first-result (eval predicate env)
        then (nth exp 2)
        else (nth exp 3)]
    (cond (= first-result true) (eval then env)
          (= first-result false) (eval else env)
          :else (error "predicate not a conditional: " predicate))))

;; (eval '(define* z* (plus* 4 5)) environment)
;; (eval '(if* (greater* z* 6) 10 15) environment)

(defn make-compound [parameters body env]
  (list compound-tag parameters body env))

(defn parameters [compound] (nth compound 1))
(defn body [compound] (nth compound 2))
(defn env [compound] (nth compound 3))

(defn eval-lambda [exp env]
  (let [args (nth exp 1)
        body (nth exp 2)]
    (make-compound args body env)))

(defn lambda? [e] (tag-check e 'lambda*))

(defn eval [exp env]
  (cond
    (number? exp) exp
    (symbol? exp) (lookup exp env)
    (define? exp) (eval-define exp env)
    (if? exp) (eval-if exp env)
    (lambda? exp) (eval-lambda exp env)
    (application? exp) (apply (eval (first exp) env)
                              (map #(eval % env) (rest exp)))
    :else
    (error "unknown expression " exp)))


(def compound-tag 'compound)
(defn compound? [e] (tag-check e compound-tag))

(declare make-bindings!)

(defn make-bindings! [names values table]
  (doseq [names values] (fn [name value] (table-put! table name value))))

(defn extend-env-with-new-frame [names values env]
  (let [new-frame (make-table)]
    (make-bindings! names values new-frame)
    (cons new-frame env)))


(defn apply [operator operands]
  (cond
    (primitive? operator)
    (clojure-apply (get-clojure-procedure operator) operands)

    (compound? operator)
    (eval (body operator)
          (extend-env-with-new-frame (parameters operator)
                                     operands
                                     (env operator)))

    :else
    (error "operator not a procedure: " operator)))

(def GE
  (extend-env-with-new-frame
   (list 'plus* 'greater*)
   (list (make-primitive +) (make-primitive >))
   nil))


(eval '(define* twice* (lambda* (x*) (plus* x* x*))))
(eval '(twice* 4))

(defn lookup [name env]
  (if (nil? env)
    (error "unbound variable: " name)
    (let [binding (table-get (first env) name)]
      (if (= :not-found binding)
        (recur name (rest env))
        binding))))

(defn eval-define [exp env]
  (let [name (nth exp 1)
        defined-to-be (nth exp 2)]
    (table-put! (first env) name (eval defined-to-be env))
    'undefined))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

