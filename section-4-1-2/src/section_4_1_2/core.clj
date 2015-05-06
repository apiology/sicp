(ns section-4-1-2.core
  (:refer-clojure :only
                  [= comment cond cons declare defn empty? first if-not
                   let list list? nil? not ns nth number? println
                   rest second seq str string? symbol? atom filter
                   -> ->> if-let swap! conj fn > count or reset!]))

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))

(defn error [& msg] (throw (IllegalStateException. ^java.lang.String (clojure.core/apply str msg))))

(declare eval)
;; XXX selective import of clojure forms

;; false not bound in global environment yet--waiting patiently for
;; section 4.1.4, but in the meantime, this helps test the rest
(defn boolean? [exp]
  (or (= true exp)
      (= false exp)))

(defn self-evaluating? [exp]
  (cond
    (number? exp) true
    (string? exp) true
    (boolean? exp) true
    :else false))

(defn variable? [exp]
  (symbol? exp))

(defn tagged-list? [exp tag]
  (if (list? exp)
    (= (first exp) tag)
    false))

(defn quoted? [exp]
  (tagged-list? exp 'quote))

(defn text-of-quotation [exp]
  (second exp))

(defn assignment? [exp]
  (tagged-list? exp 'set!))

(defn assignment-variable [exp]
  (nth exp 1))

(defn assignment-value [exp]
  (nth exp 2))

(defn lambda? [exp]
  (tagged-list? exp 'lambda))

(defn lambda-parameters [exp]
  (nth exp 1))

(defn lambda-body [exp]
  (nth exp 2))

(defn make-lambda [parameters body]
  (cons 'lambda (cons parameters body)))

(defn definition? [exp]
  (tagged-list? exp 'define))

(defn definition-variable [exp]
  (if (symbol? (second exp))
    ;; variable definition
    (second exp)
    ;; procedure definition
    (first (second exp))))

(defn definition-value [exp]
  (if (symbol? (second exp))
    ;; variable definition
    (nth exp 2)
    ;; procedure definition
    (make-lambda (rest (nth exp 1)) ;; formal parameters
                 (nth exp 2)))) ;; body

(defn if? [exp]
  (tagged-list? exp 'if))

(defn if-predicate [exp]
  (nth exp 1))

(defn if-consequent [exp]
  (nth exp 2))

(defn if-alternative [exp]
  (if (> (count exp) 3)
    (nth exp 3)
    false))
;; (if-alternative '(if 1 true))

(defn make-if [predicate consequent alternative]
  (list 'if predicate consequent alternative))

(defn or? [exp]
  (tagged-list? exp 'or))

(defn or-exps [exp]
  (rest exp))

(defn eval-or [exp env]
  (let [exps (or-exps exp)]
    (if (empty? exps)
      'false
      (let [left (first exps)
            left-value (eval left env)]
        (if (true? left-value)
          left-value
          (eval-or (cons 'or (rest exps)) env))))))
    
(defn begin? [exp]
  (tagged-list? exp 'begin))

(defn begin-actions [exp] (rest exp))

(defn last-exp? [seq] (empty? (rest seq)))

(defn first-exp [seq] (first seq))

(defn rest-exps [seq] (rest seq))

(defn make-begin [seq]
  (cons 'begin seq))

(defn sequence->exp [seq]
  (cond 
    (empty? seq) seq
    (last-exp? seq) (first-exp seq)
    :else (make-begin seq)))

(defn application? [exp]
  (list? exp))

(defn operator [exp]
  (first exp))

(defn operands [exp]
  (rest exp))

(defn no-operands? [ops]
  (empty? ops))

(defn first-operand [ops]
  (first ops))

(defn rest-operands [ops]
  (rest ops))

(defn cond? [exp]
  (tagged-list? exp 'cond))

(defn cond-clauses [exp]
  (rest exp))

(defn cond-predicate [clause]
  (first clause))

(defn cond-else-clause? [clause]
  (= (cond-predicate clause) 'else))

(defn cond-actions [clause]
  (rest clause))

(defn expand-clauses [clauses]
  (if (empty? clauses)
    'false
    (let [first-clause (first clauses)
          rest-clauses (rest clauses)]
      (if (cond-else-clause? first)
        (if (empty? rest-clauses)
          (sequence->exp (cond-actions first-clause))
          (error "ELSE clause isn't last -- COND->IF" clauses))
        (make-if (cond-predicate first)
                 (sequence->exp (cond-actions first))
                 (expand-clauses rest-clauses))))))

(defn cond->if [exp]
  (expand-clauses (cond-clauses exp)))

(defn list-of-values
  "Evaluates each item in the list of expressions and returns a list
  of values back"
  [exps env]
  (if (no-operands? exps)
    '()
    (cons (eval (first-operand exps) env)
          (list-of-values (rest-operands exps) env))))

;; Exercise 4.1
(defn list-of-values-left-eval
  "Evaluates each item in the list of expressions and returns a list
  of values back"
  [exps env]
  (if (no-operands? exps)
    '()
    (let [left-value (eval (first-operand exps) env)]
      (let [rest-values (list-of-values (rest-operands exps) env)]
        (cons left-value rest-values)))))

;; Exercise 4.1
(defn list-of-values-right-eval
  "Evaluates each item in the list of expressions and returns a list
  of values back"
  [exps env]
  (if (no-operands? exps)
    '()
    (let [rest-values (list-of-values (rest-operands exps) env)]
      (let [left-value (eval (first-operand exps) env)]
        (cons left-value rest-values)))))

(defn true? [cond]
  (if (number? cond)
    (> cond 0)
    (if (= true cond)
      true
      (if (= false cond)
        false
        (error "true? not implemented on " cond)))))

(defn eval-if [exp env]
  (if (true? (eval (if-predicate exp) env))
    (eval (if-consequent exp) env)
    (eval (if-alternative exp) env)))

(defn eval-sequence
  "Used for (begin) and for the body of a function--can be more than
  one expression in a row"
  [exps env]
  (cond 
    (last-exp? exps) (eval (first-exp exps) env)
    :else (do
            (eval (first-exp exps) env)
            (eval-sequence (rest-exps exps) env))))

(defn set-variable-value! [symbol value env]
  (error "set-variable-value! not yet implemented"))

(defn eval-assignment [exp env]
  ;; XXX I think this is a scheme primitive
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  :ok)

(defn define-variable! [symbol value env]
  (error "define-variable! not yet implemented"))

(defn lookup-variable-value [symbol env]
  (error "lookup-variable-value not yet implemented"))


(defn eval-definition [exp env]
  (define-variable! (definition-variable exp)
    (eval (definition-value exp) env)
    env)
  :ok)

(defn primitive-procedure? [exp]
  (error "primitive-procedure? not yet implemented"))

(defn apply-primitive-procedure [procedure arguments]
  (error "apply-primitive-procedure not yet implemented"))

(defn compound-procedure? [exp]
  (error "compound-procedure? not yet implemented"))

(defn procedure-body [procedure]
  (error "procedure-body not yet implemented"))

(defn procedure-parameters [procedure]
  (error "procedure-body not yet implemented"))

(defn procedure-environment [procedure]
  (error "procedure-environment not yet implemented"))

(defn make-procedure [parameters body env]
  (error "make-procedure not yet implemented"))

(defn extend-environment [variables values existing-env]
  (error "extend-environment not yet implemented"))



(defn apply [procedure arguments]
  (cond
    (primitive-procedure? procedure) (apply-primitive-procedure procedure arguments)
    (compound-procedure? procedure) (eval-sequence
                                     (procedure-body procedure)
                                     (extend-environment
                                      (procedure-parameters procedure)
                                      arguments
                                      (procedure-environment procedure)))
    :else (error "Unknown procedure type -- APPLY" procedure)))

(comment original - see below for new
  (defn eval [exp env]
    (cond
      (self-evaluating? exp) exp
      (variable? exp) (lookup-variable-value exp env)
      (quoted? exp) (text-of-quotation exp)
      (assignment? exp) (eval-assignment exp env)
      (definition? exp) (eval-definition exp env)
      (if? exp) (eval-if exp env)
      (lambda? exp) (make-procedure (lambda-parameters exp)
                                    (lambda-body exp)
                                    env)
      (begin? exp) (eval-sequence (begin-actions exp) env)
      (cond? exp) (eval (cond->if exp) env)
      (application? exp) (apply (eval (operator exp) env)
                                (list-of-values (operands exp) env))
      :else (error "unknown expression type -- EVAL" exp))))

;; Exercise 4.2

;; a

;; Won't work--special forms like (define) need to be special forms
;; and not regular function calls


;; b

;;
;; to do that:
;;
;; (defn application? [exp]
;;   (tagged-list? exp 'call))

;; (defn operator [exp]
;;   (second exp))

;; (defn operands [exp]
;;   (rest (rest exp)))

;; Exercise 4.3

(def forms (atom []))

(defn add-form [pred action]
  (swap! forms conj [pred action]))

(defn install-all-forms []
  (add-form self-evaluating? (fn [exp env] exp))
  (add-form variable? lookup-variable-value)
  (add-form quoted? (fn [exp env] (text-of-quotation exp)))
  (add-form assignment? eval-assignment)
  (add-form definition? eval-definition)
  (add-form if? eval-if)
  ;; (add-form and? eval-and)
  (add-form or? eval-or)
  (add-form lambda? (fn [exp env]
                      (make-procedure (lambda-parameters exp)
                                      (lambda-body exp)
                                      env)))
  (add-form begin? (fn [exp env]
                     (eval-sequence (begin-actions exp) env)))
  (add-form cond? (fn [exp env]
                    (eval (cond->if exp) env)))
  (add-form application? (fn [exp env]
                           (apply (eval (operator exp) env)
                                  (list-of-values (operands exp) env)))))


(install-all-forms)

;; (reset! forms [])

(defn action-for-exp [exp]
  (if-let [[pred action] (->> @forms
                             (filter #((first %) exp))
                             first)]
    action))


(defn eval [exp env]
  (if-let [action (action-for-exp exp)]
    (action exp env)
    (error "unknown expression type -- EVAL" exp)))
