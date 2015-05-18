(ns section-4-1-2.global-environment
  (:require [section-4-1-2.binding :as binding]
            [section-4-1-2.environment :as environment]))

(def primitive-procedures
  (list (list 'car first)
        (list 'cdr rest)
        (list 'cons cons)
        (list 'null? empty?)
        (list '= =)
        (list '+ +)
        (list '* *)
        (list '/ /)
        (list '- -)
        (list '> >)
        (list '< <)
        (list 'inc inc)
        (list 'println println)
        ))

(defn primitive-procedure-names []
  (map first
       primitive-procedures))

(defn primitive-procedure-objects []
  (map (fn [proc] (list 'primitive (second proc)))
       primitive-procedures))

(defn setup-environment []
  (let [initial-env
        (environment/extend-environment (primitive-procedure-names)
                                        (primitive-procedure-objects)
                                        environment/the-empty-environment)]
        (binding/define-variable! 'true true initial-env)
        (binding/define-variable! 'false false initial-env)
        initial-env))

(def the-global-environment (setup-environment))
