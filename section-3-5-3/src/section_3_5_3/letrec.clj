(ns section-3-5-3.letrec
  (:require [clojure.tools.macro :as mt]))

;; http://grokbase.com/t/gg/clojure/11ced7fdr2/letrec
(defmacro letrec [binding & body]
  (let [[var expr] binding
        g-var (gensym)]
    `(let [~g-var (promise)]
       (mt/symbol-macrolet [~var @~g-var]
                        (deliver ~g-var ~expr)
                        ~@body))))
