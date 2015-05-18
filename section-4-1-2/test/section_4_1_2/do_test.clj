(ns section-4-1-2.do-test
  (:require [clojure.test :refer :all]
            [section-4-1-2.core :refer :all]
            [section-4-1-2.do :as do]
            [section-4-1-2.test-env :refer :all])
  (:refer-clojure :only [= fn with-redefs]))

(deftest simple-do-loop-translated
  (testing "do-loop"
    (with-redefs [clojure.core/gensym (fn [] 'new-fn)]
      (is (= (do/do->combination '(do ((x 1 (inc x))) ;; note: 1+ doesn't pass clojure reader
                                      ((> x 4) x)))
             '(let new-fn ((x 1))
                   (if (> x 4)
                     x
                     (new-fn (inc x)))))))))


(deftest simple-do-loop-evaled
  (testing "do-loop"
    (is (= (eval '(do ((x 1 (inc x))) ;; note: 1+ doesn't pass clojure reader
                      ((> x 4) x))
                 (test-env))
           5))))



(deftest complicated-do-loop-evaled
  (testing "do-loop"
    (is (= (eval
            '(do ((i 1 (inc i))
                  (p 3 (* 3 p)))
                 ((> i 4)
                  p)
                 (println "hi"))
            (test-env))
           243))))

;; Or with two variables and a final return value,

;; (do ((i 1 (1+ i))
;;      (p 3 (* 3 p)))
;;     ((> i 4)
;;      p)
;;     (format #t "3**~s is ~s\n" i p))
;; -|
;; 3**1 is 3
;; 3**2 is 9
;; 3**3 is 27
;; 3**4 is 81
;; ⇒
;; 789
;; The variable bindings are established like a let, in that the expressions are all evaluated and then all bindings made. When iterating, the optional step expressions are evaluated with the previous bindings in scope, then new bindings all made.

