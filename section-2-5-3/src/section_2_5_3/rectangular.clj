(ns section-2-5-3.rectangular
  (:gen-class)
  (:require [section-2-5-3.math :refer :all]
            [section-2-5-3.module :refer :all]
            ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rectangular module
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn install-rectangular-package []
  (letfn [(real-part [z] (first z))
          (imag-part [z] (second z))
          (make-from-real-imag [real imag] (list real imag))
          (magnitude [z] (sqrt (add (square (real-part z))
                                    (square (imag-part z)))))
          (angle [z] (atan2 (imag-part z)
                            (real-part z)))
          (make-from-mag-ang [mag ang] (cons (* mag (Math/cos ang))
                                             (* mag (Math/sin ang))))
          (make-from-real-imag [x y] (list x y))
          (tag [x] (attach-tag :rectangular x))]
    (put-op :equ? '(:rectangular :rectangular) =)
    (put-op :=zero? '(:rectangular) (fn [n] (let [r (real-part n) 
                                                  i (imag-part n)]
                                              (and (= r 0) (= i 0)))))
    (put-op :real-part '(:rectangular) real-part)
    (put-op :imag-part '(:rectangular) imag-part)
    (put-op :magnitude '(:rectangular) magnitude)
    (put-op :angle '(:rectangular) angle)
    (put-op :make-from-real-imag :rectangular
            (fn [x y] (tag (make-from-real-imag x y))))
    (put-op :make-from-mag-ang :rectangular
            (fn [r a] (tag (make-from-mag-ang r a))))
    (put-op :project-one-step '(:rectangular) #(real-part %1))
    :done))
