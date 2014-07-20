(ns section-2-5-3.polar
  (:gen-class)
  (:require [section-2-5-3.math :refer :all]
            [section-2-5-3.module :refer :all]
            ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; polar module
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn install-polar-package []
  (letfn [(magnitude [z] (first z))
          (angle [z] (second z))
          (make-from-mag-ang [mag ang]
            (cons mag ang))
          (real-part [z]
            (mul (magnitude z) (cosine (angle z))))
          (imag-part [z]
            (mul (magnitude z) (sine (angle z))))
          (make-from-real-imag [real imag]
            (cons (Math/sqrt (+ (square real) (square imag)))
                  (Math/atan2 imag real)))
          (tag [x] (attach-tag :polar x))
          (make-from-mag-ang [r a] (list r a))
          (=zero? [z] (= (magnitude z) 0))]
    (put-op :equ? '(:polar :polar) =)
    (put-op :=zero? '(:polar) #(= (magnitude %1) 0))
    (put-op :real-part '(:polar) real-part)
    (put-op :imag-part '(:polar) imag-part)
    (put-op :magnitude '(:polar) magnitude)
    (put-op :angle '(:polar) angle)
    (put-op :make-from-real-imag :polar
            (fn [x y] (tag (make-from-real-imag x y))))
    (put-op :make-from-mag-ang :polar (fn [r a] (tag (make-from-mag-ang r a))))
    :done))

