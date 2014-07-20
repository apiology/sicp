(ns section-2-5-3.complex
  (:gen-class)
  (:require [section-2-5-3.module :refer :all]
            [section-2-5-3.math :refer :all]
            [section-2-5-3.module :refer :all]
            [section-2-5-3.rational :refer [make-rational]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; define additional generic operations
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn real-part [z]
  (apply-generic :real-part z))

(defn imag-part [z]
  (apply-generic :imag-part z))

(defn magnitude [z]
  (apply-generic :magnitude z))

(defn angle [z]
  (apply-generic :angle z))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; complex module
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn install-complex-package []
  (let [make-from-real-imag #((get-op-or-fail :make-from-real-imag :rectangular) %1 %2)
        make-from-mag-ang #((get-op-or-fail :make-from-mag-ang :polar) %1 %2)
        ;; internal procedures
        add-complex (fn [z1 z2]
                      (make-from-real-imag (add (real-part z1) (real-part z2))
                                           (add (imag-part z1) (imag-part z2))))
        sub-complex (fn [z1 z2]
                      (make-from-real-imag (sub (real-part z1) (real-part z2))
                                           (sub (imag-part z1) (imag-part z2))))
        mul-complex (fn [z1 z2]
                      (make-from-mag-ang (mul (magnitude z1) (magnitude z2))
                                         (add (angle z1) (angle z2))))
        div-complex (fn [z1 z2]
                      (make-from-mag-ang (div (magnitude z1) (magnitude z2))
                                         (sub (angle z1) (angle z2))))
        tag #(attach-tag :complex %)]
    (put-op :add '(:complex :complex) #(tag (add-complex %1 %2)))
    (put-op :sub '(:complex :complex) #(tag (sub-complex %1 %2)))
    (put-op :mul '(:complex :complex) #(tag (mul-complex %1 %2)))
    (put-op :div '(:complex :complex) #(tag (div-complex %1 %2)))
    (put-op :equ? '(:complex :complex) #(equ? %1 %2))
    (put-op :=zero? '(:complex) =zero?)
    (put-op :make-from-real-imag :complex #(tag (make-from-real-imag %1 %2)))
    (put-op :make-from-mag-ang :complex #(tag (make-from-mag-ang %1 %2)))
    ;; below added as part of Exercise 2.77
    (put-op :real-part '(:complex) real-part)
    (put-op :imag-part '(:complex) imag-part)
    (put-op :magnitude '(:complex) magnitude)
    (put-op :angle '(:complex) angle)
    (put-op :lower-type :complex (fn [] :rational))
    (put-op :project-one-step '(:complex) #(project-one-step %1))
    (put-op :raise '(:rational) #(tag (make-from-real-imag (apply make-rational %1) 0)))
    'done))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn make-complex-from-real-imag [x y]
  ((get-op-or-fail :make-from-real-imag :complex) x y))

(defn make-complex-from-mag-ang [x y]
  ((get-op-or-fail :make-from-mag-ang :complex) x y))

