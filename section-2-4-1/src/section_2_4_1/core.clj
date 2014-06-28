(ns section-2-4-1.core)

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))

(defn square [x] (* x x))

;; complex number representations:

;;;; REPRESENTATION A

;; z = x + i y
;;
;; (think of x,y coordinates on a graph, where x is the real and y is
;; the imaginary)
;;
;; Real-part(z1+z2) = Real-Part(z1) + Real-part(z2)
;; Complex-part(z1+z2) = Complex-part(z1) + Complex-part(z2)

(defn real-part-rectangular [z] (first z))

(defn imag-part-rectangular [z] (second z))

(defn magnitude-rectangular [z]
  (Math/sqrt (+ (square (real-part-rectangular z))
                (square (imag-part-rectangular z)))))

(defn angle-rectangular [z]
  (Math/atan2 (imag-part-rectangular z)
              (real-part-rectangular z)))

(defn make-from-real-imag-rectangular [real imag]
  (cons real imag))

(defn make-from-mag-ang-rectangular [mag ang]
  (cons (* mag (Math/cos ang)) (* mag (Math/sin ang))))

;;;; REPRESENTATION B

;; magnitude and angle

;; keep thinking of x,y on a graph; this now represents angle from zero.

;; real numbers have magnitude of themselves and angle of zero.

;; Magnitude(z1 * z2) = Magnitude(z1) * Magnitude(z2)
;; Angle(z1 * z2) = Angle(z1) + Angle(z2)

(defn angle-polar [z] (second z))

(defn magnitude-polar [z] (first z))

(defn real-part-polar [z]
  (* (magnitude-polar z) (Math/cos (angle-polar z))))

(defn imag-part-polar [z]
  (* (magnitude-polar z) (Math/sin (angle-polar z))))

(defn make-from-real-imag-polar [real imag]
  (cons (Math/sqrt (+ (square real) (square imag)))
        (Math/atan2 imag real)))

(defn make-from-mag-ang-polar [mag ang]
  (cons mag ang))

;;;; HANDLES BOTH

(defn type-tag [datum]
  (if (seq datum)
    (first datum)
    (throw (IllegalArgumentException. (str "Bad tagged dataum -- TYPE-TAG "
                                           datum)))))

(defn rectangular? [z]
  (= (type-tag z) :rectangular))

(defn polar? [z]
  (= (type-tag z) :polar))

(defn contents [datum]
  (if (seq datum)
    (second datum)
    (throw (IllegalArgumentException. (str "Bad tagged dataum -- CONTENTS "
                                           datum)))))

(defn real-part [z]
  (cond (rectangular? z) (real-part-rectangular (contents z))
        (polar? z) (real-part-polar (contents z))
        :else
        (throw (IllegalArgumentException. (str "Bad tagged dataum - "
                                           z)))))

(defn angle [z]
  (cond (rectangular? z) (angle-rectangular (contents z))
        (polar? z) (angle-polar (contents z))
        :else
        (throw (IllegalArgumentException. (str "Bad tagged dataum - "
                                           z)))))

(defn magnitude [z]
  (cond (rectangular? z) (magnitude-rectangular (contents z))
        (polar? z) (magnitude-polar (contents z))
        :else
        (throw (IllegalArgumentException. (str "Bad tagged dataum - "
                                           z)))))

(defn imag-part [z]
  (cond (rectangular? z) (imag-part-rectangular (contents z))
        (polar? z) (imag-part-polar (contents z))
        :else
        (throw (IllegalArgumentException. (str "Bad tagged dataum - "
                                           z)))))

(defn make-from-real-imag [real imag]
  (make-from-real-imag-rectangular real imag))

(defn make-from-mag-ang [mag ang]
  (make-from-mag-ang-polar mag ang))

;;;; REPRESENTATION INDEPENDENT

(defn add-complex [z1 z2]
  (make-from-real-imag (+ (real-part z1) (real-part z2))
                       (+ (imag-part z1) (imag-part z2))))

(defn sub-complex [z1 z2]
  (make-from-real-imag (- (real-part z1) (real-part z2))
                       (- (imag-part z1) (imag-part z2))))

(defn mul-complex [z1 z2]
  (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                     (+ (angle z1) (angle z2))))

(defn div-complex [z1 z2]
  (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                     (- (angle z1) (angle z2))))

;; x = r cos A
;; r = sqrt(x*x + y*y)

;; y = r sin A
;; A = arctan(y, x)

(defn attach-tag [type-tag contents]
  (cons type-tag contents))



(defn make-from-real-imag-rectangular [real imag]
  (attach-tag :rectangular (list real imag)))

(defn make-from-mag-ang-rectangular [mag ang]
  (attach-tag :rectangular
              (cons (* mag (Math/cos ang)) (* mag (Math/sin ang)))))

(defn make-from-real-imag-polar [real imag]
  (attach-tag :polar
              (cons (Math/sqrt (+ (square real) (square imag)))
                    (Math/atan2 imag real))))

(defn make-from-mag-ang-polar [mag ang]
  (attach-tag :polar (list mag ang)))

(defn real-part [z]
  (cond (rectangular? z) (real-part-rectangular (contents z))
        (polar? z) (real-part-polar (contents z))
        :else (throw (IllegalArgumentException.
                      (str "Unknown type - REAL-PART" z)))))

(defn imag-part [z]
  (cond (rectangular? z) (imag-part-rectangular (contents z))
        (polar? z) (imag-part-polar (contents z))
        :else (throw (IllegalArgumentException.
                      (str "Unknown type - IMAG-PART" z)))))

(defn magnitude [z]
  (cond (rectangular? z) (magnitude-rectangular (contents z))
        (polar? z) (magnitude-polar (contents z))
        :else (throw (IllegalArgumentException.
                      (str "Unknown type - MAGNITUDE" z)))))

(defn angle [z]
  (cond (rectangular? z) (angle-rectangular (contents z))
        (polar? z) (angle-polar (contents z))
        :else (throw (IllegalArgumentException.
                      (str "Unknown type - ANGLE" z)))))


;; now, in a data-directed style...

(def operations (atom {}))

(defn put-op [op-sym type-tags op-fn]
  (swap! operations assoc (cons op-sym type-tags) op-fn))

(defn get-op [op-sym type-tags]
  (get @operations (cons op-sym type-tags)))

(defn apply-generic [op & args]
  (let [type-tags (map type-tag args)
        proc (get-op op type-tags)]
    (if proc
      (apply proc (map contents args))
      (throw (IllegalArgumentException.
              (str "No method for these types - apply-generic"
                   (list op type-tags)))))))

(defn real-part [z]
  (apply-generic 'real-part z))

(defn imag-part [z]
  (apply-generic 'imag-part z))

(defn magnitude [z]
  (apply-generic 'magnitude z))

(defn angle [z]
  (apply-generic 'angle z))

(defn make-from-real-imag [x y]
  ((get-op 'make-from-real-imag 'rectangular) x y))

(defn make-from-mag-ang [r a]
  ((get-op 'make-from-mag-ang 'polar) r a))


(defn install-rectangular-package []
  (defn real-part [z] (first z))
  (defn imag-part [z] (second z))
  (defn make-from-real-imag [real imag]
    (cons real imag))
  (defn magnitude [z]
    (Math/sqrt (+ (square (real-part z))
                  (square (imag-part z)))))
  (defn angle [z]
    (Math/atan2 (imag-part-rectangular z)
                (real-part-rectangular z)))
  (defn make-from-mag-ang [mag ang]
    (cons (* mag (Math/cos ang)) (* mag (Math/sin ang))))
  (defn tag [x] (attach-tag 'rectangular x))
  (put-op 'real-part '(rectangular) real-part)
  (put-op 'imag-part '(retangular) imag-part)
  (put-op 'magnitude '(retangular) magnitude)
  (put-op 'angle '(rectangular) angle)
  (put-op 'make-from-real-imag 'rectangular
       (fn [x y] (tag (make-from-real-imag x y))))
  (put-op 'make-from-mag-ang 'rectangular
       (fn [r a] (tag (make-from-mag-ang r a))))
  'done)

(defn install-polar-package []
  (defn magnitude [z] (first z))
  (defn angle [z] (second z))
  (defn make-from-mag-ang [mag ang]
    (cons mag ang))
  (defn real-part [z]
    (* (magnitude z) (Math/cos (angle z))))
  (defn imag-part [z]
    (* (magnitude z) (Math/sin (angle z))))
  (defn make-from-real-imag [real imag]
    (cons (Math/sqrt (+ (square real) (square imag)))
          (Math/atan2 imag real)))
  (defn tag [x] (attach-tag 'polar x))
  (put-op 'real-part '(polar) real-part)
  (put-op 'imag-part '(polar) imag-part)
  (put-op 'magnitude '(polar) magnitude)
  (put-op 'angle '(polar) angle)
  (put-op 'make-from-real-imag 'polar
          (fn [x y] (tag (make-from-real-imag x y))))
  (put-op 'make-from-mag-ang 'polar (fn [r a] (tag (make-from-mag-ang r a))))
  'done)

;; Exercise 2.73
