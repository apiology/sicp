(ns section-2-5-3.module
  (:gen-class)
  (require [section-2-5-3.log :refer :all]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; module infrastructure
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def operations (atom {}))

(defn put-op [op-sym type-tags op-fn]
  "Add to the global list of operations based on the function
   name (op-sym), a list of type tags describing the arguments of the
   type, and the function (op-fn) which should be called if matched"
  (swap! operations assoc (list op-sym type-tags) op-fn))

(defn get-op [op-sym type-tags]
  (let [op (get @operations (list op-sym type-tags))] 
    (when-not (nil? op) ;; XXX kibit rule
      op)))

(defn get-op-or-fail [op-sym type-tags]
  (if-let [op (get-op op-sym type-tags)]
    op
    (throw (IllegalArgumentException. (str "Could not find operation " op-sym " on type tags " (types-to-str type-tags))))))

(defn type-tag [datum]
  (cond (number? datum) :clj-number
        (seq? datum) (first datum)
        :else (throw (IllegalArgumentException. (str "Bad tagged dataum -- TYPE-TAG "
                                                     datum)))))

(defn attach-tag [type-tag contents]
  (if (= type-tag :clj-number)
    contents
    (list type-tag contents)))

(defn contents [datum]
  (if (= :clj-number (type-tag datum))
    datum
    (second datum)))

(declare apply-generic-no-simplify)
(declare drop-type)

(defn apply-generic [op & args]
  (log "(apply-generic " op args ")")
  (drop-type (apply apply-generic-no-simplify op args)))

(defn cant-resolve-op [op types]
  (throw (Exception. (str "Could not find op " op 
                          " with tags " (types-to-str types) ".  Note that type of that is "
                          (class types)
                          ").  Valid tags would be " (keys @operations)))))

(defn lower-type [type]
  (when-let [fn (get-op :lower-type type)]
    (fn)))

(defn is-lower-type? [type-1 type-2]
  (if-let [lower-type-2 (lower-type type-2)]
    (if (= type-1 lower-type-2)
      true
      (recur type-1 lower-type-2))
    false))

(defn type-comparator [type-1 type-2]
  (if (= type-1 type-2)
    0
    (if (is-lower-type? type-1 type-2)
      -1
      1)))

(defn find-lowest-type [type-tags]
  (first (sort type-comparator type-tags)))

(defn find-index-of-type-to-raise [type-tags]
  (.indexOf type-tags (find-lowest-type type-tags)))

(declare equ?)
(declare raise)

(defn simple-apply-generic-or-nil [op & args]
  (let [type-tags (map type-tag args)
        proc (get-op op type-tags)]
    (when proc
      (apply proc (map contents args)))))

(defn project-one-step [x] 
  (log "(project-one-step " x ")")
  (simple-apply-generic-or-nil :project-one-step x))

(defn drop-item-one-step [num]
  (if-let [projected (project-one-step num)]
    (let [raised (raise projected)]
      (log "drop-item-one-step: For num=" num ", projected is "
           projected ", raised is " raised)
      (if (equ? num raised)
        projected
        nil))))

(defn drop-type [num]
  (log "(drop-type " num ")")
  (if-let [next-step (drop-item-one-step num)]
    (recur next-step)
    num))

(defn raise-one-step [args]
  (let [type-tags-vec (vec (map type-tag args))
        args-vec (vec args)
        index-to-raise (find-index-of-type-to-raise type-tags-vec)
        arg-to-raise (nth args index-to-raise nil)]
    (log "type-tags-vec: " (types-to-str type-tags-vec))
    (log "arg-to-raise: " arg-to-raise)
    (when arg-to-raise
      (when-let [raised-arg (raise arg-to-raise)]
        (log "raised-arg: " raised-arg)
        (let [ret (seq (assoc args-vec index-to-raise raised-arg))]
          (log "(raise-one-step " (args-to-str args) " is returning " (coll-to-str ret))
          ret)))))

(defn apply-generic-no-simplify-or-nil [op & args]
  (loop [current-args args]
    (log "Trying apply-generic for " op " with args " (args-to-str current-args))
    (if-not (nil? current-args)
      (let [type-tags (map type-tag current-args)
            proc (get-op op type-tags)]
        (if proc
          (apply proc (map contents current-args))
          (do 
            (let [next-args (raise-one-step current-args)]
              (log "Couldn't figure out an op with current args--raising one step from "
                   (args-to-str current-args) " to " (args-to-str next-args))
              (recur next-args))))))))

(defn apply-generic-or-nil [op & args]
  (let [unsimplified (apply apply-generic-no-simplify-or-nil op args)]
    (if-not (nil? unsimplified)
      (drop-type unsimplified))))

(defn apply-generic-no-simplify [op & args]
  (let [ret (apply apply-generic-no-simplify-or-nil op args)]
    (if (nil? ret)
      (cant-resolve-op op (map type-tag args))
      ret)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; generic, non-math-specific virtual functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn raise [num]
  (log "Calling (raise " num ")")
  (let [ret (simple-apply-generic-or-nil :raise num)]
    (log "(raise " num ") = " ret)
    ret))

(defn equ? [a b]
  (log "Call equ? on " a " and " b)
  (apply-generic-no-simplify :equ? a b))
