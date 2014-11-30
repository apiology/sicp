(ns section-3-5-4.memo-proc)

(defn- already-run? [state]
  (first state))

(defn- result-of-last-run [state]
  (second state))

(defn memo-proc [proc]
  (let [state-atom (atom [false nil])]
    (fn []
      (let [state @state-atom
            already-run (already-run? state)
            result (result-of-last-run state)]
        (if (not already-run)
          (do (reset! state-atom [true (proc)])
              (result-of-last-run @state-atom))
          result)))))
