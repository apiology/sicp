(ns section-4-1-2.types
  (:require [clojure.core.typed :as t]))

(t/defalias Var t/Symbol)

(t/defalias Variables (t/Seqable Var))

(t/defalias RawVal t/Any)

(t/defalias RawValues (t/Seqable RawVal))

(t/defalias Values (t/Seqable (t/Atom1 RawVal)))

(t/defalias Frame (t/Atom1 (t/HVec [Variables Values])))

(t/defalias Environment (t/Option (t/Seqable Frame)))

(t/defalias Primitive (t/U t/Symbol Number String))

(t/defalias Combination (t/List Expression))

(t/defalias Expression (t/U Primitive Combination))

(t/defalias Expressions (t/Seqable Expression))

(t/defalias Sequence  (t/U Expressions Primitive))

(t/defalias EvalFn [Expression Environment -> Expression])

(t/defalias ApplyFn [Expression Expressions -> Expression])

