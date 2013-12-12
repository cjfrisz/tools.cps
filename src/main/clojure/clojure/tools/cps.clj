(ns clojure.tools.cps
  (:require [clojure.tools.analyzer :as analyze]))

(defn cps-expr?
  [expr]
  (case (:op expr)
    :number          true
    :boolean         true
    :constant        true  ; quoted values...and set! I guess
    :var             true
    :string          true
    :keyword         true
    :def             true
    :fn-expr         true
    :if              true
    :do              true
    :let             true
    :invoke          true  ; function application & loop
    :recur           true
    :throw           true
    :try             true
    :monitor-enter   true
    :monitor-exit    true
    :instance-method true
    :new             true
    :static-method   true  ; primitives, i.e. +
    false))

(defmacro cps-test
  [expr]
  (let [analyzed (analyze/analyze-form (analyze/macroexpand expr))]
    (clojure.pprint/pprint analyzed)
    (cps-expr? analyzed)))
