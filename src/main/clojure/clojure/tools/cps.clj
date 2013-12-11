(ns clojure.tools.cps
  (:require [clojure.tools.analyzer :as analyze]))

(def cps-expr? (constantly false))

(defmacro cps-test
  [expr]
  (let [analyzed (analyze/macroexpand `(analyze/analyze-form ~expr))]
    (cps-expr? analyzed)))
