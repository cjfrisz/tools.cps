(ns clojure.tools.cps
  (:require [clojure.tools.analyzer :as analyze]
            [clojure.tools.analyzer.emit-form :as emit]))

(defn simple-expr?
  [expr]
  (case (:op expr)
    (:number :boolean :constant :string :keyword :fn-expr) true
    :def (simple-expr? (:init expr))
    :if (every? simple-expr? ((juxt :test :then :else) expr))
    :let (every? (comp simple-expr? :init :local-binding)
           (conj (:binding-inits expr) (:body expr)))
    :do (every? simple-expr? (:exprs expr))
    false))
    
(defn cps-expr
  [expr]
  (case (:op expr)
    (:number :boolean :constant :string :keyword) expr
    :def             (throw (Exception. "def expressions not supported yet."))
    :fn-expr         (throw (Exception. "fn expressions not supported yet."))
    :if              (throw (Exception. "if expressions not supported yet."))
    :do              (throw (Exception. "do expressions not supported yet."))
    :let             (throw (Exception. "let expressions not supported yet."))
    :invoke          (throw (Exception. "invoke expressions not supported yet."))
    :recur           (throw (Exception. "recur expressions not supported yet."))
    :throw           (throw (Exception. "throw expressions not supported yet."))
    :try             (throw (Exception. "try expressions not supported yet."))
    :monitor-enter   (throw (Exception. "monitor-enter expressions not supported yet."))
    :monitor-exit    (throw (Exception. "monitor-exit expressions not supported yet."))
    :instance-method (throw (Exception. "instance methods invokes not supported yet."))
    :new             (throw (Exception. "new expressions not supported yet."))
    :static-method   (throw (Exception. "static-method invokes not supported yet."))
    (throw (Exception. (str "unsupported expression " expr)))))

(defmacro cps
  [expr]
  (let [analyzed (analyze/analyze-form (analyze/macroexpand expr))]
    (emit/emit-form (cps-expr analyzed))))
