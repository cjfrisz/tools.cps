(ns clojure.tools.cps
  (:require [clojure.tools.analyzer :as analyze]
            [clojure.tools.analyzer.emit-form :as emit]))

;; NB: probably deficient in the cases covered
(defn trivial-expr?
  [expr]
  (case (:op expr)
    (:number
     :boolean
     :constant
     :string
     :keyword
     :fn-expr
     :local-binding-expr)
    ,true
    :def (trivial-expr? (:init expr))
    :if (every? trivial-expr? ((juxt :test :then :else) expr))
    :let (every? (comp trivial-expr? :init :local-binding)
           (conj (:binding-inits expr) (:body expr)))
    :do (every? trivial-expr? (:exprs expr))
    ;; NB: probably too general; might bite me later
    :static-method (every? trivial-expr? (:args expr))
    false))
    
(def serious-expr? (complement trivial-expr?))

(defn make-fresh-var
  ([] (make-fresh-var 'g))
  ([name] {:op :local-binding,
           ;; NB: consider passing in programs ns info
           ;; NB: the above comment suggests a *ton* of work for error-
           ;; NB: reporting consistency
           :env {:locals {} :ns {:name 'clojure.tools.cps}},
           :sym (gensym name),
           :tag nil,
           :init nil}))

;; NB: there's a nasty number of calls to apply in here.
;; NB: might letfn-bind the main part of the work so the rest
;; NB: args are only passed in once
(defn update-in+
  [coll key* f & arg*]
    (if-let [key (first key*)]
      (let [key* (next key*)]
        (if (identical? key first)
            (conj (next coll) (apply update-in+ (first coll) key* f arg*))
            (assoc coll key (apply update-in+ (get coll key) key* f arg*))))
      (apply f coll arg*)))

(defn build-invoke-expr
  [rator rand]
  {:args
   (list rand),
   :op :invoke,
   :protocol-on nil,
   :is-protocol false,
   :fexpr
   rator,
   :is-direct false,
   :env
   ;; NB: consider flowing useful source information here
   {:source "unknown",
    :column nil,
    :line nil,
    :locals {},
    :ns {:name 'clojure.tools.cps}},
   :site-index -1,
   :tag nil})

(defn unsupported!
  [expr-str]
  (throw (Exception. (str expr-str " expressions not supported yet"))))

;; NB: assuming single-body input fn expressions for now
;; NB: assuming only required symbol arguments (no rest or arg destructuring)
;; NB: also eschewing compatibility for non-CPS calls
(defn cps-fn
  [expr]
  (let [k (make-fresh-var 'k)]
    (-> expr
        (update-in+ [:methods first :required-params] conj k)
        (update-in+ [:methods first :body :exprs first]
          #(if (trivial-expr? %)
               (build-invoke-expr k %)
               (unsupported! "serious expressions in fn bodies"))))))
    
(defn cps-expr
  [expr]
  (case (:op expr)
    (:number :boolean :constant :string :keyword) expr
    :def             (unsupported! "def")
    :fn-expr         (cps-fn expr)
    :if              (unsupported! "if")
    :do              (unsupported! "do")
    :let             (unsupported! "let")
    :invoke          (unsupported! "invoke")
    :recur           (unsupported! "recur")
    :throw           (unsupported! "throw")
    :try             (unsupported! "try")
    :monitor-enter   (unsupported! "monitor-enter")
    :monitor-exit    (unsupported! "monitor-exit")
    :instance-method (unsupported! "instance methods invokes")
    :new             (unsupported! "new")
    :static-method   (unsupported! "static-method invokes")
    (throw (Exception. (str "unexpected expression " expr)))))

(defmacro cps
  [expr]
  (-> expr
      analyze/macroexpand
      analyze/analyze-form
      cps-expr
      emit/emit-form))
