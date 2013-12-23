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
     :local-binding
     :local-binding-expr
     :var)
    ,true
    :def (trivial-expr? (:init expr))
    :if (every? trivial-expr? ((juxt :test :then :else) expr))
    :let (every? (comp trivial-expr? :init :local-binding)
           (conj (:binding-inits expr) (:body expr)))
    :do (every? trivial-expr? (:exprs expr))
    ;; NB: probably too general; might bite me later
    :static-method (every? trivial-expr? (:args expr))
    false))
    
(defn update-in+
  [coll key* f & arg*]
  (letfn [($update-in+ [coll key*]
            (if-let [key (first key*)]
              (let [key* (next key*)]
                (if (identical? key clojure.core/first)
                    (conj (next coll) ($update-in+ (first coll) key*))
                    (assoc coll key ($update-in+ (get coll key) key*))))
              (apply f coll arg*)))]
    ($update-in+ coll key*)))

(defn make-fresh-var
  ([] (make-fresh-var 'g))
  ([name] {:op :local-binding,
           ;; NB: consider passing in programs ns info
           ;; NB: this suggests a *ton* of work for error-reporting consistency
           :env {:locals {} :ns {:name 'clojure.tools.cps}},
           :sym (gensym name),
           :tag nil,
           :init nil}))

(defn build-invoke-expr
  [rator rand*]
  {:args rand*,
   :op :invoke,
   :protocol-on nil,
   :is-protocol false,
   :fexpr rator,
   :is-direct false,
   :env
   ;; NB: consider flowing useful source information here
   {:source "unknown",
    :column -1,
    :line -1,
    :locals {},
    :ns {:name 'clojure.tools.cps}},
   :site-index -1,
   :tag nil})

(defn build-continuation-expr
  [arg body]
  ;; NB: missing lots of source information here
  {:op :fn-expr,
   :env {:line -1, :locals {}, :ns {:name 'clojure.tools.cps}},
   :methods
   (list {:op :fn-method,
          :env {:locals {}, :ns {:name 'clojure.tools.cps}},
          :body
          {:op :do,
           :env
           {:source "unknown",
            :column -1,
            :line -1,
            :locals {},
            :ns {:name 'clojure.tools.cps}},
           :exprs (list body)},
          :required-params (list arg),
          :rest-param nil}),
   :variadic-method nil,
   :tag nil})

(def empty-continuation
  (let [x (make-fresh-var 'x)]
    (build-continuation-expr x x)))

(declare cps-triv cps-srs)

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
               (build-invoke-expr k (list %))
               (cps-srs % k))))))
    
(defn cps-app
  [expr k]
  (letfn [(trivit [e]
            (if (trivial-expr? e)
                (cps-triv e)
                (make-fresh-var 's)))]
    ;; NB: this is a den of inefficient things
    (loop [rcall* (reverse (conj (:args expr) (:fexpr expr)))
           rtriv* (map trivit rcall*)
           out (build-invoke-expr (last rtriv*)
                 (conj (reverse (butlast rtriv*)) k))]
      (if (and (nil? (seq rcall*)) (nil? (seq rtriv*)))
          out
          (let [[rcall & rcall*] rcall*
                [rtriv & rtriv*] rtriv*]
            (if (trivial-expr? rcall)
                (recur rcall* rtriv* out)
                (recur rcall* rtriv*
                  (cps-srs rcall (build-continuation-expr rtriv out)))))))))
                
(defn error
  [& msg]
  (throw (Exception. (apply str msg))))

(defn unsupported!
  [expr-str]
  (error (str expr-str " expressions not supported yet")))

(defn cps-triv
  [expr]
  (case (:op expr)
    (:var
     :local-binding
     :local-binding-expr
     :number
     :boolean
     :constant
     :string
     :keyword)
    ,expr
    :fn-expr         (cps-fn expr)
    :def             (unsupported! "def")
    :if              (unsupported! "if")
    :do              (unsupported! "do")
    :let             (unsupported! "let")
    :recur           (unsupported! "recur")
    :throw           (unsupported! "throw")
    :try             (unsupported! "try")
    :monitor-enter   (unsupported! "monitor-enter")
    :monitor-exit    (unsupported! "monitor-exit")
    :instance-method (unsupported! "instance method invokes")
    :new             (unsupported! "new")
    :static-method   (unsupported! "static-method invokes")
    (error "unexpected trivial expression " (emit/emit-form expr))))

(defn cps-srs
  [expr k]
  (case (:op expr)
    :invoke          (cps-app expr k)
    :def             (unsupported! "def")
    :if              (unsupported! "if")
    :do              (unsupported! "do")
    :let             (unsupported! "let")
    :recur           (unsupported! "recur")
    :throw           (unsupported! "throw")
    :try             (unsupported! "try")
    :monitor-enter   (unsupported! "monitor-enter")
    :monitor-exit    (unsupported! "monitor-exit")
    :instance-method (unsupported! "instance method invokes")
    :new             (unsupported! "new")
    :static-method   (unsupported! "static-method invokes")
    (error "unexpected serious expression " (emit/emit-form expr))))

(defn cps-expr
  [expr]
  (case (:op expr)
    (:var
     :number
     :boolean
     :constant
     :string
     :keyword
     :fn-expr)
    ,(cps-triv expr)
    :invoke 
    ,(cps-app expr empty-continuation)
    (:def
     :if
     :do
     :let
     :recur
     :throw
     :try
     :monitor-enter
     :monitor-exit
     :instance-method
     :new
     :static-method)
    ,(if (trivial-expr? expr)
         (cps-triv expr)
         (cps-srs expr (make-fresh-var 'k)))
    (error (str "unexpected expression " expr))))

(defmacro cps
  [expr]
  (-> expr
      analyze/macroexpand
      analyze/analyze-form
      cps-expr
      emit/emit-form))
