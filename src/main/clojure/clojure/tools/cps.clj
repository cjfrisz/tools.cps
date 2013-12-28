(ns clojure.tools.cps
  (:require [clojure.tools.analyzer :as analyze]
            [clojure.tools.analyzer.emit-form :as emit]))

;; NB: probably deficient in the cases covered
(defn trivial-expr?
  "Takes an expression (as output by analyze-form) and returns a boolean
  representing whether the expression is trivial from the point of view of
  the First Order One Pass CPS algorithm."
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

;; TODO: audit AST-as-hash-map-generating functions for code duplictation
;; NB: should AST-as-hash-map-generating functions be simple recursive
;; NB: calls to analyze-form?

(defn make-binding
  [sym]
  {:op :local-binding,
   ;; NB: consider passing in programs ns info
   ;; NB: this suggests a *ton* of work for error-reporting consistency
   :env {:locals {} :ns {:name 'clojure.tools.cps}},
   :sym sym,
   :tag nil,
   :init nil})

(defn make-binding-ref
  [binding]
  {:op :local-binding-expr,
   :env {:locals {}, :ns {:name 'clojure.tools.cps}},
   :local-binding binding,
   :tag nil})

(defn make-app
  "Generates an analyzer-style function invocation expression with rator as
  the fexpr (function expression/operator) and rand* as the args (arguments
  list/operands)." 
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

(defn make-continuation
  "Generates an analyzer-style expression representing a continuation (as a
  fn expression."
  ;; NB: missing lots of source information here
  [arg body]
  {:args
   (list {:op :fn-expr,
          :env {:line -1, :locals {}, :ns {:name 'clojure.tools.cps}},
          :methods
          (list {:op :fn-method,
                 :env {:locals {}, :ns {:name 'clojure.tools.cps}},
                 :body
                 {:op :do,
                  :env {:locals {}, :ns {:name 'clojure.tools.cps}},
                  :exprs (list body)},
                 :required-params (list arg),
                 :rest-param nil}),
          :variadic-method nil,
          :tag nil}
         {:op :constant,
          :env {:locals {}, :ns {:name 'clojure.tools.cps}},
          :val {$k-tag true}}),
   :op :invoke,
   :protocol-on nil,
   :is-protocol false,
   :fexpr
   {:op :var,
    :env {:locals {}, :ns {:name 'clojure.tools.cps}},
    ;; NB: not sure if this is the right thing to do in the long run
    :var (var with-meta),
    :tag nil},
   :is-direct false,
   :env
   {:source "unknown",
    :locals {},
    :ns {:name 'clojure.tools.cps}},
   :site-index -1,
   :tag nil})

(defn make-fn-method
  [param* expr*]
  {:op :fn-method,
   :env {:locals {}, :ns {:name 'clojure.tools.cps}},
   :body
   {:op :do,
    :env {:locals {}, :ns {:name 'clojure.tools.cps}},
    :exprs expr*},
   :required-params param*,
   :rest-param nil})

(defn make-if
  [test conseq alt]
  {:op :if,
   :env {:locals {}, :ns {:name 'clojure.tools.cps}},
   :test test,
   :then conseq,
   :else alt})

(defn make-let
  [bind* init* body]
  (letfn [(make-bind [bind init]
            {:op :binding-init,
             :env {:locals {}, :ns {:name 'clojure.tools.cps}},
             :local-binding (assoc bind :init init),
             :init init})]
    {:op :let,
     :env {:locals {}, :ns {:name 'clojure.tools.cps}},
     :binding-inits (map make-bind bind* init*),
     :body body,
     :is-loop false}))

(defn make-do
  [expr*]
  {:op :do,
   :env {:locals {}, :ns {:name 'clojure.tools.cps}},
   :exprs expr*})

(defn make-keyword
  [key]
  {:op :keyword,
   :env {:locals {}, :ns {:name 'clojure.tools.cps}},
   :val key})

(defn make-var
  [var]
  {:op :var,
   :env {:locals {}, :ns {:name 'clojure.tools.cps}},
   :var var,
   :tag nil})

(defn make-fresh-var
  "Generates an analyzer-style variable expression with a gensym for the
  variable's symbol value. Optionally takes a symbol as the base name for
  the gensym."
  ([] (make-fresh-var 'g))
  ([name] (make-binding (gensym name))))

(def sym->var-ref
  "Convenience function that takes a symbol representing a variable
  expression and applies both make-binding and make-binding-ref to it to
  create a proper variable reference."
  (comp make-binding-ref make-binding))

(def empty-k
  "The empty continuation, represented as the identity function."
  (let [x (make-fresh-var 'x)]
    (make-continuation x (make-binding-ref x))))

(def $k-tag
  "Unique tag used to distinguish continuations from other arguments."
  :kont_es5y2pkhzlx4x7816y9ezl-0)

(def $k-tag-kw (make-keyword $k-tag))

(defn make-k-test
  [maybe-k]
  (let [meta-var (make-var (var clojure.core/meta))
        maybe-k-meta (make-app meta-var (list maybe-k))]
    (make-app $k-tag-kw (list maybe-k-meta))))

(declare cps-triv cps-srs)

;; NB: cps-body is a stand-in for applying CPS to a "do" expression
(defn cps-body
  [bexpr* k]
  (for [bexpr bexpr*]
    (if (trivial-expr? bexpr)
      (make-app k (list bexpr))
      (cps-srs bexpr k))))

(defn cps-method
  [method]
  (let [k (make-fresh-var 'k)]
    (make-fn-method (conj (:required-params method) k)
                    ;; NB: simply CPS as a do expression once that's
                    ;; NB: supported
                    (cps-body (-> method :body :exprs) k))))

(defn build-compat-method
  [fn-name method]
  (let [param* (:required-params method)
        arg* (map make-binding-ref param*)
        k+arg* (conj arg* empty-k)]
    (make-fn-method param*
                    (list (make-app (sym->var-ref fn-name) k+arg*)))))

;; NB: fix this so it doesn't hurt my eyes
(defn merge-methods
  [cps-method* compat-method*]
  (let [get-arg-count (comp count :required-params)
        cps-arg-count* (map get-arg-count cps-method*)
        compat-arg-count* (map get-arg-count compat-method*)
        cps-arg-cnt=>method (zipmap cps-arg-count* cps-method*)
        compat-arg-cnt=>method (zipmap compat-arg-count* compat-method*)
        get-first-binding (comp first :required-params)]
    (for [arg-cnt (set (concat cps-arg-count* compat-arg-count*))
          :let [cps-method (get cps-arg-cnt=>method arg-cnt)
                compat-method (get compat-arg-cnt=>method arg-cnt)]]
      (if (and cps-method compat-method)
          (let [maybe-k-var (get-first-binding cps-method)
                cps-param* (:required-params cps-method)
                ;; have to reverse the bindings since inits can refer to
                ;; previous bindings (i.e. let* semantics bite us here)
                compat-body (make-let
                              (reverse (:required-params compat-method))
                              (reverse (map make-binding-ref cps-param*))
                              (:body compat-method))
                k-test (make-if (make-k-test maybe-k-var)
                         (:body cps-method)
                         compat-body)]
            (make-fn-method cps-param* (list k-test)))
          (if-let [method (or cps-method compat-method)]
            method
            (error "expected method but got nil"))))))

;; by the time an fn expression is handed to cps-fn, data-structure
;; arguments (i.e. argument destructuring) has been expanded so that the
;; argument is a simple variable and the destructuring is done in the
;; fn body as a let binding.
;; NB: this implementation doesn't solve the arity clash problem
(defn cps-fn
  "Applies the First Order One Pass CPS algorithm to fn expressions, and
  returns the analyzer representation for the result."
  [expr]
  (let [fn-name (or (:name expr) (gensym 'fn))
        method* (:methods expr)
        compat-method* (map (partial build-compat-method fn-name)
                            method*)
        cps-method* (map cps-method method*)]
    (-> expr
        (assoc :name fn-name)
        (assoc :methods (merge-methods cps-method* compat-method*)))))

(defn cps-app
  "Applies the First Order One Pass CPS algorithm to function applications,
  and the returns the analyzer representation for the result."
  [expr k]
  (letfn [(trivit [e]
            (if (trivial-expr? e)
                (cps-triv e)
                (make-fresh-var 's)))]
    ;; NB: this is a den of inefficient things
    (loop [rcall* (reverse (conj (:args expr) (:fexpr expr)))
           rtriv* (map trivit rcall*)
           out (make-app (last rtriv*)
                 (conj (reverse (butlast rtriv*)) k))]
      (if (and (nil? (seq rcall*)) (nil? (seq rtriv*)))
          out
          (let [[rcall & rcall*] rcall*
                [rtriv & rtriv*] rtriv*]
            (if (trivial-expr? rcall)
                (recur rcall* rtriv* out)
                (recur rcall* rtriv*
                  (cps-srs rcall (make-continuation rtriv out)))))))))
                
(defn error
  "Throws a generic exception with its arguments concatenated into a string
  as the message."
  [& msg]
  (throw (Exception. (apply str msg))))

(defn unsupported!
  "Convenience function for signalling that a feature isn't supported
  yet. It gets an awful lot of use currently, and will hopefully be
  deprecated in the future."
  [expr-str]
  (error (str expr-str " expressions not supported yet")))

(defn cps-triv
  "Applies the First Order One Pass CPS algorithm to a trivial,
  analyzer-style expression and returns the result as an analyzer-style
  expression.

  If expr is not a trivial expression, cps-triv throws an uncaught
  exception. Note that if this ever occurs, it represents a bug in the
  implementation."
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
  "Applies the First Order One Pass CPS algorithm to a serious,
  analyzer-style expression and returns the result as an analyzer-style
  expression.

  If expr is not a serious expression, cps-srs throws an uncaught
  exception. Note that if this ever occurs, it represents a bug in the
  implementation."
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
  "Takes an analyzer-style Clojure expression and applies the First Order
  One Pass CPS algorithm to it, returning the result as an analyzer-style
  expression."
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
    ,(cps-app expr empty-k)
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

(defmacro cps [expr]
  "Takes a Clojure expression, applies the First Order One Pass CPS
  algorithm to it, and emits the result."
  (-> expr
      analyze/macroexpand
      analyze/analyze-form
      cps-expr
      emit/emit-form))
