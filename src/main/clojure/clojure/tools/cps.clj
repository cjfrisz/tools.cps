(ns clojure.tools.cps
  (:require [clojure.tools.analyzer :as analyze]
            [clojure.tools.analyzer.emit-form :as emit]))

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

(def $k-tag
  "Unique tag used to distinguish continuations from other arguments."
  :kont_es5y2pkhzlx4x7816y9ezl-0)

;; TODO: audit AST-as-hash-map-generating functions for code duplictation
;; NB: should AST-as-hash-map-generating functions be simple recursive
;; NB: calls to analyze-form? probably easier but more computationally
;; NB: expensive.

(defn make-binding
  "Generates an analyzer expression for a local binding variable."
  [sym]
  {:op :local-binding,
   ;; NB: consider passing in programs ns info
   ;; NB: this suggests a *ton* of work for error-reporting consistency
   :env {:locals {} :ns {:name 'clojure.tools.cps}},
   :sym sym,
   :tag nil,
   :init nil})

(defn make-binding-ref
  "Generates an analyzer expression for a reference to a local binding
  variable."
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
  "Generates an anlyzer expression for a fn-method, aka a single
  overload for a fn expression."
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
  "Generates an analyzer expression for an if expression with the
  given test, consequence, and alternative expressions."
  [test conseq alt]
  {:op :if,
   :env {:locals {}, :ns {:name 'clojure.tools.cps}},
   :test test,
   :then conseq,
   :else alt})

(defn make-let
  "Generates an analyzer expression for a let expression that binds
  each variable expression in bind* to its corresponding init
  expression in init* with body given by the (singular) body
  expression."
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
  "Generates an analyzer expression for a do expression with the given
  list of expression."
  [expr*]
  {:op :do,
   :env {:locals {}, :ns {:name 'clojure.tools.cps}},
   :exprs expr*})

(defn make-keyword
  "Generates an analyzer expression for a keyword."
  [kw]
  {:op :keyword,
   :env {:locals {}, :ns {:name 'clojure.tools.cps}},
   :val kw})

(defn make-var
  "Generates an analyzer expression for a given var. This is used to
  refer to built-in Clojure functions, i.e. clojure.core/meta and
  clojure.core/with-meta."
  [var]
  {:op :var,
   :env {:locals {}, :ns {:name 'clojure.tools.cps}},
   :var var,
   :tag nil})

(def $k-tag-kw
  "The metadata tag for continuations as an analyzer-style keyword."
  (make-keyword $k-tag))

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

(defn make-k-test
  "Generates the analyzer expression for whether k-var is a
  continuation, i.e. ($k-tag-kw (meta k-var)). This is used for
  generating fn-methods that require both a CPS version and a
  compatibility call."
  [k-var]
  (let [meta-var (make-var (var clojure.core/meta))
        k-var-meta (make-app meta-var (list k-var))]
    (make-app $k-tag-kw (list k-var-meta))))

(declare cps-triv cps-srs)

(defn make-arity->k+body
  "Takes a list of argument counts for the CPS versions of each method
  and the original methods and returns a hash-map of the arities for
  the CPS methods associated with the body list of body expressions."
  [cps-arity* method*]
  (let [k+body* (for [method method*
                      :let [k (make-fresh-var 'k)
                            body (for [expr (-> method :body :exprs)]
                                   (if (trivial-expr? expr)
                                       (make-app (make-binding-ref k)
                                         (list expr))
                                       (cps-srs expr k)))]]
                  [k body])]
    (zipmap cps-arity* k+body*)))
  

(defn build-method
  "Takes an arity, a hash-map of arities associated with parameter
  lists, an fn name, a continuation parameter name (possibly nil), and
  a sequence of CPS expression (possibly nil in the same instances as
  k-param), and returns an appropriate analyzer fn-method, aka a
  single overload of an fn expression.

  Note that cps-body should be a do expression once those are
  supported by cps-expr."
  [arity arity->param* fn-name k-param cps-body]
  (let [orig-param* (arity->param* (dec arity))
        compat-param* (arity->param* arity)
        cps? (and k-param cps-body orig-param*)
        compat? (not (nil? compat-param*))
        param* (if cps? (conj orig-param* k-param) compat-param*)
        make-compat-call (fn [fn-name param*]
                           (make-app (-> fn-name
                                         make-binding
                                         make-binding-ref)
                             (conj (map make-binding-ref param*)
                               empty-k)))]
    (cond
      (and cps? compat?) (make-fn-method param*
                           (list (make-if (make-k-test k-param)
                                   (make-do cps-body)
                                   (make-compat-call fn-name param*))))
      cps? (make-fn-method param* cps-body)
      compat? (make-fn-method param*
                (list (make-compat-call fn-name param*)))
      :else (error "unexpected arg count " (count param*)
                   " while building fn-method."))))
                  
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
        param** (map :required-params method*)
        orig-arity* (map count param**)
        arity->param* (zipmap orig-arity* param**)
        cps-arity* (map inc orig-arity*)
        arity->k+body (make-arity->k+body cps-arity* method*)]
    (-> expr
        (assoc :name fn-name)
        (assoc :methods
          ;; NB: output is slightly nicer to read if this is sorted
          (for [arity (distinct (concat orig-arity* cps-arity*))
                :let [[k-param cps-body] (arity->k+body arity)]]
            (build-method arity arity->param*
              fn-name
              k-param
              cps-body))))))

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
      (if (not-any? (comp nil? seq) [rcall* rtriv*])
          out
          (let [[rcall & rcall*] rcall*
                [rtriv & rtriv*] rtriv*]
            (if (trivial-expr? rcall)
                (recur rcall* rtriv* out)
                (recur rcall* rtriv*
                  (cps-srs rcall (make-continuation rtriv out)))))))))
                
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
  "Applies a CPS transformation to an arbitrary Clojure expression."
  (-> expr
      analyze/macroexpand
      analyze/analyze-form
      cps-expr
      emit/emit-form))
