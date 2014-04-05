(ns clojure.tools.cps
  (:require [clojure.jvm.tools.analyzer :as analyze]
            [clojure.jvm.tools.analyzer.emit-form :as emit]))

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
    (:nil
     :number
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
    :var #'clojure.core/with-meta,
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
  overload for a fn expression. Optionally takes a rest argument
  parameter name as its second argument."
  ([param* expr*] (make-fn-method param* nil expr*))
  ([param* rest-param expr*]
   {:op :fn-method,
    :env {:locals {}, :ns {:name 'clojure.tools.cps}},
    :body
    {:op :do,
     :env {:locals {}, :ns {:name 'clojure.tools.cps}},
     :exprs expr*},
    :required-params param*,
    :rest-param rest-param}))

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

(defn make-keyword-invoke
  [kw target]
  {:op :keyword-invoke,
   :env {:locals {}, :ns {:name 'clojure.tools.cps}},,
   :kw kw,
   :tag nil,
   :target target})

(defn make-recur
  [bind* arg*]
  {:op :recur,
   :env {:locals {},:ns {:name 'clojure.tools.cps}},
   :loop-locals bind*,
   :args arg*})
  
(defn make-k-test
  "Generates the analyzer expression for whether k-var is a
  continuation, i.e. ($k-tag-kw (meta k-var)). This is used for
  generating fn-methods that require both a CPS version and a
  compatibility call."
  [k-var]
  (let [meta-var (make-var  #'clojure.core/meta)
        k-var-meta (make-app meta-var (list (make-binding-ref k-var)))]
    (make-keyword-invoke $k-tag-kw k-var-meta)))

(declare cps-triv cps-srs)

(defn cps-method-body
  "Takes an analyzer fn-method and returns a CPS version of it. This
  will likely be deprecated in favor of applying cps-expr to the
  method's do when that is supported."
  [method k]
  (for [expr (-> method :body :exprs)]
    (if (trivial-expr? expr)
        (make-app (make-binding-ref k)
          (list expr))
        (cps-srs expr k))))

(defn cps-varg-method-body
  [varg-method k]
  (let [cps-body (cps-method-body varg-method k)
        orig-param* (conj (:required-params varg-method) k)
        last-param (or (last orig-param*) k)
        rest-param (:rest-param varg-method)
        last-arg+rest (make-app (make-var #'clojure.core/conj)
                        (map make-binding-ref
                          (list rest-param last-param)))
        recur-with-k (make-recur
                       (concat orig-param* (list rest-param))
                       (concat (list empty-k)
                         (map make-binding-ref (butlast orig-param*))
                         (list last-arg+rest)))]
    (list (make-if (make-k-test k) (make-do cps-body) recur-with-k))))

(defn make-arity->k+body
  "Takes a list of argument counts for the CPS versions of each method
  and the original methods and returns a hash-map of the arities for
  the CPS methods associated with the body list of body expressions."
  [cps-arity* method*]
  (let [k+body* (for [method method*
                      :let [k (make-fresh-var 'k)
                            body (if (:rest-param method)
                                     (cps-varg-method-body method k)
                                     (cps-method-body method k))]]
                  [k body])]
    (zipmap cps-arity* k+body*)))

(defn build-arity*
  [farg-method* varg-method]
  (let [farg-arity* (map (comp count :required-params) farg-method*)]
    (if (nil? varg-method)
        farg-arity*
        (let [varg-min-arity (count (:required-params varg-method))]
          (concat farg-arity*
                  (list (if (contains? (set farg-arity*) varg-min-arity)
                            (inc varg-min-arity)
                            varg-min-arity)))))))

(defn do-build-method*
  "Takes a sequence of argument counts, a hash-map of argument counts
  associated with parameter lists, another hash-map of argument counts
  associated with vectors of a continuation parameter variable and a
  sequence of CPS expressions representing a function method body, and
  a fn name and returns a sequence of analyzer fn-methods."
  [fn-ref arity* arity->param* arity->k+body]
  (for [arity arity*
        :let [orig-param* (arity->param* (dec arity))
              compat-param* (arity->param* arity)
              [k-param cps-body] (arity->k+body arity)
              cps? (and k-param cps-body orig-param*)
              compat? (not (nil? compat-param*))
              param* (or (and cps? (conj orig-param* k-param))
                         compat-param*)
              ;; not used if nil
              compat-call (and compat?
                               (make-app fn-ref
                                 (conj (map make-binding-ref param*)
                                   empty-k)))]]
    (cond
      ;; variadic overloads will never have an arity clash
      (and cps? compat?) (make-fn-method param*
                           (list (make-if (make-k-test k-param)
                                   (make-do cps-body)
                                   compat-call)))
      cps? (make-fn-method param* cps-body)
      compat? (make-fn-method param* (list compat-call))
      :else (error "unexpected arg count " (count param*)
              " while building fn-method."))))

(defn build-method*
  [src-method* fn-name]
  (let [fn-ref (-> fn-name make-binding make-binding-ref)
        src-farg-method* (remove :rest-param src-method*)
        src-varg-method (first (filter :rest-param src-method*))
        farg-param** (map :required-params src-farg-method*)
        varg-param* (:required-params src-varg-method)
        orig-arity* (build-arity* src-farg-method* src-varg-method)
        _ (prn orig-arity*)
        arity->param* (zipmap orig-arity*
                        (concat farg-param** (list varg-param*)))
        cps-arity* (map inc orig-arity*)
        arity->k+body (make-arity->k+body cps-arity* src-method*)
        ;; NB: output is slightly nicer to read if arity* is sorted
        arity* (distinct (concat orig-arity* cps-arity*))]
    ;; much like the highlander, there can be only one variadic method
    (assert (some #{(count (filter :rest-param src-method*))} [0 1]))
    (do-build-method* fn-ref arity* arity->param* arity->k+body)))
                  
;; by the time a fn expression is handed to cps-fn, data-structure
;; arguments (i.e. argument destructuring) has been expanded so that the
;; argument is a simple variable and the destructuring is done in the
;; fn body as a let binding.
;; NB: this implementation doesn't solve the arity clash problem
(defn cps-fn
  "Applies the First Order One Pass CPS algorithm to fn expressions, and
  returns the analyzer representation for the result."
  [expr]
  (let [fn-name (or (:name expr) (gensym 'fn))
        method* (build-method* (:methods expr) fn-name)]
    (-> expr
        (assoc :name fn-name)
        (assoc :methods method*)
        (assoc :variadic-method
          (some #(and (:rest-param %) %) method*)))))

(defn cps-fn
  [expr]
  (let [method-info* (map (fn [method]
                            (let [required-params (:required-params method)
                                  arity (count required-params)]
                            {:original-method method
                             :original-body (:body method)
                             :required-params required-params
                             :rest-param (:rest-param method)
                             :original-arity arity
                             :cps-arity (inc arity)})
                       (:methods expr))
        with-k-param* (map (fn [method-info]
                             (assoc method-info
                               :k-param (make-fresh-var 'k)))
                        method-info*)
        with-cps-body* (map (fn [method-info]
                              (assoc method-info
                                :cps-body (cps-method-body
                                            (:original-body method-inf0)
                                            (:k-param method-info))))
                         with-k-param*)
        with-compat-body* (map (fn [method-info]
                                 (if (some (comp (partial = (:cps-arity method-info))
                                                 :original-arity)
                                       with-cps-body*)
                                     (comment build compat body))))
        (comment adjust minimum arity for variadic method)
        (comment take care of variadic function building))]
    (comment merge required params and updated bodies with original methods)))

(defn cps-app
  "applies the first Order One Pass CPS algorithm to function applications,
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
    (:nil
     :var
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
  (binding [analyze/*eval-ast* false]
    (-> expr
      analyze/macroexpand
      analyze/analyze-form
      cps-expr
      emit/emit-form)))
