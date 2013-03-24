(defproject tools.cps "0.1.0-SNAPSHOT"
  :description "A library for applying the CPS transformation to Clojure
  code. Includes support for control operators such as
  call-with-current-continuation and delimited continuations."
  :url "https://github.com/cjfrisz/tools.cps"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/jvm.tools.analyzer "0.3.0"]])
