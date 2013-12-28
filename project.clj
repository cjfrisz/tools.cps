(defproject tools.cps "0.1.0-SNAPSHOT"
  :description "CPS utilities for Clojure"

  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/jvm.tools.analyzer "0.5.2"]]

  :repositories {"sonatype-oss-public" "https://oss.sonatype.org/content/groups/public/"}
  
  :source-paths ["src/main/clojure"]

  :repl-options {:init-ns clojure.tools.cps})
