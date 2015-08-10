(defproject complex "0.1.1"
  :description "Cross platform library for complex numbers. geometry and transforms"
  :url "http://wbabic.github.io"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :profiles
  {:dev {:plugins [[lein-cljsbuild "1.0.6"]
                   [lein-figwheel "0.3.7"
                    :exclusions [org.clojure/clojure
                                 org.codehaus.plexus/plexus-utils]]]
         :aliases {"cleantest" ["do" "clean," "cljsbuild" "test"]}}}

  :dependencies [[org.clojure/clojure "1.7.0"]
                 [org.clojure/clojurescript "1.7.48"]
                 [org.clojure/core.match "0.3.0-alpha4"]
                 [org.clojure/test.check "0.7.0"]
                 [prismatic/schema "0.4.3"]]

  :cljsbuild
  {:builds [{:id "test"
             :source-paths ["src" "test"]
             :compiler {:output-to "target/testable.js"
                        :optimizations :whitespace
                        :pretty-print true}}]
   :test-commands {"test" ["phantomjs" "phantom/unit-test.js" "phantom/unit-test.html"]}}
)
