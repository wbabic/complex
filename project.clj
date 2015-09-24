(defproject complex "0.1.2"
  :description "Cross platform library for complex numbers. geometry and transforms"
  :url "http://wbabic.github.io"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :profiles
  {:dev {:plugins [[lein-cljsbuild "1.1.0"]]
         :aliases {"cleantest" ["do" "clean," "cljsbuild" "test"]
                   "nodetest" ["do" "clean," "cljsbuild" "once" "node-dev"]}}}

  :dependencies [[org.clojure/clojure "1.7.0"]
                 [org.clojure/clojurescript "1.7.48"]
                 [org.clojure/core.match "0.3.0-alpha4"]
                 [org.clojure/test.check "0.7.0"]
                 [prismatic/schema "0.4.3"]
                 [com.gfredericks/exact "0.1.8"]]

  :cljsbuild
  {:builds
   [{:id "test"
     :source-paths ["src" "test"]
     :compiler {:output-to "target/testable.js"
                :optimizations :whitespace
                :pretty-print true}}
    {:id "node-dev"
     :source-paths ["src" "test"]
     :notify-command ["node" "resources/run.js"]
     :compiler {:optimizations :none
                :static-fns true
                :target :nodejs
                :output-to "target/cljs/node_dev/tests.js"
                :output-dir "target/cljs/node_dev/out"
                :source-map true}}]
   :test-commands {"test" ["phantomjs" "phantom/unit-test.js" "phantom/unit-test.html"]}})
