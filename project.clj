(defproject complex "0.1.8"
  :description "Cross platform library for complex numbers. geometry and transforms"
  :url "http://wbabic.github.io"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :dependencies [[org.clojure/clojure "1.7.0" :scope "provided"]
                 [org.clojure/clojurescript "1.7.145" :scope "provided"]
                 [org.clojure/core.match "0.3.0-alpha4"]
                 [com.gfredericks/exact "0.1.8"]]

  :profiles
  {:dev
   {:dependencies [[org.clojure/test.check "0.8.2"]
                   [prismatic/schema "1.0.1"]]
    :plugins [[lein-cljsbuild "1.1.0"]]
    :aliases {"nodetest" ["do" "clean," "cljsbuild" "once" "node-dev"]}}}

  :cljsbuild
  {:builds
   [{:id "node-dev"
     :source-paths ["src" "test"]
     :notify-command ["node" "resources/run.js"]
     :compiler {:optimizations :none
                :static-fns true
                :target :nodejs
                :output-to "target/cljs/node_dev/tests.js"
                :output-dir "target/cljs/node_dev/out"
                :source-map true}}]
   :test-commands {"test" ["node" "resources/run.js"]}})
