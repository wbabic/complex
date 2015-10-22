(ns complex.turtle
  "namspace for turtle definition and operations"
  (:require [complex.number :as n
             :refer [infinity zero one i minus]]
            [complex.geometry :as g]
            [complex.transform :as t]
            #?(:clj
               [clojure.core.match :refer [match]]
               :cljs
               [cljs.core.match :refer-macros [match]])))

(def negative-one (minus one))
(def negative-i (minus i))

;; 3 genearlized circles
(def x-axis      [zero one infinity])
(def y-axis      [zero i infinity])
(def unit-circle [one i negative-one])
(def circles     [x-axis y-axis unit-circle])
(def circle-keys [:x-axis :y-axis :unit-circle])

;; 6 points
(def points      [zero infinity one negative-one i negative-i])

(def turtle-circles
  {:x-axis x-axis
   :y-axis y-axis
   :unit-circle unit-circle})

(def turtle-points
  {:zero zero
   :infinity infinity
   :one one
   :negative-one negative-one
   :i i
   :negative-i negative-i})

(def default-turtle-style
  {:x-axis       {:edge :green  :inside :lt-green}
   :y-axis       {:edge :purple :inside :lt-purple}
   :unit-circle  {:edge :orange :inside :lt-orange}
   :zero         {:edge :grey   :inside :yellow}
   :infinity     {:edge :grey   :inside :black}
   :one          {:edge :grey   :inside :cyan}
   :negative-one {:edge :grey   :inside :blue}
   :i            {:edge :grey   :inside :red}
   :negative-i   {:edge :grey   :inside :magenta}})

(def standard-turtle
  {:circles turtle-circles
   :points turtle-points
   :style default-turtle-style})

(comment
  (require '[complex.turtle] :reload)
  (in-ns 'complex.turtle)
  (use 'clojure.repl)
  (require '[complex.turtle-test] :reload-all)
  (in-ns 'complex.turtle-test)
  (run-tests)
  )
