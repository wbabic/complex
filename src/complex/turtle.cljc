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

;; 3 genearlized circles
(def x-axis      [zero one infinity])
(def y-axis      [zero i infinity])
(def unit-circle [one i (minus one)])
(def circles     [x-axis y-axis unit-circle])
(def circle-keys [:x-axis :y-axis :unit-circle])

;; 6 points
(def points      [zero infinity one (minus one) i (minus i)])
(def point-keys  [:zero :infinity :one :minus-one :i :minus-1])

(def circle-map
  {:x-axis x-axis
   :y-axis y-axis
   :unit-circle unit-circle})

(def point-map
  {:zero zero
   :infinity infinity
   :one one
   :minus-one (minus one)
   :i i
   :minus-i (minus i)})

(def style-map
  {:x-axis      {:stroke :green  :fill :lt-green}
   :y-axis      {:stroke :purple :fill :lt-purple}
   :unit-circle {:stroke :orange :fill :lt-orange}
   :zero        {:stroke :grey   :fill :yellow}
   :infinity    {:stroke :grey   :fill :black}
   :one         {:stroke :grey   :fill :cyan}
   :minus-one   {:stroke :grey   :fill :blue}
   :i           {:stroke :grey   :fill :red}
   :minus-i     {:stroke :grey   :fill :magenta}})
