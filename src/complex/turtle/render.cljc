(ns complex.turtle.render
  (:require
   [complex.number :as n
    :refer [mult div add sub minus recip infinity zero one i coords arg length conjugate]]
   [complex.geometry :as g]
   #?(:clj
      [clojure.core.match :refer [match]]
      :cljs
      [cljs.core.match :refer-macros [match]])))

(defn stroke-style [color]
  [:style {:stroke color}])

(defn point-style [color]
  [:style {:stroke color :fill :grey}])

(defn plus-infinity
  "return large point on line from z1 to z2
  where z1 and z2 are complex numbers"
  [z1 z2]
  (let [l (g/param-line z1 z2)
        l-max (l 100000)
        len (length l-max)]
    l-max))

(defn line
  "line between first two complex numbers
  of given line where either one may be infinity"
  [z1 z2 z3]
  (let [[w1 w2]
        (cond
          (= infinity z1) [(plus-infinity z3 z2) z2]
          (= infinity z2) [z1 (plus-infinity z3 z1)]
          :else [z1 z2])]
    [:line (coords w1) (coords w2)]))

(defn render-circle
  "assumes g-circle is not a line"
  [g-circle circle-style]
  [(stroke-style (:edge circle-style))
   (g/circumcircle g-circle)])

(defn render-line
  "render line l consisting of three collinear points
  any of which may be infinity"
  [l line-style]
  (let [[z1 z2 z3] l
        infinity? (some #(= infinity %) l)]
    (if infinity?
      [(stroke-style (:edge line-style))
       (line z1 z2 z3)
       (line z2 z3 z1)
       (line z3 z1 z2)]
      [(stroke-style (:edge line-style))
       (line z1 z2 infinity)
       (line infinity z1 z2)
       (line z2 infinity z1)])))

(defn render-point
  "render point with given color inside
  defaults to grey edge"
  [point color]
  [(point-style (:inside color))
   [:point (coords point)]])

(defn render
  "transform generalized circle to
  a sequence of graphics primitives to be rendered"
  [circle-or-line color-scheme]
  (if (g/collinear? circle-or-line)
    (render-line circle-or-line color-scheme)
    (render-circle circle-or-line color-scheme)))

(defn render-turtle
  "render circles first, then points,
of the given turtle"
  [turtle]
  (concat
   ;; render lines
   (render-line   (-> turtle :circles :x-axis) (-> turtle :style :x-axis))
   (render-line   (-> turtle :circles :y-axis) (-> turtle :style :yh-axis))
   (render-circle (-> turtle :circles :unit-circle)
                  (-> turtle :style :unit-circle))
   ;; render points
   (render-point  (-> turtle :points :one)
                  (-> turtle :style :one))
   (render-point  (-> turtle :points :i)
                  (-> turtle :style :i))
   (render-point  (-> turtle :points :zero)
                  (-> turtle :style :zero))
   (render-point  (-> turtle :points :infinity)
                  (-> turtle :style :infinity))
   (render-point  (-> turtle :points :negative-one)
                  (-> turtle :style :negative-one))
   (render-point  (-> turtle :points :negative-i)
                  (-> turtle :style :negative-i))))

(comment
  (require '[complex.turtle.render] :reload)
  (in-ns 'complex.turtle.render)
  (use 'clojure.repl)

  (require '[complex.turtle :as turtle])
  (let [st turtle/standard-turtle]
    (render-circle (-> st :circles :unit-circle)
                   (-> st :style :unit-circle)))
  ;;=> [[:style {:stroke :orange}] [:circle {:center [0N 0N], :radius 1.0}]]

  (let [st turtle/standard-turtle]
    (render-line (-> st :circles :x-axis)
                 (-> st :style :x-axis)))
  ;;=>
  [[:style {:stroke nil}]
   [:line [0 0] [1 0]]
   [:line [1 0] [100000 0]]
   [:line [-99999 0] [0 0]]]

  (let [st turtle/standard-turtle]
    (render-line (-> st :circles :y-axis)
                 (-> st :style :y-axis)))
  ;;=>
  [[:style {:stroke nil}]
   [:line [0 0] [0 1]]
   [:line [0 1] [0 100000]]
   [:line [0 -99999] [0 0]]]

  (let [st turtle/standard-turtle]
    (render-turtle st))
  ;;=>
  ([:style {:stroke :green}]
   [:line [0 0] [1 0]] [:line [1 0] [100000 0]] [:line [-99999 0] [0 0]]
   [:style {:stroke nil}]
   [:line [0 0] [0 1]] [:line [0 1] [0 100000]] [:line [0 -99999] [0 0]]
   [:style {:stroke :orange}]
   [:circle {:center [0N 0N], :radius 1.0}]
   [:style {:stroke :cyan, :fill :grey}]
   [:point [1 0]]
   [:style {:stroke :red, :fill :grey}]
   [:point [0 1]]
   [:style {:stroke :yellow, :fill :grey}]
   [:point [0 0]]
   [:style {:stroke :black, :fill :grey}]
   [:point :infinity]
   [:style {:stroke :blue, :fill :grey}]
   [:point [-1 0]]
   [:style {:stroke :magenta, :fill :grey}]
   [:point [0 -1]])
  )
