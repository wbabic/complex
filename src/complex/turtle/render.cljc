(ns complex.turtle.render
  (:require
   [complex.number :as n
    :refer [mult div add sub minus recip infinity zero one i coords arg length conjugate]]
   [complex.geometry :as g]
   #?(:clj
      [clojure.core.match :refer [match]]
      :cljs
      [cljs.core.match :refer-macros [match]])))

(defn p-style [k color-scheme]
  [:style {:stroke "grey" :fill (k color-scheme)}])

(defn l-style [k color-scheme]
  [:style {:stroke (color-scheme k)}])

(defn stroke-style [color]
  [:style {:stroke color}])

(defn plus-infinity
  "return largest point on line within user space (r = 4)"
  [z1 z2]
  (let [l (g/param-line z1 z2)
        l-max (l 100000)
        len (length l-max)
        k (/ 6 len)]
    ;;(mult l-max k)
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
  [])

(defn render
  "transform generalized circle to
  a sequence of graphics primitives to be rendered"
  [circle-or-line color-scheme]
  (if (g/collinear? circle-or-line)
    (render-line circle-or-line color-scheme)
    (render-circle circle-or-line color-scheme)))

(defn render-turtle
  ""
  [turtle]
  )

(comment
  (require '[complex.turtle.render] :reload)
  (in-ns 'complex.turtle.render)
  (use 'clojure.repl)

  (require '[complex.turtle :as turtle])
  (def st turtle/standard-turtle)
  (render-circle (-> st :circles :unit-circle)
                 (-> st :style :unit-circle))
  )

(comment
  (require '[complex.turtle.render] :reload)
  )
