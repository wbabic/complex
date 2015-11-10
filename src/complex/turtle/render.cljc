(ns complex.turtle.render
  (:require
   [complex.number :as n
    :refer [infinity zero one i negative-one negative-i coords]]
   [complex.turtle :as turtle]
   [complex.geometry :as g]
   #?(:clj
      [clojure.core.match :refer [match]]
      :cljs
      [cljs.core.match :refer-macros [match]])))

(defn stroke-style [color]
  [:style {:stroke color}])

(defn fill-style [color]
  [:style {:fill color}])

(defn point-style [color]
  [:style {:stroke :grey :fill color}])

(defn render-point
  "return render point data if point is not infinity,
  or nil if it is"
  [point-keyword turtle]
  (let [point-value (get-in turtle [:points point-keyword])
        point-style-map (get-in turtle [:style point-keyword])]
    (when-not (= point-value infinity)
      [(point-style (:inside point-style-map))
       [:point (coords point-value)]])))

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

(defn to-line-segment
  "turn collinear generalized circle (with a possible point at infinity) into a line segment
  with finite endpoints"
  [l]
  (let [[z1 z2 z3] l]
    (if (some #(= infinity %) l)
      (cond
        (= z1 infinity) [z2 z3]
        (= z2 infinity) [z1 z3]
        :else [z1 z2])
      [z1 z2])))

(defn extend-segment
  "extend line segment"
  [segment]
  (let [[z1 z2] segment]
    [(plus-infinity z2 z1) (plus-infinity z1 z2)]))

(defn inside-rect
  "return inside extended rectangle for given extended line segment"
  [extended-line-segment]
  (let [[ez1 ez2] extended-line-segment
        lower-left ez1
        upper-right (n/add ez2 (n/mult i (n/sub ez2 ez1)))]
    [lower-left upper-right]))

(comment
  (map coords
       (let [l [zero one infinity]]
         (-> l to-line-segment extend-segment inside-rect)))
  ;;=> ([-99999 0] [100000 199999])
  )

(defn render-inside
  "render inside of line l consisting of three collinear points
  any of which may be infinity"
  [l line-style]
  (let [rect (-> l
                 to-line-segment
                 extend-segment
                 inside-rect)
        rect (map coords rect)]
    [(fill-style (:inside line-style))
     [:rect rect]]))

(comment
  (render-inside [zero one infinity] {:edge :red :inside :lt-red})
  ;;=>
  [[:style {:fill :lt-red}]
   [:rect ([-99999 0] [100000 199999])]]
  )

(defn render-edge
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

(defn render-line
  "render line l consisting of three collinear points
  any of which may be infinity"
  [l line-style]
  (concat
   (render-edge l line-style)
   (render-inside l line-style)))

(comment
  (render-line [zero one infinity] {:edge :red :inside :lt-red})
  ;;=>
  ([:style {:stroke :red}]
   [:line [0 0] [1 0]]
   [:line [1 0] [100000 0]]
   [:line [-99999 0] [0 0]]
   [:style {:fill :lt-red}]
   [:rect ([-99999 0] [100000 199999])])
  )

(defn render-circle
  "assumes g-circle is not a line"
  [g-circle circle-style]
  [(stroke-style (:edge circle-style))
   (g/circumcircle g-circle)])

(defn render-circle-or-line
  [circle-keyword turtle]
  (let [circle-points (turtle/points-for-circle
                       circle-keyword
                       (:points turtle))
        circle-style (get-in turtle [:style circle-keyword])]
    (if (g/collinear? circle-points)
      (render-line circle-points circle-style)
      (render-circle circle-points circle-style))))

(defn render-turtle
  "render circles first, then points,
of the given turtle"
  [turtle]
  (concat
   ;; render lines
   (render-circle-or-line :x-axis turtle)
   (render-circle-or-line :y-axis turtle)
   (render-circle-or-line :unit-circle turtle)

   ;; render points
   (render-point :one turtle)
   (render-point :i turtle)
   (render-point :zero turtle)
   (render-point :infinity turtle)
   (render-point :negative-one turtle)
   (render-point :negative-i turtle)))

(comment
  (require '[complex.turtle.render] :reload)
  (in-ns 'complex.turtle.render)
  (use 'clojure.repl)
  (require '[complex.turtle :as turtle] :reload)

  (render-circle-or-line :unit-circle turtle/standard-turtle)
  ;;=> [[:style {:stroke :orange}] [:circle {:center [0N 0N], :radius 1.0}]]

  (render-circle-or-line :x-axis turtle/standard-turtle)
  ;;=>
  [[:style {:stroke :green}]
   [:line [0 0] [1 0]]
   [:line [1 0] [100000 0]]
   [:line [-99999 0] [0 0]]]

  (render-circle-or-line :y-axis turtle/standard-turtle)
  ;;=>
  ([:style {:stroke :green}]
   [:line [0 0] [1 0]]
   [:line [1 0] [100000 0]]
   [:line [-99999 0] [0 0]]
   [:style {:fill :lt-green}]
   [:rect ([-99999 0] [100000 199999])])

  (render-point :zero turtle/standard-turtle)
  ;;=> [[:style {:stroke :yellow, :fill :grey}] [:point [0 0]]]

  (render-turtle turtle/standard-turtle)
  ;;=>
  ([:style {:stroke :green}]
   [:line [0 0] [1 0]]
   [:line [1 0] [100000 0]]
   [:line [-99999 0] [0 0]]
   [:style {:fill :lt-green}]
   [:rect ([-99999 0] [100000 199999])]
   [:style {:stroke :purple}]
   [:line [0 0] [0 1]]
   [:line [0 1] [0 100000]]
   [:line [0 -99999] [0 0]]
   [:style {:fill :lt-purple}]
   [:rect ([0 -99999] [-199999 100000])]
   [:style {:stroke :orange}]
   [:circle {:center [0N 0N], :radius 1.0}]
   [:style {:stroke :grey, :fill :cyan}]
   [:point [1 0]]
   [:style {:stroke :grey, :fill :red}]
   [:point [0 1]]
   [:style {:stroke :grey, :fill :yellow}]
   [:point [0 0]]
   [:style {:stroke :grey, :fill :blue}]
   [:point [-1 0]]
   [:style {:stroke :grey, :fill :magenta}]
   [:point [0 -1]])
  )
