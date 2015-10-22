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

(defn render-line
  "render line l consisting of three collinear points
  any of which may be infinity"
  [l color-scheme]
  (let [[z1 z2 z3] l
        infinity? (some #(= infinity %) l)
        ;; if one of zi is infinity
        lines
        (if infinity?
          [(l-style :s1 color-scheme)
           (line z1 z2 z3)
           (line z2 z3 z1)
           (line z3 z1 z2)]
          [(l-style :s1 color-scheme)
           (line z1 z2 infinity)
           (line infinity z1 z2)
           (line z2 infinity z1)])
        ;; if none of z1 is infinity
        ;; then extend ...
        points (cond-> []
                 (not (= infinity z1))
                 (into [(p-style :p1 color-scheme)
                        [:point (coords z1)]])

                 (not (= infinity z2))
                 (into [(p-style :p2 color-scheme)
                        [:point (coords z2)]])

                 (not (= infinity z3))
                 (into [(p-style :p3 color-scheme)
                        [:point (coords z3)]]))]
    (concat lines points)))

(defn arc
  "arc between two complex numbers"
  [center radius start end clockwise]
  [:arc {:center center :radius radius
         :start start :end end
         :clockwise clockwise}])

(defn args
  "arguments of three complex numbers"
  [z1 z2 z3]
  [(arg z1) (arg z2) (arg z3)])

(defn arg-diffs [[z1 z2 z3]]
  [(arg (div z2 z1))
   (arg (div z3 z2))
   (arg (div z1 z3))])

(defn clockwise [c]
  (let [diffs (arg-diffs c)]
    (if (some #(> % (/ n/TAU 2)) diffs)
      true false)))

(comment
  ;; for render-circle arc segments
  ;; need to fix
  clockwise? (clockwise g-circle)

  arcs [(l-style :s1 color-scheme)
        (arc center radius a1 a2 clockwise?)

        (l-style :s2 color-scheme)
        (arc center radius a2 a3 clockwise?)

        (l-style :s3 color-scheme)
        (arc center radius a3 a1 clockwise?)]
  )

(defn render-circle
  "assumes g-circle is not a line"
  [g-circle color-scheme]
  (assert (not (g/collinear? g-circle)))
  (let [[z1 z2 z3] g-circle
        [a1 a2 a3] (args z1 z2 z3)

        [p1 p2 p3] (mapv coords g-circle)
        circle (g/circumcircle g-circle)
        {:keys [center radius]} (second circle)

        circ [(l-style :s1 color-scheme)
              circle]
        points [(p-style :p1 color-scheme)
                [:point p1]
                (p-style :p2 color-scheme)
                [:point p2]
                (p-style :p3 color-scheme)
                [:point p3]]]
    (concat circ points)))

(def color-scheme
  {:p1 "cyan"
   :p2 "magenta"
   :p3 "yellow"
   :s1 "red"
   :s2 "green"
   :s3 "blue"})

(def cs-1
  {:p1 "cyan"
   :p2 "magenta"
   :p3 "yellow"
   :s1 "blue"
   :s2 "green"
   :s3 "red"})

(def cs-2
  {:p1 "red"
   :p2 "purple"
   :p3 "green"
   :s1 "orange"
   :s2 "purple"
   :s3 "black"})

(def cs-3
  {:p1 "cyan"
   :p2 "magenta"
   :p3 "yellow"
   :s1 "green"})

(def cs-4
  {:p1 "red"
   :p2 "blue"
   :p3 "white"
   :s1 "purple"})

(def cs-5
  {:p1 "cyan"
   :p2 "yellow"
   :p3 "magenta"
   :s1 "orange"})

(defn render
  "transform generalized circle to
  a sequence of graphics primitives to be rendered"
  ([circle-or-line] (render circle-or-line color-scheme))
  ([circle-or-line color-scheme]
   (if (g/collinear? circle-or-line)
     (render-line circle-or-line color-scheme)
     (render-circle circle-or-line color-scheme))))

(comment
  (require '[complex.turtle.render] :reload)
  )
