(ns complex.target
  "namspace for pancils of generalized circle"
  (:require [complex.number :as n
             :refer [infinity zero one i minus]]
            [complex.geometry :as g]
            [complex.transform :as t]))

(def target
  {:x-axis
   {:geometry [zero one infinity]
    :style {:stroke :green :fill :lt-green}}
   :y-axis
   {:geometry [zero i infinity]
    :style {:stroke :purple :fill :lt-purple}}
   :unit-circle
   {:geometry [one i (minus one)]
    :style {:stroke :orange :fill :lt-orange}}
   :one
   {:geometry one
    :style {:stroke :grey :fill :cyan}}
   :m1
   {:geometry (minus one)
    :style {:stroke :grey :fill :blue}}
   :i
   {:geometry i
    :style {:stroke :grey :fill :red}}
   :mi
   {:geometry (minus i)
    :style {:stroke :grey :fill :magenta}}
   :zero
   {:geometry zero
    :style {:stroke :grey :fill :yellow}}
   :infinity
   {:geometry infinity
    :style {:stroke :grey :fill :black}}})

;; pencil creating functions
(defn pencil [geom style g-fn s-fn num-steps]
  (loop [step 0
         geom geom
         style style
         res []]
    (if (< step num-steps)
      (let [new-geom (mapv g-fn geom)
            new-style (s-fn style)]
        (recur (inc step)
               new-geom
               new-style
               (conj res {:geometry new-geom
                          :style new-style})))
      res)))

(defn up
  "translate x-axis in upward direction by dx
  for given number of steps
  returning a data sequence
  with optional color transformation"
  ([dx num-steps] (up dx num-steps identity))
  ([dx num-steps style-trans]
   (let [x-axis (:x-axis target)
         geom (:geometry x-axis)
         style (:style x-axis)
         f #(t/mult (t/translation (n/c [dx 0])) %)]
     (pencil geom style f style-trans num-steps))))

(defn right
  "translate y-axis to the right by dx
  for given number of steps
  returning a data sequence
  with optional color transformation"
  ([dy num-steps] (up dy num-steps identity))
  ([dy num-steps style-trans]
   (let [y-axis (:y-axis target)
         geom (:geometry y-axis)
         style (:style y-axis)
         f #(t/mult (t/translation (n/c [dy 0])) %)]
     (pencil geom style f style-trans num-steps))))

(defn around
  "rotate x-axis given amount of degrees
  about the origin in the counter clockwise direction"
  ([degrees num-steps] (around degrees num-steps identity))
  ([degrees num-steps style-trans]
   (let [x-axis (:x-axis target)
         geom (:geometry x-axis)
         style (:style x-axis)
         f #(t/mult (t/rotation degrees) %)]
     (pencil geom style f style-trans num-steps))))

(comment
  (up 1 4)
  (right 1 4)
  (mapv (comp #(mapv n/coords %) :geometry) (around 30 6))
  )

(comment
  (require '[complex.target] :reload)
  (in-ns 'complex.target)
  )
