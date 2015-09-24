(ns complex.pencil
  "namspace for pancils of generalized circle"
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

;; pencil creating functions
(defn pencil-iteration
  "return a sequence of generalized circles"
  ([geom f num-steps]
   (loop [step 0
          geom geom
          res []]
     (if (< step num-steps)
       (let [new-geom (mapv f geom)]
         (recur (inc step)
                new-geom
                (conj res new-geom)))
       res)))
  ([center geom f num-steps]
   (let [trans #(t/mult (t/translation (n/c center)) %)]
     (pencil-iteration geom (comp trans f) num-steps))))

(defn up
  "translate x-axis in upward direction by dx
  from given number of steps at given center or origin
  returns a sequence of generalized circle"
  [{:keys [delta num-steps center]}]
  (let [f #(t/mult (t/translation (n/c [0 delta])) %)]
    (if center
      (pencil-iteration center x-axis f num-steps)
      (pencil-iteration x-axis f num-steps))))

(defn right
  "translate y-axis"
  [{:keys [delta num-steps center]}]
  (let [f #(t/mult (t/translation (n/c [delta 0])) %)]
    (if center
      (pencil-iteration center y-axis f num-steps)
      (pencil-iteration y-axis f num-steps))))

(defn around
  "rotate x-axis given amount of degrees
  about the origin in the counter clockwise direction"
  [{:keys [delta num-steps center]}]
  (let [f #(t/mult (t/rotation delta) %)]
    (if center
      (pencil-iteration center x-axis f num-steps)
      (pencil-iteration x-axis f num-steps))))

(defn outward
  "scale unit-axis given factor"
  [{:keys [delta num-steps center]}]
  (let [f #(t/mult (t/scale (n/c [delta 0])) %)]
    (if center
      (pencil-iteration center unit-circle f num-steps)
      (pencil-iteration unit-circle f num-steps))))

;; some smaple data to describe pencils
;; moving to the outsdie of the generalized circle
(def pencil-data
  "data describing a pencil"
  [[:up {:delta 1 :num-steps 4}]
   [:up {:delta 1 :num-steps 4 :center [1 1]}]
   [:up {:delta -1 :num-steps 4 :center [0 0]}]

   [:right {:delta 1 :num-steps 4 :center [0 0]}]
   [:right {:delta 1 :num-steps 4 :center [1 1]}]
   [:right {:delta -1 :num-steps 4 :center [0 0]}]

   [:around {:delta 30 :num-steps 5 :center [0 0]}]
   [:around {:delta 30 :num-steps 5 :center [1 1]}]
   [:around {:delta -30 :num-steps 5 :center [0 0]}]

   [:outward {:delta (/ 2) :num-steps 4 :center [0 0]}]
   [:outward {:delta (/ 2) :num-steps 4 :center [1 1]}]
   [:outward {:delta 2 :num-steps 4 :center [0 0]}]
   [:outward {:delta 2 :num-steps 4 :center [1 1]}]]
  )

(def iterations
  [[:up 1]
   [:up -1]
   [:right 1]
   [:right -1]
   [:out 2]
   [:out (/ 2)]
   [:around [:degrees 30]]
   [:around [:degrees -30]]
   [:around [:tau (/ 12)]]
   [:around [:tau (/ 24)]]])

(def pencils
  [{:center [0 0]
    :iter [:up 1]
    :num-steps 5
    :trails false
    :time-between-steps 100}])

;; separate geometric transfromation from style transformation
;; for easir development and testing
;; intermingle style later
(defn generate-pencil
  "using target and pencil-data, generate sequence of generalized circls
  by iteration
  possible types of pencil data are :up :right :out :around
  for x-axis, y-axis, unit circle and x-axis respecively"
  [pencil-data]
  (match pencil-data
         [:up d] (up d)
         [:right d] (right d)
         [:around d] (around d)
         [:outward d] (outward d)))

(def c-coords
  #(mapv n/coords %))

(comment
  (require '[complex.pencil] :reload)
  (in-ns 'complex.pencil)
  (use 'clojure.repl)

  (mapv c-coords
        (generate-pencil [:up {:delta 1 :num-steps 4}]))
  [[[0 1] [1 1] :infinity]
   [[0 2] [1 2] :infinity]
   [[0 3] [1 3] :infinity]
   [[0 4] [1 4] :infinity]]

  (mapv c-coords
        (generate-pencil [:up {:delta 1 :num-steps 4 :center [1 1]}]))
  [[[1 2] [2 2] :infinity]
   [[2 4] [3 4] :infinity]
   [[3 6] [4 6] :infinity]
   [[4 8] [5 8] :infinity]]

  (defn round [c]
    (mapv
     (fn [v]
       (if (= v :infinity) :infinity
           (let [[x y] v]
             [(format "%.2f" (double x)) (format "%.2f" (double y))])))
     c))

  (mapv (comp round c-coords)
        (generate-pencil [:around {:delta 30 :num-steps 5}]))
  [[["0.00" "0.00"] ["0.87" "0.50"] :infinity]
   [["0.00" "0.00"] ["0.50" "0.87"] :infinity]
   [["0.00" "0.00"] ["0.00" "1.00"] :infinity]
   [["0.00" "0.00"] ["-0.50" "0.87"] :infinity]
   [["0.00" "0.00"] ["-0.87" "0.50"] :infinity]]
  )
