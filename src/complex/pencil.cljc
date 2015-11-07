(ns complex.pencil
  "namspace for pencils of generalized circle"
  (:require [complex.number :as n
             :refer [infinity zero one i negative-one negative-i]]
            [complex.transform :as t]
            #?(:clj
               [clojure.core.match :refer [match]]
               :cljs
               [cljs.core.match :refer-macros [match]])))

(def x-axis      [zero one infinity])
(def y-axis      [zero i infinity])
(def unit-circle [one i negative-one])

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
