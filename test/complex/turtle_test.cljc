(ns complex.turtle-test
  (:require
   [complex.number :as n
    :refer [mult div add sub minus recip infinity zero one i coords arg length]]
   [complex.number-test :as nt]
   [complex.geometry :as g]
   [complex.turtle :as turtle]
   [complex.turtle.render :as render]
   [clojure.test.check :as tc]
   [clojure.test.check.generators :as gen]
   [clojure.test.check.properties :as prop]
   #?@(:clj
       [[clojure.test :refer :all]
        [clojure.test.check.clojure-test :refer [defspec]]]
       :cljs
       [[cljs.test :as text :refer-macros [is deftest are testing run-tests]]
        [clojure.test.check.clojure-test :include-macros true]])))

(deftest render-circle
  (testing "render unit-circle"
    (let [st turtle/standard-turtle
          unit-circle (-> st :circles :unit-circle)
          unit-circle-style (-> st :style :unit-circle)]
      (is (= [[:style {:stroke :orange}] [:circle {:center [0N 0N], :radius 1.0}]]
             (render/render-circle unit-circle unit-circle-style))))))
