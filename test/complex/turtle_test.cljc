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

(deftest render-unit-circle
  (testing "render unit-circle"
    (let [st turtle/standard-turtle]
      (is (= [[:style {:stroke :orange}]
              [:circle {:center [0N 0N], :radius 1.0}]
              [:style {:fill :lt-orange}]
              [:disk {:center [0N 0N], :radius 1.0}]]
             (render/render-circle-or-line :unit-circle st))))))

(deftest render-x-axis
  (testing "render x-axis"
    (let [st turtle/standard-turtle]
      (is (= [[:style {:stroke :green}]
              [:line [0 0] [1 0]]
              [:line [1 0] [100000 0]]
              [:line [-99999 0] [0 0]]
              [:style {:fill :lt-green}]
              [:quad [-99999 0] [100000 0] [100000 199999] [-99999 199999]]]
             (render/render-circle-or-line :x-axis st))))))
