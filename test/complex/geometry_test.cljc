(ns complex.geometry-test
  (:require
   [complex.number :as n
    :refer [mult div add sub minus recip infinity zero one i coords arg length]]
   [complex.number-test :as nt]
   [complex.geometry :as g]
   #?@(:clj
       [[clojure.test :refer :all]
        [clojure.test.check :as tc]
        [clojure.test.check.generators :as gen]
        [clojure.test.check.properties :as prop]
        [clojure.test.check.clojure-test :refer [defspec]]]
       :cljs
       [[cljs.test :as text :refer-macros [is deftest are testing run-tests]]
        [cljs.test.check :as tc]
        [cljs.test.check.properties :as prop :include-macros true]
        [cljs.test.check.generators :as gen]
        [cljs.test.check.cljs-test :refer-macros [defspec]]])))

(deftest inversion-test
  (testing "inversion in a circle"
    (let [f (g/inversion)]
      (is (= infinity (f zero)))
      (is (= zero (f infinity))))))

(deftest midpoint-test
  (testing "midpoint of two complex numbers"
    (let [m (n/c [(/ 2) (/ 2)])]
      (is (= m (g/midpoint one i))))))

(deftest param-circle-test
  (testing "parameterized equation for unit circle"
    (let [p-uc (g/circle i one (minus one))]
      (is (= i (p-uc 0)))
      (is (= one (p-uc 1)))
      (is (= (minus one) (p-uc :infinity))))))

(deftest param->standard
  (testing "standard eq for param circle"
    (let [uc [:circle {:center [0 0] :radius 1.0}]
          p-uc (g/three-point->param i one (minus one))
          s-uc (apply g/param->standard p-uc)]
      (is (= uc s-uc)))))

(comment
  (def gen-circle
    (gen/vector nt/complex-gen 3))

  ;; todo - make sure numbers are unique
  (mapv #(mapv coords %) (gen/sample gen-circle))
  (tc/quick-check 100 complex-prop)
  )

(comment
  ;; to run these tests from a clojure repl:
  (require 'complex.geometry-test :reload)
  (in-ns 'complex.geometry-test)
  (clojure.test/run-tests)
  )
