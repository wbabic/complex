(ns complex.geometry-test
  (:require
   [complex.number :as n
    :refer [mult div add sub minus recip infinity zero one i coords arg length]]
   [complex.number-test :as nt]
   [complex.geometry :as g]
   [clojure.test.check :as tc]
   [clojure.test.check.generators :as gen]
   #?@(:clj
       [[clojure.test :refer :all]
        [clojure.test.check.clojure-test :refer [defspec]]
        [clojure.test.check.properties :as prop]]
       :cljs
       [[clojure.test.check.clojure-test :refer-macros [defspec]]
        [cljs.test :as text :refer-macros [is deftest are testing run-tests]]
        [clojure.test.check.clojure-test :include-macros true]])))

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

(deftest hernitian-unit-circle
  (testing "unit circle as hermitian matrix"
    (let [uc [:circle {:center [0 0] :radius 1.0}]
          h-c (g/circle-as-matrix [0 0] 1)]
      (is (= uc (g/to-circle h-c)))
      (is (= -1 (g/determinant h-c)))
      (is (= "[1 [0 0] [0 0] -1]" (g/to-string h-c))))))

(deftest hernitian-imaginary-circle
  (testing "imaginary circle as hermitian matrix"
    (let [ic [:circle {:center [0 0] :radius (n/c [0 1.0])}]
          h [1 zero zero 1]]
      (is (= ic (g/to-circle h)))
      (is (= 1 (g/determinant h))))))

(deftest hermitian-line
  (testing "line perpendicular to [2 3]"
    (let [b (n/c [2 3])
          l (g/line-as-matrix b)
          l-coords (g/line-coords l)]
      (is (= [4 6 0] l-coords))
      (is (g/point-on-line? [-3 2] l-coords))
      (is (g/point-on-line? [3 -2] l-coords)))))

(deftest line-through-two-points
  (testing "line through [1 0] [0 1]"
    (let [l [:line [1 0] [0 1]]
          l-coords (g/line-coords l)]
      (is (= [1 1 1] l-coords))
      (is (g/point-on-line? [1 0] l-coords))
      (is (g/point-on-line? [(/ 2) (/ 2)] l-coords)))))

(deftest intersection-lines-point
  (testing "interssection of two lines"
    (let [o [0 0]
          e1 [1 0]
          e2 [0 1]
          l1 [:line o e1]
          l2 [:line o e2]]
      (is (= [0 0] (g/intersection l1 l2))))))

(deftest intersection-lines-complex
  (testing "intersection of two lines using complex points"
    (let [l1 [zero one]
          l2 [zero i]]
      (is (= [0 0] (g/intersection l1 l2))))))

(deftest circumcircle
  (testing "circumcirle of three points"
    (let [l [one i (minus one)]
          uc [:circle {:center [0 0] :radius 1.0}]]
      (is (= uc (g/circumcircle l))))))

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
  (run-tests)

  )
