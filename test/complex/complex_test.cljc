(ns complex.complex-test
  (:require
   [complex.number :as c
    :refer [mult div add sub minus recip infinity zero one i coords arg length]]
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

(deftest test-arg
  (testing "complex arg"
    (let [z1 (c/c [1 1])
          z2 (c/c [1 -1])]
      (are [a b] (== a b)
           45 (c/rad->deg (arg z1))
           315 (c/rad->deg (arg z2))
           180 (c/rad->deg (arg (minus one)))
           90(c/rad->deg (arg i))))))

(deftest div-by-zero
  (testing "divide by zero"
    (is (= infinity (div one zero)))
    (is (= zero (div one infinity)))))

(comment
  ;; to run these tests from a clojure repl:
  (require 'complex.complex-test :reload)
  (in-ns 'complex.complex-test)
  (clojure.test/run-tests)
  )

(def complex-gen
  (gen/fmap c/complex-rect (gen/vector gen/ratio 2)))

(def non-zero-complex-gen
  (gen/such-that #(not (= zero %)) complex-gen))

(def gen-circle
  (gen/vector complex-gen 3))

(def complex-prop
  (prop/for-all [z complex-gen]
                (satisfies? c/Complex z)))

(comment
  (mapv #(mapv coords %) (gen/sample gen-circle))
  (tc/quick-check 100 complex-prop)
  )

(defspec complex-gen-satisfies-protocol complex-prop)

(def additive-inverse-prop
  (prop/for-all [z complex-gen]
                (= zero (add z (minus z)))))

(defspec additive-inverse additive-inverse-prop)

(defn almost-zero? [z]
  (let [[x y] (coords z)
        epsilon 10e-15
        f (fn [x] (< (Math/abs x) epsilon))]
    (and (f x) (f y))))

(def mult-inverse-prop
  (prop/for-all [z non-zero-complex-gen]
                (let [p (mult z (recip z))
                      r (sub p one)]
                  (almost-zero? r))))

(defspec mult-inverse mult-inverse-prop)
