(ns complex.number-test
  (:require
   [complex.number :as n
    :refer [mult div add sub minus recip infinity zero one i coords arg length]]
   [clojure.test.check :as tc]
   [clojure.test.check.generators :as gen]
   #?@(:clj
       [[clojure.test :refer :all]
        [clojure.test.check.clojure-test :refer [defspec]]
        [clojure.test.check.properties :as prop]]
       :cljs
       [[cljs.test :as text :refer-macros [is deftest are testing run-tests]]
        [clojure.test.check.clojure-test :refer-macros [defspec]]
        [clojure.test.check.properties :as prop :include-macros true]])))

(deftest test-arg
  (testing "complex arg"
    (let [z1 (n/c [1 1])
          z2 (n/c [1 -1])]
      (are [a b] (== a b)
           45 (n/rad->deg (arg z1))
           315 (n/rad->deg (arg z2))
           180 (n/rad->deg (arg (minus one)))
           90(n/rad->deg (arg i))))))

(deftest div-by-zero
  (testing "divide by zero"
    (is (= infinity (div one zero)))
    (is (= zero (div one infinity)))))

(def complex-gen
  (gen/fmap n/c (gen/vector gen/ratio 2)))

(def non-zero-complex-gen
  (gen/such-that #(not (= zero %)) complex-gen))

(def complex-prop
  (prop/for-all [z complex-gen]
                (satisfies? n/Complex z)))

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

(comment
  ;; to run these tests from a clojure repl:
  (require 'complex.complex-test :reload)
  (in-ns 'complex.complex-test)
  (clojure.test/run-tests)
  )
