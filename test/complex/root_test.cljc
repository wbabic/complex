(ns complex.root-test
  (:require
   [complex.roots :as r]
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

(comment
  (require '[complex.root-test] :reload)
  (in-ns 'complex.root-test)
  (run-tests)
  )

(deftest is-zero
  (testing "zero values"
    (is (r/my-zero? 0))
    (is (r/my-zero? [:root 0]))
    (is (r/my-zero? [:polar 0 [:tau (/ 10)]]))
    (is (r/my-zero? [:number 0 [:root 0]]))
    (is (r/my-zero? (r/collect-terms [:number 0 [:root 2] [:root 2 -1]])))))

(deftest is-one
  (testing "values of unity"
    (is (r/one? 1))
    (is (r/one? [:tau 0]))
    (is (r/one? [:root 1]))
    (is (r/one? [:polar 1 [:tau 0]]))
    (is (r/one? [:number 1]))
    (is (r/one? [:number 1 [:root 0]]))
    (is (r/one? (r/collect-terms [:number 1 [:root 2] [:root 2 -1]])))
    (is (r/one? [:rect 1 0]))))

(def gen-root-base (gen/tuple (gen/return :root) gen/pos-int))

(def gen-root-base-mult (gen/tuple (gen/return :root) gen/pos-int gen/ratio))

(def gen-root (gen/one-of [gen-root-base gen-root-base-mult]))

(def gen-non-zero-root
  (gen/such-that #(not (r/my-zero? %)) gen-root))

(def root-property
  (prop/for-all [r gen-root]
                (r/root-number? r)))

(def additive-inverse-prop
  (prop/for-all [r gen-root]
                (r/my-zero? (r/add-root r (r/negative r)))))

(def multiplicative-inverse-prop
  (prop/for-all [r gen-non-zero-root]
                (r/one? (r/mult-root r (r/reciprocal r)))))

(defspec is-root root-property)
(defspec additive-inverse additive-inverse-prop)
(defspec multiplicative-inverse multiplicative-inverse-prop)

(deftest alpha-conjugate
  (testing "conjugate alpha"
    (is (= (r/collect-terms r/beta) (r/conjugate r/alpha)))))

(def conjugate-prop
  (prop/for-all [r gen-root]
                (= (r/evaluate r) (r/evaluate (r/conjugate (r/conjugate r))))))

(defspec conjugate-involution conjugate-prop)

(comment
  (tc/quick-check 1000 root-property)
  (tc/quick-check 1000 additive-inverse-prop)
  (tc/quick-check 1000 multiplicative-inverse-prop)
  (tc/quick-check 1000 conjugate-prop)
  )

(deftest phi-test
  (testing "basic phi relationships"
    (is (= (r/reciprocal r/Phi) r/phi))
    (is (= r/phi (r/reciprocal r/Phi)))
    (is (= (r/add 1 r/phi) (r/collect-terms r/Phi)))))
