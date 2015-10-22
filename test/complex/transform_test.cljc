(ns complex.transform-test
  (:require
   [complex.number :as n
    :refer [mult div add sub minus recip infinity zero one i coords arg length]]
   [complex.number-test :as nt]
   [complex.geometry :as g]
   [complex.transform :as t]
   [clojure.test.check :as tc]
   [clojure.test.check.generators :as gen]
   [clojure.test.check.properties :as prop]
   #?@(:clj
       [[clojure.test :refer :all]
        [clojure.test.check.clojure-test :refer [defspec]]]
       :cljs
       [[cljs.test :as text :refer-macros [is deftest are testing run-tests]]
        [clojure.test.check.clojure-test :refer-macros [defspec]]])))

(deftest identity-transform
  (testing "identity transform"
    (let [I #(t/mult t/I %)
          z (n/c [1 2])]
      (is (= one (I one)))
      (is (= zero (I zero)))
      (is (= z (I z))))))

(deftest scale-transform
  (testing "scale transform"
    (let [z (n/c [1 1])
          w (n/c [-1 1])
          s #(t/mult (t/scale z) %)]
      (is (= w (s i))))))

(comment
  ;; to run these tests from a clojure repl:
  (require 'complex.transform-test :reload)
  (in-ns 'complex.transform-test)
  (clojure.test/run-tests)
  )
