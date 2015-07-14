(ns complex.complex-test
  (:require
   [complex.number :as c]
   #?(:clj
      [clojure.test :refer :all]
      :cljs
      [cljs.test :as text :refer-macros [is deftest are testing run-tests]])))

(deftest test-test
  (testing "if tests worrk"
    (is (= true false))))
