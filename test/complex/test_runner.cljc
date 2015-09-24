(ns complex.test-runner
  (:require #?(:clj
               [clojure.test :refer [run-tests]]
               :cljs
               [cljs.test :refer-macros [run-tests]])
            [complex.number-test]
            [complex.geometry-test]
            [complex.transform-test]
            [complex.root-test]))

#?(:cljs [(enable-console-print!)])

(defn runner []
  #?(:cljs
     (if (cljs.test/successful?
          (run-tests
           'complex.number-test
           'complex.geometry-test
           'complex.transform-test
           'complex.root-test))
       0
       1)
     :clj
     (if (clojure.test/successful?
          (run-tests
           'complex.number-test
           'complex.geometry-test
           'complex.transform-test
           'complex.root-test))
       "Success"
       "Failure")))
