(ns complex.test-runner
  (:require #?(:clj
               [clojure.test :refer [run-tests]]
               :cljs
               [cljs.test :refer-macros [run-tests]])
            [complex.number-test]))

#?(:cljs [(enable-console-print!)])

(defn runner []
  #?(:cljs
     (if (cljs.test/successful?
          (run-tests
           'complex.number-test))
       0
       1)
     :clj
     (if (clojure.test/successful?
          (run-tests
           'complex.number-test))
       "Success"
       "Failure")))
