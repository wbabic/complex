(ns complex.test-runner
  (:require [cljs.test :refer-macros [run-tests]]
            [complex.complex-test]))

(enable-console-print!)

(defn runner []
  (if (cljs.test/successful?
        (run-tests
         'complex.complex-test))
    0
    1))
