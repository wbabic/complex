(ns complex.node-test-runner
  (:require [cljs.nodejs :as nodejs]
            [cljs.test :as rest :refer-macros [run-tests]]
            [complex.number-test]
            [complex.geometry-test]
            [complex.transform-test]
            [complex.root-test]))

(nodejs/enable-util-print!)

(defn -main []
  (run-tests
   'complex.number-test
   'complex.geometry-test
   'complex.transform-test
   'complex.root-test))

(set! *main-cli-fn* -main)
