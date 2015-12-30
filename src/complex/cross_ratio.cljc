(ns complex.cross-ratio
  "all things related to the complex cross ratio"
  (:require [complex.number :as n
             :refer [div add sub minus recip infinity zero one i arg length]]
            [complex.vector :as v]))

(comment
  (require '[complex.cross-ratio] :reload)
  (in-ns 'complex.cross-ratio)
  (use 'clojure.repl)
  )

(defn cross-ratio
  "the cross tatio of 4 complex numbers"
  [z1 z2 z3 z4]
  (div (* (sub z1 z3) (sub z2 z4))
       (* (sub z2 z3) (sub z1 z4))))
