(ns complex.schemas
  "schemas for complex library"
  (:require [schema.core :as s :include-macros true]))

(def Point [s/Num])
(def Circle "a schema for a circle" {:center Point :radius s/Num})

(def circle [(s/one (s/eq :circle) "tag") (s/one Circle "data")])

(comment
  (s/check circle [:circle {:center [0 0] :radius 1}])
  (s/validate circle [:circle {:center [0 0] :radius 1}])
  )

(comment
  ;; from clojure repl
  (require 'complex.schemas :reload)
  (in-ns 'complex.schemas)
  (use 'clojure.repl)
  (doc s/Num)
  )
