(ns complex.transit.clojure
  "explore clojure specific transit operations"
  (:require [cognitect.transit :as t]
            [complex.schemas :as s])
  (:import [java.io File ByteArrayInputStream ByteArrayOutputStream OutputStreamWriter]))

(comment
  (require 'complex.transit.clojure)
  (in-ns 'complex.transit.clojure)
  (use 'clojure.repl)

  (require 'complex.transit.clojure :reload)

  )


(comment
  (class (read-string (pr-str (s/test-record [0 0] 1))))
  ;;=> "#complex.schemas.TestRecord{:center [0 0], :radius 1}"

  )
