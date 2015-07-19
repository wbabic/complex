(ns complex.schemas
  "schemas for complex library"
  (:require [schema.core :as s :include-macros true]))

(def Point [s/Num])
(def Circle "a schema for a circle" {:center Point :radius s/Num})
(def Arc "a schema for an arc" {:center Point
                                :radius s/Num
                                :start s/Num
                                :end s/Num
                                :clockwise s/Bool})

(def point  [(s/one (s/eq :point) "tag") (s/one Point "data")])
(def circle [(s/one (s/eq :circle) "tag") (s/one Circle "data")])
(def arc [(s/one (s/eq :arc) "tag") (s/one Arc "data")])
(def line   [(s/one (s/eq :line) "tag")
             (s/one Point "data")
             (s/one Point "data")])
(def rect [(s/one (s/eq :rect) "tag")
           (s/one Point "data")
           (s/one Point "data")])
(def triangle [(s/one (s/eq :triangle) "tag")
               (s/one Point "data")
               (s/one Point "data")
               (s/one Point "data")])

(comment
  (s/validate point [:point [0 0]])
  (s/check circle [:circle {:center [0 0] :radius 1}])
  (s/validate circle [:circle {:center [0 0] :radius 1}])
  (s/validate line [:line [0 0] [1 0]])
  (s/validate rect [:rect [0 0] [1 0]])
  (s/validate triangle [:triangle [0 0] [1 0] [0 1]])
  )

(comment
  ;; from clojure repl
  (require 'complex.schemas :reload)
  (in-ns 'complex.schemas)
  (use 'clojure.repl)
  (doc schema.core)
  (doc s/Num)

  (s/defrecord TestRecord
      [center :- Point
       radius :- s/Num])

  (s/defn test-record :- TestRecord
    [center :- Point
     radius :- s/Num]
    (TestRecord. center radius))

  (schema.utils/class-schema TestRecord)
  ;;=> (record complex.schemas.TestRecord {:center [java.lang.Number], :radius java.lang.Number})

  (s/validate TestRecord (test-record [0 0] 1))
  ;;=> #complex.schemas.TestRecord{:center [0 0], :radius 1}

  (s/fn-schema test-record)
  ;;=> (=> (record complex.schemas.TestRecord {:center [java.lang.Number], :radius java.lang.Number}) [java.lang.Number] java.lang.Number)

  (s/with-fn-validation (test-record [0 0] :a))
  ;;=> ExceptionInfo Input to test-record does not match schema: [nil (named (not (instance? java.lang.Number :a)) radius)]  complex.schemas/eval3788/test-record--3789 (schemas.cljc:20)

  (s/explain TestRecord)
  (s/explain (s/fn-schema test-record))

  (require 'schema.test)
  (doc schema.test/validate-schemas)
  (doc schema.test/deftest)

  (s/defrecord Complex
      [x :- s/Num
       y :- s/Num])

  (s/defrecord Hermitian-circle
      [A :- s/Num
       B :- Complex
       C :- Complex
       D :- s/Num])
  )
