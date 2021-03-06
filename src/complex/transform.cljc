(ns complex.transform
  "the algebra of linear fractional transformations"
  (:require [complex.number :as n
             :refer [div add sub minus recip infinity zero one i arg length]]
            [complex.vector :as v]))

(defprotocol Mobius
  "protocol for mobius transformations"
  (coords [T] "returns a vector of complex entries of mobius transformation")
  (det [_] "ad - bc")
  (mult [T z] "apply a mobius transformation to a complex number")
  (compose [T1 T2] "compose two mobius transformations")
  (inverse [T] "return inverse of mobius transformation")
  (conjugate [T S] "return conjugate of T wrt conjugate mapping S")
  (trace [T] "trace of a mobius transform"))

(defrecord mobius-trans [a b c d]
  Mobius
  (coords [_] [a b c d])
  (det [_] (add (n/mult a d) (minus (n/mult b c))))
  (mult [_ z] (cond
                (= z infinity) (div a c)
                (= z zero) (div b d)
                (= z (minus (div b a))) zero
                (= z (minus (div d c))) infinity
                :else (div (add (n/mult a z) b)
                           (add (n/mult c z) d))))
  (inverse [T]
    (let [k (det T)
          m #(n/mult % k)]
      (mobius-trans. (m d)
                     (m (minus b))
                     (m (minus c))
                     (m a))))
  (compose [_ T2] (let [[a2 b2 c2 d2] (coords T2)]
                    (mobius-trans. (n/dot [a b] [a2 c2])
                                   (n/dot [a b] [b2 d2])
                                   (n/dot [c d] [a2 c2])
                                   (n/dot [c d] [b2 d2]))))
  (conjugate [T S] (compose S (compose T (inverse S))))
  (trace [_] (add a d)))

(def I (mobius-trans. one zero zero one))
(defn scale [a] (mobius-trans. a zero zero one))

(def S1 (let [w (n/c [1 0.4])]
          (scale w)))

;; J: z -> 1/z
(def J
  (mobius-trans. zero i i zero))

(defn translation [b]
  (mobius-trans. one b zero one))

;; transformation which maps one and -one to zero and infinity respectively
;; preserving the real-axis
(def T2 (mobius-trans. one (minus one) one one))

;; Cayley Traansform
(def Cayley (mobius-trans. one (minus i) one i))

(defn rotation
  "return the mobius transformation
  of a rotation about P, if given, or origin otherwise,
  by given degrees"
  ([degrees]
   (scale (n/complex-polar degrees)))
  ([P degrees]
   (conjugate (rotation degrees) (translation P))))

(def transforms
  [{:name "Identity"
    :text "z -> z"
    :transform I}
   {:name "Inversion"
    :text "z -> 1/z"
    :transform J}
   {:name "Cayley"
    :text "z -> (z-i)/(z+i)"
    :transform Cayley}
   {:name "T2"
    :text "z -> (z-1)/(z+1)"
    :transform T2}
   {:name "S1"
    :text "z -> (1 + 0.4i)z"
    :transform S1}])

(comment
  (require 'complex.transform :reload)
  (in-ns 'complex.transform)
  )
