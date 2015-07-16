(ns complex.protocols)

;; Complex arithmetic protocol
(defprotocol Complex
  "protocol for complex number"
  (coords [z] "x y coordinates")
  (plus [z w] "addition of complex numbers")
  (minus [z] "negative a complex number")
  (times [z w] "multiplication of two complex numbers")
  (recip [z] "one over z")
  (conjugate [z] "the conjugate of z")
  (length [z] "the length of z")
  (arg [z] "the argument of z"))

;; the algebra of
;; linear fractional transformations

(defprotocol Mobius
  "protocol for mobius transformations"
  (entries [T] "returns a vector of complex entries of mobius transformation")
  (det [_] "ad - bc")
  (mult [T z] "apply a mobius transformation to a complex number")
  (compose [T1 T2] "compose two mobius transformations")
  (inverse [T] "return inverse of mobius transformation")
  (conjugation [T S] "return conjugate of T wrt conjugate mapping S")
  (trace [T] "trace of a mobius transform"))

(defprotocol Transformable
  "protocol for things that are able to be transformed"
  (transform [_ transformation]))

(defprotocol Renderable
  "protocal for things that con be rebdered"
  (render [_ context]))

(comment
  (require 'complex.protocols)
  (require 'complex.protocols :reload)
  (in-ns 'complex.protocols)
  )
