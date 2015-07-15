(ns complex.number
  (:require [complex.vector :as v]))

(def ^:const PI Math/PI)
(def TAU (* 2 PI))
(defn mod-tau [x] (mod x TAU))

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

(defn rad->deg [rad]
  (* rad (/ 360 TAU)))

(defn deg->rad [deg]
  (* deg (/ TAU 360)))

(declare c)
(declare infinity)
(declare zero)
(declare undefined)

(defn product
  ([] [1 0])
  ([z] z)
  ([z1 z2] (let [[x1 y1] z1
                 [x2 y2] z2]
             [(- (* x1 x2) (* y1 y2))
              (+ (* y1 x2) (* x1 y2))]))
  ([z1 z2 & rest] (reduce product (product z1 z2) rest)))

(defrecord complex [x y]
  Complex
  (coords [_] [x y])
  (plus [_ w] (if (= w infinity)
                infinity
                (c (v/sum [x y] (coords w)))))
  (minus [z] (complex. (- x) (- y)))
  (times [_ w]
    (cond (= w zero) zero
          (= w infinity) infinity
          (number? w) (c [(* x w) (* y w)])
          :else
          (c (product [x y] (coords w)))))
  (recip [z] (let [d (+ (* x x) (* y y))]
             (complex. (/ x d) (/ (- y) d))))
  (conjugate [_] (complex. x (- y)))
  (length [_] (v/len [x y]))
  (arg [_] (mod-tau (v/angle [x y]))))

(defn c [[x y]]
  (if (and (zero? x) (zero? y))
    zero
    (complex. x y)))

(def one (c [1 0]))
(def i (c [0 1]))

(defn polar->rect
  ([angle] (polar->rect 1 angle))
  ([length angle] [(* length (Math/cos angle))
                   (* length (Math/sin angle))]))

(defn complex-polar
  ([degrees] (complex-polar 1 degrees))
  ([radius degrees]
   (let [radians (deg->rad degrees)]
     (c (polar->rect radius radians)))))

(def zero
  (reify Complex
    (plus [_ w] w)
    (minus [z] z)
    (times [z w]
      (if (= w infinity)
        undefined
        z))  ;; unless w = infinity, then undefined
    (recip [_] infinity)
    (length [_] 0)
    (arg [_] 0)
    (conjugate [z] z)
    (coords [_] [0 0])))

(def infinity
  (reify Complex
    (coords [_] "infinity")
    (plus [z w]
      (if (= w infinity)
        undefined
        z))
    (minus [z] z)
    (conjugate [z] z)
    (arg [_] (assert false "infinity has no arg"))
    (length [_] (assert false "infinity has no length"))
    (times [this w]
      (if (= zero w)
        undefined
        this))
    (recip [_] zero)))

(def undefined
  (reify Complex
    (coords [_] "undefined")))

(defn add
  "add complex numbers"
  ([] zero)
  ([z] z)
  ([z w]
   (if (and (= z infinity) (= w infinity))
     undefined
     (plus z w)))
  ([z w & rest] (reduce plus (plus z w) rest)))

(defn sub [z w]
  (add z (minus w)))

(defn mult
  "multiply complex numbers"
  ([] one)
  ([z] z)
  ([z w] (times z w))
  ([z w & rest] (reduce times (times z w) rest)))

(defn div
  "divide two complex numbers"
  [z w]
  (cond
    (and (= z infinity) (= w infinity)) undefined
    (and (= z zero) (= w zero)) undefined
    (= w infinity) zero
    (= z zero) zero
    (= w zero) infinity
    :else (times z (recip w))))

(defn len-sq [z] (mult z (conjugate z)))

(defn distance
  "distance between two complex numbers
  assuming z w not infinity"
  [z w]
  (let [z-w (add z (minus w))
        d (v/len (coords z-w))]
    d))

(defn sqrt
  "square root of given number"
  [n]
  (let [n-pos (Math/abs n)
        n-pos-sqrt (Math/sqrt n-pos)]
    (c [0 n-pos-sqrt])))

(comment
  ;; from the clojure repl
  (require '[complex.number] :reload)
  (in-ns 'complex.number)
  )
