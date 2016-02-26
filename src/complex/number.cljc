(ns complex.number
  "Extended Complex Number"
  (:require [complex.vector :as v]))

(def ^:const PI Math/PI)
(def TAU (* 2 PI))
(defn mod-tau [x] (mod x TAU))
(defn mod-1 [x] (mod x 1))

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
(def negative-one (minus one))
(def negative-i (minus i))

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
    (coords [_] :infinity)
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
  ([z w] (plus z w))
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
  ([] one)
  ([z] (div one z))
  ([z w]
   (cond
     (and (= z infinity) (= w infinity)) undefined
     (and (= z zero) (= w zero)) undefined
     (= w infinity) zero
     (= z zero) zero
     (= w zero) infinity
     :else (times z (recip w)))))

(defn len-sq [z]
  (let [w (mult z (conjugate z))
        [x y] (coords w)]
    x))

(defn distance
  "distance between two complex numbers
  assuming z w not infinity"
  [z w]
  (let [z-w (add z (minus w))
        d (v/len (coords z-w))]
    d))

(defn dot
  "complex dot product for 2x2 complex vectors"
  [[a1 b1] [a2 b2]]
  (add (mult a1 a2) (mult b1 b2)))

(defn cross
  "returns 3d vector of z1 cross z2
where z1 and z2 are treated as vectors in a plane"
  [z1 z2]
  (let [[x1 y1] (coords z1)
        [x2 y2] (coords z2)]
    (v/cross-3d [x1 y1 0] [x2 y2 0])))

(defn sqrt
  "square root of given number"
  [n]
  (let [n-pos (Math/abs n)
        n-pos-sqrt (Math/sqrt n-pos)]
    (c [0 n-pos-sqrt])))

(defn pow
  "raise complex number z to the nth power
  where n is given ineger"
  [z n]
  (assert (integer? n) "n must be an integer")
  (loop [w z n n]
    (cond (zero? n) one
          (= 1 n) w
          :else (recur (mult w z) (dec n)))))
(comment
  ;; from the clojure repl
  (require '[complex.number] :reload)
  (in-ns 'complex.number)
  )
