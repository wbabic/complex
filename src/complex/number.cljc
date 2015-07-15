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

(declare complex-rect)
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
                (complex-rect (v/sum [x y] (coords w)))))
  (minus [z] (complex. (- x) (- y)))
  (times [_ w]
    (cond (= w zero) zero
          (= w infinity) infinity
          (number? w) (complex-rect [(* x w) (* y w)])
          :else
          (complex-rect (product [x y] (coords w)))))
  (recip [z] (let [d (+ (* x x) (* y y))]
             (complex. (/ x d) (/ (- y) d))))
  (conjugate [_] (complex. x (- y)))
  (length [_] (v/len [x y]))
  (arg [_] (mod-tau (v/angle [x y]))))

(defn complex-rect [[x y]]
  (if (and (zero? x) (zero? y))
    zero
    (complex. x y)))

(def c complex-rect)

(def one (complex-rect [1 0]))
(def i (complex-rect [0 1]))

(defn polar->rect
  ([angle] (polar->rect 1 angle))
  ([length angle] [(* length (Math/cos angle))
                   (* length (Math/sin angle))]))

(defn complex-polar
  ([degrees] (complex-polar 1 degrees))
  ([radius degrees]
   (let [radians (deg->rad degrees)]
     (complex-rect (polar->rect radius radians)))))

(comment
  ;; from the clojure repl
  (require '[complex.number] :reload)
  (in-ns 'complex.number)
  )

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

(defn c-dot
  "dot product for 2x2 complex vectors"
  [[a1 b1] [a2 b2]]
  (add (mult a1 a2) (mult b1 b2)))

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
    (complex-rect [0 n-pos-sqrt])))


;; inversion in a general circle C
;; where C has center P and radius r
;; is z ->

(defn inversion
  "inversion in a circle"
  ([] (fn [z] (recip (conjugate z))))
  ([circle]
   (let [{:keys [center radius]} circle
         T (inversion)
         Q (complex-rect center)
         r (complex-rect [radius 0])
         S (fn [z] (add (mult r z) Q))
         S-inv (fn [w] (mult (recip r) (add w (minus Q))))]
     (comp S T S-inv))))

(defn midpoint
  "midpoint -f two complex numbers"
  [z w]
  (mult (add z w) (/ 2)))

;; parameterized circles
;; from deaux
(defn param-circle
  "returns a parameterized circle
  given four complex numbers
  where :infinity is an accepted value"
  [a b c d]
  (fn [t]
    (if (= t :infinity)
      (div a c)
      (div (add (mult a t) b)
                   (add (mult c t) d)))))

(defn three-point->param
  [p q r]
  (let [a (mult r (sub p q))
        b (mult q (sub r p))
        c (sub p q)
        d (sub r p)]
    [a b c d]))

(defn circle
  "return a parameterized circle through the trhee given complex numbers"
  [p q r]
  (apply param-circle (three-point->param p q r)))

(defn f [u v]
  (sub (mult u (conjugate v)) (mult v (conjugate u))))

(defn g [a b c d] (sub (mult a (conjugate d)) (mult b (conjugate c))))

(defn param->general
  "return given parameterized circle in general form
  "
  [a b c d]
  (let [ alpha-bar (minus (div (g a b c d) (f c d)))
        beta (minus (div (f a b) (f c d)))]
    [one (conjugate alpha-bar) alpha-bar beta]))

(defn param->standard
  [a b c d]
  (let [[_ alpha alpha-bar beta] (param->general a b c d)]
    [:circle {:center (coords (minus alpha-bar))
              :radius (length (sub (mult alpha alpha-bar) beta))}]))
