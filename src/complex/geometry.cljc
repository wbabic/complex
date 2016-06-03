(ns complex.geometry
  (:require
   [complex.number :as n
    :refer [mult div add sub minus recip infinity zero one i coords arg length conjugate]]
   [complex.vector :as v]
   #?(:clj
      [clojure.core.match :refer [match]]
      :cljs
      [cljs.core.match :refer-macros [match]])))

(defn point-on-line?
  "check if given point p is on line l
  where l is given by [a b c] with ax + by = c
  being the equation for the line"
  [p l]
  (let [[x y] p
        [a b c] l]
    (< (* (- c (+ (* a x) (* b y))) (- c (+ (* a x) (* b y)))) 10e-10)))

(declare line-coords)

(defn collinear?
  "return true if given complex numbers are collinear
  any two points and infinity are collinear"
  [l]
  (if (some #(= infinity %) l)
    true
    (let [[c1 c2 c3] l
          [p1 p2 p3] (mapv coords [c1 c2 c3])
          l (line-coords p1 p2)]
      (point-on-line? p3 l))))

(defn midpoint
  "midpoint -f two complex numbers"
  [z w]
  (mult (add z w) (/ 2)))

(defn perp-bisector
  "return perp bisector of line segment z w
  where z and w are complex numbers not equal to infinity
  and result is returned as two complex numbers"
  [[z w]]
  (let [m (midpoint z w)
        t #(add m %)
        t-inv #(add (minus m) %)
        r #(mult i %)
        f (comp t r t-inv)]
    [(f z) (f w)]))

(defn intersection
  "return the intersection of two lines"
  [l1 l2]
  (let [[a b c] (line-coords l1)
        [d e f] (line-coords l2)
        inv (v/mat-inverse [[a b] [d e]])
        result (v/mvmult inv [c f])]
    result))

(defn circumcircle
  "return the circumcircle of a a generalized circle,
  given by a vector of three non collinear complex numbers,
  returning a map with keys :center and :radius"
  [c]
  (let [[p1 p2 p3] c
        c (intersection (perp-bisector [p1 p2]) (perp-bisector [p2 p3]))
        r (n/distance p3 (n/c c))]
    [:circle {:center c :radius r}]))

;; inversion in a general circle C
;; where C has center P and radius r
;; is z ->

(defn inversion
  "inversion in a circle"
  ([] (fn [z] (recip (conjugate z))))
  ([circle]
   (let [{:keys [center radius]} circle
         T (inversion)
         Q (n/c center)
         r (n/c [radius 0])
         S (fn [z] (add (mult r z) Q))
         S-inv (fn [w] (mult (recip r) (add w (minus Q))))]
     (comp S T S-inv))))

(comment
  (require '[complex.geometry] :reload)
  (in-ns 'complex.geometry)
  (coords ((inversion) zero))
  ;;=> :infinity
  (coords ((inversion) infinity))
  ;;=> [0 0]
  )

;; parameterized circles
;; from deaux
(defn param-circle
  "returns a parameterized circle
  given four complex numbers a b c d
  (a*t + b)/(c*t + d) for parameter t
  where :infinity is an accepted parameter"
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
  "return a parameterized circle through the p q r
  for paramter values of 0 1 and :infinity, repectully"
  [p q r]
  (apply param-circle (three-point->param q p r)))

(defn param->general
  "return given parameterized circle in general form
  "
  [a b c d]
  (let [f (fn [u v] (sub (mult u (conjugate v)) (mult v (conjugate u))))
        g (fn [a b c d] (sub (mult a (conjugate d)) (mult b (conjugate c))))
        alpha-bar (minus (div (g a b c d) (f c d)))
        beta (minus (div (f a b) (f c d)))]
    [one (conjugate alpha-bar) alpha-bar beta]))

(defn param->standard
  [a b c d]
  (let [[_ alpha alpha-bar beta] (param->general a b c d)]
    [:circle {:center (coords (minus alpha-bar))
              :radius (length (sub (mult alpha alpha-bar) beta))}]))

(comment
  (def unit-circle [one i (minus one)])
  (apply param->standard (apply three-point->param unit-circle))
  [:circle {:center [0 0], :radius 1.0}]
  )

;; representation of circles and lines by hermitian matrices
;; from schwerdtfeger
(defn circle-as-matrix
  "return matrix representing circle with given center and radius"
  [center radius]
  (let [gamma (n/c center)
        A 1
        B (minus (conjugate gamma))
        C gamma
        D (- (n/len-sq gamma) (* radius radius))]
    [A B C D]))

(defn line-as-matrix
  "return matrix representing line
  through origin and perpendicular to complex number b"
  [b]
  (let [b-bar (conjugate b)]
    [0 b-bar b 0]))

;; methods that take a generalized circle
(defn to-string [[A B C D]]
  (pr-str [A (coords B) (coords C) D]))

(defn vaild-circle?
  "determine if given vector represents a hermitian matrix
need A and B to be real and B anc C complex conjugates"
  [[A B C D]]
  (assert (and (number? A) (number? D)))
  ;; (= B (conjugate C))
  true)

(defn to-circle
  "return center and radius for given hermitian matrix"
  [[A B C D]]
  (let [c (mult C (/ (- A)))
        c-coords (coords c)
        c-sq (n/len-sq c)
        r-sq (- c-sq (/ D A))
        r (if (< r-sq 0)
            (n/sqrt r-sq)
            (Math/sqrt r-sq))]
    [:circle {:center c-coords :radius r}]))

(defn line-coords-from-matrix
  "return line cords [a b c]
  for given matrix where ax + by = c"
  [[A B C D]]
  (assert (zero? A))
  (let [[u v] (coords B)
        a (* 2 u)
        b (* (- 2) v)
        c (- D)]
    [a b c]))

(defn line-coords-from-two-points
  "return line coords for line through two given points"
  [p1 p2]
  (let [[x1 y1] p1
        [x2 y2] p2
        a (- y2 y1)
        b (- x1 x2)
        c (- (* x1 y2) (* x2 y1))]
    [a b c]))

(defn line-coords
  "return line coords [a b c] for line ax + by = c
  for given line or for line btween two given points,
  as vectors or complex numbers"
  ([l]
   (match l
          [p1 p2]
          (if (vector? p1)
            (line-coords-from-two-points p1 p2)
            (line-coords-from-two-points (coords p1)
                                         (coords p2)))
          [:line p1 p2] (line-coords-from-two-points p1 p2)
          [A B C D] (line-coords-from-matrix l)))
  ([p1 p2]
   (if (vector? p1)
     (line-coords-from-two-points p1 p2)
     (line-coords-from-two-points (coords p1)
                                  (coords p2)))))

(defn param-line
  "the parameterized line between two complex numbers"
  [z w]
  (fn [t] (add (mult z (- 1 t)) (mult w t))))

(defn radial-line-through-point [point]
  (let [z (n/c point)]
    [zero z infinity]))

(defn circle-through-point [point]
  (let [z (n/c point)
        z1 (mult i z)
        z2 (minus z)
        z3 (mult (minus i) z)]
    [z1 z2 z3]))

(defn circle-about-point [point]
  (let [z (n/c point)
        z1 (add z one)
        z2 (sub z one)
        z3 (sub z i)]
    [z1 z2 z3]))

(defn horizontal-line-through-point
  [point]
  (let [[x y] point
        z (n/c point)
        z1 (add z one)]
    [z z1 infinity]))

(defn vertical-line-through-point
  [point]
  (let [[x y] point
        z (n/c point)
        z1 (add z i)]
    [z z1 infinity]))

(defn determinant
  "determinant of a hermitian matrix"
  [[A B C D]]
  (let [b (n/len-sq B)]
    (- (* A D) b)))

(comment
  (require 'complex.geometry :reload)
  (in-ns 'complex.geometry)
  (use 'clojure.repl)
  )

(comment
  (require '[complex.transform :as t])
  (let [T #(t/mult t/J %)
        l1 [zero one infinity]
        l2 [zero i infinity]
        c1 [one i (minus one)]
        tv #(mapv (comp coords T) %)]
    (mapv tv [l1 l2 c1]))
  ;;=> ["infinity" [1 0] [0 0]] ["infinity" [0 -1] [0 0]] [[1 0] [0 -1] [-1 0]]]

  (mapv coords (circle-through-point [1 1]))
  ;;=> [[-1 1] [-1 -1] [1 -1]]

  (mapv coords (circle-through-point [1 1]))
  (render (circle-through-point [1 1]))
  (mapv (comp n/rad->deg n/arg) (circle-through-point [1 1]))
  ;;=> [135.0 225.0 315.0]

  (let [point [0 0]
        c (circle-through-point point)
        l (radial-line-through-point point)
        T #(t/mult t/J %)
        tc (mapv T c)
        tl (mapv T l)
        rl [c l]
        c #(mapv coords %)
        f #(mapv T %)
        trl (mapv f rl)]
    [(mapv c rl)
     (mapv c trl)
     ;;(mapv render trl)
     ])

  (let [point [1 1]
        T #(t/mult t/J %)
        h-line (horizontal-line-through-point point)
        v-line (vertical-line-through-point point)
        rl [h-line v-line]
        c #(mapv coords %)
        f #(mapv T %)
        trl (mapv f rl)]
    [(mapv c rl)
     (mapv c trl)
     (mapv collinear? trl)])

  (let [point [1 1]
        T #(t/mult t/J %)
        h-line (horizontal-line-through-point point)
        v-line (vertical-line-through-point point)
        rl [h-line v-line]
        c #(mapv coords %)
        f #(mapv T %)
        trl (mapv f rl)]
    [(mapv render trl)])

  (let [point [0 0]
        T #(t/mult t/J %)
        h-line (horizontal-line-through-point point)
        v-line (vertical-line-through-point point)
        rl [h-line v-line]
        c #(mapv coords %)
        f #(mapv T %)
        trl (mapv f rl)]
    [(mapv c rl)
     (mapv c trl)
     (mapv collinear? trl)])

  (let [point [0 0]
        T #(t/mult t/J %)
        h-line (horizontal-line-through-point point)
        v-line (vertical-line-through-point point)
        rl [h-line v-line]
        c #(mapv coords %)
        f #(mapv T %)
        trl (mapv f rl)]
    (render h-line cs-3))

  ([:style {:stroke "green"}]
   [:line [0 0] [1 0]]
   [:line [1 0] [100000 0]]
   [:line [-99999 0] [0 0]]
   [:style {:stroke "grey", :fill "cyan"}]
   [:point [0 0]]
   [:style {:stroke "grey", :fill "magenta"}]
   [:point [1 0]])

  (let [point [-3.952 0.22399999999999975]
        T #(t/mult t/J %)
        h-line (horizontal-line-through-point point)
        v-line (vertical-line-through-point point)
        rl [h-line v-line]
        c #(mapv coords %)
        f #(mapv T %)
        trl (mapv f rl)]
    (render h-line cs-3))

  )
