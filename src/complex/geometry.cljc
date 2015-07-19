(ns complex.geometry
  (:require
   [complex.number :as n
    :refer [mult div add sub minus recip infinity zero one i coords arg length conjugate]]
   [complex.vector :as v]
   [schema.core :as s :include-macros true]
   #?(:clj
      [clojure.core.match :refer [match]]
      :cljs
      [cljs.core.match :refer-macros [match]])))

(def unit-circle [:circle {:center [0 0] :radius 1}])
(def real-axis [:line [0 0] [1 0]])
(def imaginary-axis [:line [0 0] [0 1]])
(def points [[:point [0 0]] [:point [1 0]] [:point [0 1]]])
(def lines [real-axis imaginary-axis unit-circle])
(def axis (interleave points lines))

(declare point-on-line?)
(declare line-coords)
(declare intersection)
(declare midpoint)
(defn collinear?
  "return true if given complex numbers are collinear"
  [l]
  (if (some #(= infinity %) l)
    true
    (let [[c1 c2 c3] l
          [p1 p2 p3] (mapv coords [c1 c2 c3])
          l (line-coords p1 p2)]
      (point-on-line? p3 l))))

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

(defn circumcircle [c]
  (assert (not (collinear? c)))
  (let [[p1 p2 p3] c
        c (intersection (perp-bisector [p1 p2]) (perp-bisector [p2 p3]))
        r (n/distance p3 (n/c c))
        cc [:circle {:center c :radius r}]]
    cc))

(defn concentric-circles
  "generate sequence of circles
  with given center and radii in range"
  [center start end step]
  (let [c (fn [r] [:circle {:center center :radius r}])]
    (for [r (range start end step)]
      (c r))))

(defn radial-lines
  "return n radial lines through orogin"
  [n]
  (let [l (fn [c1] [:line [0 0] (coords c1)])]
    (for [i (range n)]
      (let [angle (/ (* i 180) n)
            c1 (n/complex-polar angle)
            c2 (n/complex-polar 4 (+ 180 angle))]
        (l c1)))))

(defn horizontal-lines
  "horizontal lines"
  [step-size]
  (let [h-line (fn [i] [:line [-4 i] [4 i]])]
    (for [i (range -4 4 step-size)]
      (h-line i))))

(defn vertical-lines
  "horizontal lines"
  [step-size]
  (let [v-line (fn [j] [:line [j -4] [j 4]])]
    (for [j (range -4 4 step-size)]
      (v-line j))))

(defn rectangular-point
  [[x y]]
  [[:line [x 0] [x y]]
   [:line [0 y] [x y]]])

(defn polar-point
  [point]
  (let [radius (v/len point)]
    [[:line [0 0] point]
     [:circle {:radius radius :center [0 0]}]]))

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

(comment
  (defn param-line
  "given two endpoints return function
  of parameterized linem"
  [A B]
  (fn [t]
    (v/sum A (v/scal-mul t (v/sum B (v/scal-mul -1 A)))))))

(defn param-line
  "the parameterized line between two complex numbers"
  [z w]
  (fn [t] (add (mult z (- 1 t)) (mult w t))))

(defn p-style [k color-scheme]
  [:style {:stroke "grey" :fill (k color-scheme)}])

(defn l-style [k color-scheme]
  [:style {:stroke (color-scheme k)}])

(defn plus-infinity
  "return largest point on line within user space (r = 4)"
  [z1 z2]
  (let [l (param-line z1 z2)
        l-max (l 100000)
        len (length l-max)
        k (/ 6 len)]
    ;;(mult l-max k)
    l-max))

(defn line
  "line between first two complex numbers
  of given line where either one may be infinity"
  [z1 z2 z3]
  (let [[w1 w2]
        (cond
          (= infinity z1) [(plus-infinity z3 z2) z2]
          (= infinity z2) [z1 (plus-infinity z3 z1)]
          :else [z1 z2])]
    [:line (coords w1) (coords w2)]))

(defn render-line
  "render line l consisting of three collinear points
  any of which may be infinity"
  [l color-scheme]
  (let [[z1 z2 z3] l
        infinity? (some #(= infinity %) l)
        ;; if one of zi is infinity
        lines
        (if infinity?
          [(l-style :s1 color-scheme)
           (line z1 z2 z3)
           (line z2 z3 z1)
           (line z3 z1 z2)]
          [(l-style :s1 color-scheme)
           (line z1 z2 infinity)
           (line infinity z1 z2)
           (line z2 infinity z1)])
        ;; if none of z1 is infinity
        ;; then extend ...
        points (cond-> []
                 (not (= infinity z1))
                 (into [(p-style :p1 color-scheme)
                        [:point (coords z1)]])

                 (not (= infinity z2))
                 (into [(p-style :p2 color-scheme)
                        [:point (coords z2)]])

                 (not (= infinity z3))
                 (into [(p-style :p3 color-scheme)
                        [:point (coords z3)]]))]
    (concat lines points)))

(defn arc
  "arc between two complex numbers"
  [center radius start end clockwise]
  [:arc {:center center :radius radius
         :start start :end end
         :clockwise clockwise}])

(defn args
  "arguments of three complex numbers"
  [z1 z2 z3]
  [(arg z1) (arg z2) (arg z3)])

(defn arg-diffs [[z1 z2 z3]]
  [(arg (div z2 z1))
   (arg (div z3 z2))
   (arg (div z1 z3))])

(defn clockwise [c]
  (let [diffs (arg-diffs c)]
    (if (some #(> % (/ n/TAU 2)) diffs)
      true false)))

(comment
  ;; for render-circle arc segments
  ;; need to fix
  clockwise? (clockwise g-circle)

  arcs [(l-style :s1 color-scheme)
        (arc center radius a1 a2 clockwise?)

        (l-style :s2 color-scheme)
        (arc center radius a2 a3 clockwise?)

        (l-style :s3 color-scheme)
        (arc center radius a3 a1 clockwise?)]
  )

(defn render-circle
  "assumes g-circle is not a line"
  [g-circle color-scheme]
  (assert (not (collinear? g-circle)))
  (let [[z1 z2 z3] g-circle
        [a1 a2 a3] (args z1 z2 z3)

        [p1 p2 p3] (mapv coords g-circle)
        circle (circumcircle g-circle)
        {:keys [center radius]} (second circle)

        circ [(l-style :s1 color-scheme)
              circle]
        points [(p-style :p1 color-scheme)
                [:point p1]
                (p-style :p2 color-scheme)
                [:point p2]
                (p-style :p3 color-scheme)
                [:point p3]]]
    (concat circ points)))

(def color-scheme
  {:p1 "cyan"
   :p2 "magenta"
   :p3 "yellow"
   :s1 "red"
   :s2 "green"
   :s3 "blue"})


(def cs-1
  {:p1 "cyan"
   :p2 "magenta"
   :p3 "yellow"
   :s1 "blue"
   :s2 "green"
   :s3 "red"})

(def cs-2
  {:p1 "red"
   :p2 "purple"
   :p3 "green"
   :s1 "orange"
   :s2 "purple"
   :s3 "black"})

(def cs-3
  {:p1 "cyan"
   :p2 "magenta"
   :p3 "yellow"
   :s1 "green"})

(def cs-4
  {:p1 "red"
   :p2 "blue"
   :p3 "green"
   :s1 "purple"})

(def cs-5
  {:p1 "cyan"
   :p2 "yellow"
   :p3 "magenta"
   :s1 "orange"})

(defn render
  "transform generalized circle to
  a sequence of graphics primitives to be rendered"
  ([circle-or-line] (render circle-or-line color-scheme))
  ([circle-or-line color-scheme]
   (if (collinear? circle-or-line)
     (render-line circle-or-line color-scheme)
     (render-circle circle-or-line color-scheme))))

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

(defn point-on-line?
  "check if given point p is on line l
  where l is given by [a b c] with ax + by = c
  being the equation for the line"
  [p l]
  (let [[x y] p
        [a b c] l]
    (< (* (- c (+ (* a x) (* b y))) (- c (+ (* a x) (* b y)))) 10e-10)))

(defn intersection
  "return the intersection of two lines"
  [l1 l2]
  (let [[a b c] (line-coords l1)
        [d e f] (line-coords l2)
        inv (v/mat-inverse [[a b] [d e]])
        result (v/mvmult inv [c f])]
    result))

(comment
  (require 'complex.geometry :reload)
  (in-ns 'complex.geometry)

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
