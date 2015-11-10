(ns complex.vector)
;; vector and matrix operations
;; vector
;; sum product dot len-sq len angle scale-mul

(defn sum
  "sum two vectors"
  [z w] (mapv + z w))

(defn scal-mul
  "multiply a scalar and a vector"
  [t p]
  (let [[x y] p]
    [(* t x) (* t y)]))

(defn dot [[x1 y1] [x2 y2]] (+ (* x1 x2) (* y1 y2)))

(defn len-sq [z] (dot z z))

(defn len [z] (Math/sqrt (len-sq z)))

(defn angle [[x y]] (Math/atan2 y x))

(defn midpoint
  "return midpoint of given two points"
  [p1 p2]
  (scal-mul (/ 2) (sum p1 p2)))

(defn mvmult
  "matrix vector multipy"
  [[r1 r2] v]
  [(dot r1 v) (dot r2 v)])

(defn mmmult
  "multiply m, a 2x2 matrix by c
  where c is a vector"
  [m c]
  (let [c00 (get-in c [0 0])
        c10 (get-in c [1 0])
        c01 (get-in c [0 1])
        c11 (get-in c [1 1])
        c1 [c00 c10]
        c2 [c01 c11]
        [r1 r2] m]
    [[(dot r1 c1) (dot r1 c2)]
     [(dot r2 c1) (dot r2 c2)]]))

(defn mat-inverse
  "matrix inverse of a 2x2 real matrix"
  [[r1 r2]]
  (let [[a b] r1
        [c d] r2
        det (- (* a d) (* b c))
        res [(scal-mul (/ det) [d (- b)])
             (scal-mul (/ det) [(- c) a])]]
    res))

;; vector operations
(defn translation
  "returns function that translates vectors by given vector"
  ([w]
   (fn [v] (sum w v)))
  ([w n]
   (fn [v] (sum (scal-mul n  w) v))))

(defn scale
  ([s] (scale s s))
  ([sx sy] (fn [[x y]] [(* sx x) (* sy y)])))

(defn det-2d
  ""
  [[a1 a2] [b1 b2]]
  (- (* a1 b2) (* a2 b1)))

;; cross product
(defn cross-3d
  "return cross product of two 3d vactors "
  [[a1 a2 a3] [b1 b2 b3]]
  (let [c1 (det-2d [a2 a3] [b2 b3])
        c2 (det-2d [a1 a3] [b1 b3])
        c3 (det-2d [a1 a2] [b1 b2])]
    [c1 (- c2) c3]))
