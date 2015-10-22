(ns complex.roots
  "construtable numbers with exact precision"
  (:require [complex.number :as n]
            #?(:clj
               [clojure.core.match :refer [match]]
               :cljs
               [cljs.core.match :refer-macros [match]])))

;; a number is
;; a number
;; or
;; a vector
;; [:number num [:root base1 multiplier1] [:root base2 mult2] [:root base3]]
;; a :number tag followed by the a number and zero or more roots
;; or
;; a seq of
;; a list of numbers and roots
;; (list num-1 [:root base-1] [:root base-2 multiplier-2] num-2)
;; collect-terms will transform such sequences into a tagged :number vector

;; roots are :root tagged vectors [:root base & number] where
;; base is the number to take the square root of and
;; number is a optional multiplier, defaulting to 1

(def keywords [[:rect :polar]
               [:number :root]
               [:tau :degree :radian]
               [:zero :one :infinity]
               ;; for later:
               [:degree :minute :second]
               [:year :month :day :hour :minute :second]
               [:add :multiply :negative :reciprocal :conjugal :inversion]
               [:translate :dilate :negate :reciprocate :conjugate :invert]])

 (defn evaluate
  "evaluate number tree to a Double"
  [tree]
  (cond
    (number? tree) tree
    (seq? tree) (reduce + (map evaluate tree))
    (vector? tree) (match tree
                          [:root base & rest] (reduce * (Math/sqrt base) rest)
                          [:number n & rest]  (reduce + n (map evaluate rest))
                          [:tau f]
                          (evaluate [:radians (* f n/TAU)])
                          [:deg angle]
                          (evaluate [:radians (n/deg->rad angle)])
                          [:radians radians]
                          [(Math/cos radians) (Math/sin radians)]
                          [:polar radius angle]
                          (mapv #(* radius %) (evaluate angle))
                          [:rect x y] [x y]
                          [:reciprocal n] (/ 1 (evaluate n)))))

(defn root-number? [r]
  (assert (vector? r))
  (let [[tag base & rest] r]
    (assert (= :root tag))
    (assert (number? base))
    (assert (every? number? rest))
    true))

(defn my-zero? [n]
  (cond
    (number? n) (zero? n)
    (vector? n)
    (match n
           [:root b & mult] (or (zero? b) (zero? (reduce * mult)))
           [:rect x y] (and (zero? x) (zero? y))
           [:polar r a] (zero? r)
           [:number rational & roots]
           (reduce #(and %1 %2)
                   (zero? rational)
                   (map my-zero? roots)))))

(defn one? [n]
  (cond
    (number? n) (zero? (- 1 n))
    (vector? n)
    (match n
           [:tau a] (zero? a)
           [:root b & m] (and (zero? (- 1 b))
                              (zero? (- 1 (reduce * m))))
           [:polar r a] (and (zero? (- 1 r))
                             (one? a))
           [:number rational & roots]
           (or (and (one? rational)
                    (every? my-zero? roots))
               (== 1 (evaluate n)))
           [:rect x y]
           (and (zero? (- 1 x))(zero? y)))))

(defn factors [n]
  (reverse
   (sort
    (loop [j 1 res []]
      (if (> (* j j) n) res
          (recur (inc j) (if (zero? (rem n j))
                           (conj res (/ n j) j)
                           res)))))))

(def squares (map #(* % %)))

(defn less-than [n]
  (take-while #(<= % n)))

(defn squares-less-than [k]
  (into #{} (comp squares (less-than k)) (range)))

(defn largest-square-factor [n]
  (let [squares (squares-less-than n)
        factors (factors n)]
    (loop [factors factors]
      (let [f (first factors)]
        (if (squares f) f
            (recur (rest factors)))))))

(defn collect-roots
  "simplify sequence of numbers and roots
  collect common bases, collect numbers"
  [roots]
  (let [r-map
        (reduce
         (fn [result item]
           (cond
             (number? item) (update-in result [:number] #(+ % item))
             (vector? item)
             (match item
                    [:root base & mult]
                    (update-in result [:bases base] (fnil #(+ % (reduce * mult)) 0)))))
         {:number 0
          :bases {}}
         roots)]
    (into (vector :number (:number r-map))
          (for [[k v] (:bases r-map) :when (not= 0 v)] [:root k v]))))

(defn collect-terms
  "collect terms into a tagged :number vector"
  [num-seq]
  (cond (number? num-seq) (collect-terms (list num-seq))
        (vector? num-seq) (match num-seq
                                 [:root base & mult] [:number 0 num-seq]
                                 [:number n & roots] (collect-terms (concat (list n) roots)))
        (seq? num-seq)
        (loop [num 0 roots {} nums num-seq]
          (if-let [n (first nums)]
            (do
              (if (number? n)
                (recur (+ num n)
                       roots
                       (rest nums))
                (match n
                       [:root b & m]
                       (recur num
                              (update-in roots [b] (fnil #(+ % (reduce * m)) 0))
                              (rest nums))
                       [:number num1 & nroots]
                       (recur (+ num num1)
                              (reduce
                               (fn [result root]
                                 (match root [:root b & m]
                                        (update-in result [b]
                                                   (fnil #(+ % (reduce * m)) 0))))
                               roots
                               nroots)
                              (rest nums)))))
            (into [:number num] (for [[k v] roots :when (not= 0 v)]
                                  [:root k v]))))))

(defn mult-number
  "mult num2 by a regular number n"
  [n num2]
  (assert (number? n))
  (cond
    (number? num2) (* n num2)
    (vector? num2) (match num2
                          [:root base & mult]
                          [:root base (reduce * n mult)]
                          [:number q & roots]
                          (into [:number (* n q)]
                                (map #(mult-number n %) roots)))
    (seq? num2) (mult-number n (collect-terms num2))))

(defn reduce-base [base rest]
  (let [lsf (largest-square-factor base)]
    (cond
      (= lsf 1) [:root base rest]
      (= lsf base) (* (int (Math/sqrt base)) rest)
      :else [:root (/ base lsf) (* rest (int (Math/sqrt lsf)))])))

(defn mult-root
  "mult num2 by root r"
  [r num2]
  (assert (vector? r))
  (assert (= :root (first r)))
  (match r
         [:root base1 & rest1]
         (cond
           (number? num2) (mult-number num2 r)
           (vector? num2)
           (match num2
                  [:root base2 & rest2]
                  (reduce-base (* base1 base2)
                               (* (reduce * rest1) (reduce * rest2)))
                  [:number num & roots]
                  (let [mr #(mult-root r %)]
                    (collect-terms (concat (list (mr num)) (map mr roots)))))
           (seq? num2) (mult-root r (collect-terms num2)))))

(defn mult [num1 num2]
  (cond
    (number? num1) (mult-number num1 num2)
    (vector? num1) (match num1
                          [:root b & m] (mult-root num1 num2)
                          [:number n & roots]
                          (collect-terms
                           (concat (list (mult-number n num2))
                                   (map #(mult-root % num2) roots))))
    (seq? num1) (mult (collect-terms num1) num2)))

(defn add-root
  "add root r to number n"
  [r n]
  (match r
         [:root base & mult]
         (cond
          (number? n) [:number n r]
          (vector? n)
          (match n
                 [:number num & roots]
                 (collect-terms (conj n r))
                 [:root b2 & m2]
                 (if (== base b2)
                   [:number 0 [:root base (+ (reduce * mult)
                                             (reduce * m2))]]
                   [:number 1 r n]))
          (seq? n) (add-root r (collect-terms n)))))

(defn add
  "add two numbers"
  [num1 num2]
  (collect-terms (list num1 num2)))

(defn negative [v]
  (cond (number? v) (* -1 v)
        (vector? v)
        (match v
               [:root b & m] [:root b (reduce * -1 m)]
               [:tau frac] [:tau (n/mod-1 (+ (/ 2) frac))]
               [:rect x y] [:rect (negative x) (negative y)]
               [:number n & roots] (into [:number (negative n)] (map negative roots)))
        (seq? v) (negative (collect-terms v))))

(defn conjugate [v]
  (cond
    (number? v) v
    (vector? v)
    (match v
           [:rect x y] [:rect x (negative y)]
           [:tau frac] [:tau (n/mod-1 (- 1 frac))]
           [:root b & m] (negative v)
           [:number n root] [:number n (negative root)])
    (seq? v) (conjugate (collect-terms v))))

(defn reciprocal [num]
  (cond
    (number? num) (/ num)
    (seq? num) (reciprocal (collect-terms num))
    (vector? num)
    (match num
           [:number n & roots]
           (if (= 1 (count roots))
             (let [conj (conjugate num)
                   root (first roots)
                   [_ b & ms] root
                   m (reduce * ms)
                   k (/ (- (* n n) (* b m m)))]
               (mult-number k conj))
             [:reciprocal num])

           [:root base & mult]
           (reciprocal (collect-terms num)))))

(def alpha (list (/ 2) [:root 5 (/ 2)]))
(def beta  (list  (/ 2) [:root 5 (/ -2)]))

(def Phi alpha)
(def phi (negative beta))

(comment
  (require '[complex.roots] :reload)
  (in-ns 'complex.roots)

  (def t (list (/ 2) [:root 3] [:root 5 (/ 2)]))

  (= (evaluate t)
     (+ (/ 2) (Math/sqrt 3) (* (Math/sqrt 5) (/ 2))))

  (== 1 (evaluate (list alpha beta)))
  ;;=> true

  (mult-number (/ 3) (/ 2))
  (mult-number (/ 3) [:root 2])
  (mult-number (/ 3) [:root 2 1/3])
  (mult-number (/ 3) (list (/ 2)))
  (mult-number (/ 3) (list (/ 2) [:root 2] [:root 3 (/ 2)]))

  (< (Math/abs (- (evaluate (mult alpha beta))
                  (* (evaluate alpha) (evaluate beta))))
     1e-15)
  ;;=> true

  (< (Math/abs (- (evaluate (mult alpha beta))
                  (* (evaluate alpha) (evaluate beta))))
     1e-16)
  ;;=> false

  (* (evaluate alpha) (evaluate beta))
  ;;=> -1.0000000000000002

  (mult alpha beta)
  ;;=> [:number -1N]
  (evaluate (mult alpha beta))
  ;;=> -1N

  (mult (/ 4) alpha)
  ;;=> [:number 1/8 [:root 5 1/8]]
  )


(def plimpton
  [[119 169]
   [3367 4825]
   [4601 6649]
   [12709 18541]
   [65 97]
   [319 481]
   [2291 3541]
   [799 1249]
   [481 769]
   [4961 8161]
   [45 75]
   [1679 2929]
   [161 289]
   [1771 3229]
   [56 106]]
  )

(comment
  (for [[b c] plimpton]
    (let [b2 (* b b)
          c2 (* c c)]
      [c2 b2 (- c2 b2)]))

  (do
    (println "")
    (for [[b c] plimpton]
      (let [b2 (* b b)
            c2 (* c c)
            a2 (- c2 b2)
            a (Math/sqrt a2)
            d (double (/ c2 a2))]
        [d a b c])))

  (doseq [[b c] plimpton]
    (let [b2 (* b b)
          c2 (* c c)
          a2 (- c2 b2)
          a (Math/sqrt a2)]
      (prn [a b c])
      (assert (= c2 (+ a2 b2)))))
  [28561 14161 14400]
  [23280625 11336689 11943936]
  [44209201 21169201 23040000]
  [343768681 161518681 182250000]
  [9409 4225 5184]
  [231361 101761 129600]
  [12538681 5248681 7290000]
  [1560001 638401 921600]
  [591361 231361 360000]
  [66601921 24611521 41990400]
  [5625 2025 3600]
  [8579041 2819041 5760000]
  [83521 25921 57600]
  [10426441 3136441 7290000]
  [11236 3136 8100]
  )

(defn minus [[x1 y1] [x2 y2]]
  [(- x1 x2) (- y1 y2)])

(defn length-squared [[x y]]
  (+ (* x x) (* y y)))

(defn distance-squared [p1 p2]
  (let [[x1 y1] p1
        [x2 y2] p2]
    (length-squared (minus p2 p1))))

(defn distance-from-origin-squared [z]
  (match z
         [:polar r _] (* r r)
         [:tau _] 1
         [:rect x y] (+ (* x x) (* y y))))

(defn point-on-circle?
  [z]
  (= 1 (distance-from-origin-squared z)))

(comment
  (point-on-circle? omega)
  ;;=> true

  (let [p #(point-on-circle? %)
        d (concat square hexagon)]
    (reduce #(and %2 %1) (map p d)))

  (length-squared (evaluate [:tau (/ 3)]))
  ;;=> 0.9999999999999999
  )

(def p-circle
  (fn [t]
    (let [d (+ 1 (* t t))
          n1 (- 1 (* t t))]
      [(/ n1 d) (/ (* 2 t) d)])))

(comment
  (map (comp length-squared p-circle) (range 100))
  )
